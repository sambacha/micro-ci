{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Paths_micro_ci
import qualified Config
import Config (Config)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Exception (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON(..), fromJSON, Value(..), Object, withArray, eitherDecodeStrict)
import qualified Data.Aeson.Types as A
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Dhall
import GitHub.Data
import GitHub.Endpoints.Repos.Status
import Network.Wai.Handler.Warp as Warp
import Servant
import Servant.GitHub.Webhook
import Servant.Server
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import qualified System.Process as Process


-- Job

data Job = Job
  { jobRepoOwner :: Text.Text
  , jobRepoProject :: Text.Text
  , jobCommit :: Text.Text
  , jobAttr :: AttrPath
  }

doJob :: Config -> Job -> IO ()
doJob config job = do
  ensureRepository config (jobRepoOwner job) (jobRepoProject job)

  checkoutRef config (jobRepoOwner job) (jobRepoProject job) (jobCommit job)

  buildRes <-
    buildAttribute config (jobRepoOwner job) (jobRepoProject job) (jobAttr job)

  createStatus
    (OAuth (fromString $ LT.unpack $ Config.oauth config))
    (mkName (Proxy @Owner)  (jobRepoOwner job))
    (mkName (Proxy @Repo)   (jobRepoProject job))
    (mkName (Proxy @Commit) (jobCommit job))
    NewStatus { newStatusState =
                  if buildSuccess buildRes then
                    Success
                  else
                    Failure
              , newStatusTargetUrl =
                  Just $ URL $ Text.pack $
                  LT.unpack (Config.httpRoot config) ++ "/" ++ takeFileName (buildDerivation buildRes)
              , newStatusDescription =
                  Just $
                  if buildSuccess buildRes then
                    "nix-build successful"
                  else
                    "nix-build failed"
              , newStatusContext =
                  "ci.nix: " <> fromString (encodeAttrPath (jobAttr job))
              }

  return ()


-- buildAttribute

data BuildResult = BuildResult
  { buildSuccess :: Bool
  , buildDerivation :: String
  }


buildAttribute :: Config -> Text.Text -> Text.Text -> AttrPath -> IO BuildResult
buildAttribute config owner project path = do
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config owner project
         (Process.proc "nix-instantiate"
            ["ci.nix"
            , "-A"
            , encodeAttrPath path
            ]))
      ""

  drv <-
    case lines stdout of
      [drv] ->
        return drv

      _ ->
        fail $ unlines $
        [ "Could not find .drv from nix-instantiate ci.nix:"
        , ""
        , stdout
        , stderr
        ]

  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config owner project
         (Process.proc "nix-store" [ "--realise", drv ]))
      ""

  createDirectoryIfMissing
    True
    (LT.unpack $ Config.logs config)

  writeFile (LT.unpack (Config.logs config) </> takeFileName drv <.> "stdout") stdout

  writeFile (LT.unpack (Config.logs config) </> takeFileName drv <.> "stderr") stderr

  return BuildResult
    { buildSuccess =
        case exitCode of
          ExitSuccess ->
            True

          ExitFailure{} ->
            False
    , buildDerivation = drv
    }



-- findJobAttrPaths


findJobAttrPaths :: Config -> Text.Text -> Text.Text -> IO [AttrPath]
findJobAttrPaths config owner project = do
  jobsNixPath <-
    getDataFileName "jobs.nix"

  (exitCode, jobs, stderr) <-
    readCreateProcessWithExitCode
      (inGitRepository config owner project
         (Process.proc
            "nix-instantiate"
            [ "--eval"
            , "--strict"
            , "--json"
            , "-E"
            , "import " ++ jobsNixPath ++ " (import ./ci.nix)"
            ]))
      ""

  unless (exitCode == ExitSuccess) $
    fail $ unlines
      [ "Could not identify jobs in ci.nix:"
      , ""
      , stderr
      ]

  case eitherDecodeStrict (fromString jobs) of
    Left e ->
      fail $ unlines $
        [ "Could not parse jobs JSON"
        , ""
        , "Aeson reported:"
        , ""
        , show e
        , ""
        , "I'm trying to parse:"
        , ""
        , jobs
        ]

    Right paths ->
      return paths



-- repoDir


repoDir :: Config -> Text.Text -> Text.Text -> FilePath
repoDir config owner project =
  LT.unpack (Config.repoRoot config)
    </> Text.unpack owner
    </> Text.unpack project



-- ensureRepository

-- | Ensure that a repository has been cloned into the repoRoot directory, and that
-- it is up-to-date.

ensureRepository :: Config -> Text.Text -> Text.Text -> IO ()
ensureRepository cfg owner project = do
  let
    dir =
      repoDir cfg owner project

  exists <-
    doesDirectoryExist dir

  if exists then
    void $
      readCreateProcess
        (inGitRepository cfg owner project
           (proc "git" [ "fetch" ]))
        ""
  else do
    let cloneUrl = "https://github.com/" <> owner <> "/" <> project <> ".git"
    void $ readCreateProcess (proc "git" [ "clone", Text.unpack cloneUrl, dir ]) ""



checkoutRef :: Config -> Text.Text -> Text.Text -> Text.Text -> IO ()
checkoutRef config owner project ref =
  void $
    readCreateProcess
      (inGitRepository config owner project
         (proc "git" [ "checkout", Text.unpack ref ]))
      ""


-- inGitRepository

inGitRepository :: Config -> Text.Text -> Text.Text -> CreateProcess -> CreateProcess
inGitRepository config owner project a =
  a { cwd = Just (repoDir config owner project) }



-- HttpApi


type HttpApi =
  "github"
    :> "web-hook"
    :> GitHubEvent '[ 'WebhookPingEvent
                    , 'WebhookPushEvent
                    , 'WebhookCreateEvent
                    , 'WebhookPullRequestEvent
                    ]
    :> GitHubSignedReqBody '[JSON] Object
    :> Post '[JSON] ()
  :<|>
  Capture "drv" Text.Text
    :> Get '[PlainText] Text.Text


httpEndpoints :: TQueue (Text.Text, Text.Text, Text.Text) -> Config -> Server HttpApi
httpEndpoints q config =
  gitHubWebHookHandler q :<|> detailsHandler config



-- detailsHandler


detailsHandler :: Config-> Text.Text -> Handler Text.Text
detailsHandler config drvName = do
  stdout <-
    liftIO
      $ readFile (LT.unpack (Config.logs config) </> Text.unpack drvName <.> "stdout")

  stderr <-
    liftIO
      $ readFile (LT.unpack (Config.logs config) </> Text.unpack drvName <.> "stderr")

  return (Text.pack $ unlines [ stdout, "", stderr ])



-- AttrPath


-- | An attribute path.
newtype AttrPath =
  AttrPath [String]
  deriving (FromJSON)


encodeAttrPath :: AttrPath -> String
encodeAttrPath (AttrPath parts) =
  intercalate "." parts


-- gitHubWebHookHandler


-- | Top-level GitHub web hook handler. Ensures that a build is scheduled.

gitHubWebHookHandler :: TQueue (Text.Text, Text.Text, Text.Text) -> RepoWebhookEvent -> ((), Object) -> Handler ()
gitHubWebHookHandler queue WebhookPullRequestEvent ((), obj)
  | A.Success ev <- fromJSON (Object obj) = do
      let prCommit = pullRequestHead (pullRequestEventPullRequest ev)
          sha = pullRequestCommitSha prCommit
          repo = pullRequestCommitRepo prCommit
      liftIO $ atomically $
        writeTQueue queue
          ( untagName $ simpleOwnerLogin (repoOwner repo)
          , untagName $ repoName repo
          , sha
          )
gitHubWebHookHandler queue WebhookPushEvent ((), obj) = do
   let mcommit = A.parse (flip A.parseField "head_commit" >=> flip A.parseField "id" . asObj) obj
       mrepo = A.parse (flip A.parseField "repository" >=> parseRepo) obj
   case (,) <$> mcommit <*> mrepo of
     A.Error str                 -> liftIO . putStrLn $ "Error: " ++ str
     A.Success (commitSha, (owner, project)) -> liftIO . atomically $
       writeTQueue queue (owner, project, commitSha)

   where asObj obj = obj :: Object
         parseRepo o = (,) <$> (flip A.parseField "owner" >=> flip A.parseField "name" . asObj) o
                           <*> flip A.parseField "name" o
gitHubWebHookHandler _ _ _ = return ()



-- processCommit


processCommit :: Config -> TQueue Job -> (Text.Text, Text.Text, Text.Text) ->  IO ()
processCommit config jobQueue (owner, project, commitsha) = do
  ensureRepository config owner project

  checkoutRef config owner project commitsha

  paths <- findJobAttrPaths config owner project

  for_ paths $ \path ->
    atomically $
    writeTQueue
      jobQueue
      Job
        { jobRepoOwner = owner
        , jobRepoProject = project
        , jobCommit = commitsha
        , jobAttr = path
        }



-- main


main :: IO ()
main = do
  config <-
    LT.readFile "config.dhall"
      >>= Dhall.input Dhall.auto

  jobQueue <-
    newTQueueIO

  prQueue <-
    newTQueueIO

  forkIO $ forever $ do
    job <-
      atomically $ fmap Left (readTQueue prQueue) <|> fmap Right (readTQueue jobQueue)

    --try $
    case job of
        Left commitData ->
          processCommit config jobQueue commitData

        Right job ->
          doJob config job

  Warp.run 8080
    (serveWithContext
       (Proxy @HttpApi)
       (gitHubKey (return (fromString $ LT.unpack $ Config.secret config)) :. EmptyContext)
       (httpEndpoints prQueue config))
