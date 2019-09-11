{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson
import           Data.Text
import           GHC.Generics    (Generic)
import           System.FilePath

data Config = Config
  { repoRoot :: !Text
  , secret   :: !Text
  , oauth    :: !Text
  , logs     :: !Text
  , httpRoot :: !Text
  } deriving (Generic)

instance FromJSON Config
