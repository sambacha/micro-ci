with (import <nixpkgs> {});
{
  micro-ci = haskellPackages.callPackage ./default.nix {};
}
