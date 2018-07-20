{ pkgs ? import <nixpkgs> {}
}:

with pkgs;
{
  micro-ci = (haskellPackages.override (import ./haskell-overrides.nix pkgs)).callPackage ./default.nix {};
}
