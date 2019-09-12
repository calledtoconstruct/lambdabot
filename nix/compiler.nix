{
  pkgs ? import ./nixpkgs.nix {}
, version ? "ghc865"
}:
pkgs.haskell.packages.${version}
