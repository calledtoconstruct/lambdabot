{
  pkgs ? import ./nixpkgs.nix {}
, version ? "ghc864"
}:
pkgs.haskell.packages.${version}
