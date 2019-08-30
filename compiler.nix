{
  nixpkgs ? (import ./nixpkgs.nix) {}
, version ? "ghc864"
}:
nixpkgs.haskell.packages.${version}
