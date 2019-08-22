{
  nixpkgs ? (import ./nixpkgs.nix) {}
, version ? "ghc864"
}:
let compiler = nixpkgs.haskell.packages.${version}.ghcWithHoogle (ps: with ps; [
    base hint placeholders template-haskell
    zlib
    zlib.out
]);
in (compiler)
