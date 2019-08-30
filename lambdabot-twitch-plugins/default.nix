{ pkgs ? import ../nixpkgs.nix {} , compiler ? "ghc865" }:
compiler.callPackage ./project.nix {
    lambdabot-core = (import ../lambdabot-core { inherit compiler; });
}
