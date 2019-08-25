{ pkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:
pkgs.haskell.packages.${compiler}.callPackage ./project.nix { 
  lambdabot-core = (import ../lambdabot-core { inherit compiler; });
}
