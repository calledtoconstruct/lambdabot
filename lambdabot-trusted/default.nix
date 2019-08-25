{ pkgs ? import <nixpkgs> { }, compiler ? "ghc865" }:
pkgs.haskell.packages.${compiler}.callPackage ./project.nix { }
