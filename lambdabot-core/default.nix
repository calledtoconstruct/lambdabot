{ pkgs ? import <nixpkgs> { }, compiler ? "ghc864" }:
pkgs.haskell.packages.${compiler}.callPackage ./project.nix { }
