{ compiler ? import ../compiler.nix {} }:
compiler.callPackage ./project.nix { }
