{ pkgs ? import ./nixpkgs.nix {}
, compiler ? import ./compiler.nix { inherit pkgs; }
}:
import ./lambdabot { inherit compiler;} #lambdabot executeable
