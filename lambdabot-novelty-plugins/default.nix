{ compiler ? import ../compiler.nix {} }:
compiler.callPackage ./project.nix { 
    lambdabot-core = (import ../lambdabot-core { inherit compiler; });
}
