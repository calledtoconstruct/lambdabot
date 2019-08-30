{ compiler ? import ./compiler.nix {} }:
import ./lambdabot {
    inherit compiler;
}
