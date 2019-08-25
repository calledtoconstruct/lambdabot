{ pkgs ? import <nixpkgs> {} , compiler ? "ghc865" }:
pkgs.pkgs.haskell.packages.${compiler}.callPackage ./project.nix {
    lambdabot-core = (import ../lambdabot-core { inherit compiler; });
    lambdabot-hangman-plugins = (import ../lambdabot-hangman-plugins { inherit compiler; });
    lambdabot-irc-plugins  = (import ../lambdabot-irc-plugins { inherit compiler; });
    lambdabot-misc-plugins = (import ../lambdabot-misc-plugins { inherit compiler; });
    lambdabot-novelty-plugins  = (import ../lambdabot-novelty-plugins { inherit compiler; });
    lambdabot-points-plugins = (import ../lambdabot-points-plugins { inherit compiler; });
    lambdabot-reference-plugins  = (import ../lambdabot-reference-plugins { inherit compiler; });
    lambdabot-social-plugins = (import ../lambdabot-social-plugins { inherit compiler; });
    lambdabot-suggest-plugins  = (import ../lambdabot-suggest-plugins { inherit compiler; });
    lambdabot-twitch-plugins  = (import ../lambdabot-twitch-plugins { inherit compiler; });
}
