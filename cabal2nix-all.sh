cd ./lambdabot-core
cabal2nix . > project.nix
cd ../lambdabot-trusted-plugins
cabal2nix . > project.nix
cd ../lambdabot-hangman-plugins
cabal2nix . > project.nix
cd ../lambdabot-points-plugins
cabal2nix . > project.nix
cd ../lambdabot-suggest-plugins
cabal2nix . > project.nix
cd ../lambdabot-social-plugins
cabal2nix . > project.nix
cd ../lambdabot-reference-plugins
cabal2nix . > project.nix
cd ../lambdabot-novelty-plugins
cabal2nix . > project.nix
cd ../lambdabot-misc-plugins
cabal2nix . > project.nix
cd ../lambdabot-irc-plugins
cabal2nix . > project.nix
cd ../lambdabot-twitch-plugins
cabal2nix . > project.nix
cd ../lambdabot-haskell-plugins
cabal2nix . > project.nix
cd ../lambdabot
cabal2nix . > project.nix
cd ..
