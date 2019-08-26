{ mkDerivation, base, dependent-sum, lambdabot-core
, lambdabot-hangman-plugins, lambdabot-irc-plugins
, lambdabot-misc-plugins, lambdabot-novelty-plugins
, lambdabot-points-plugins, lambdabot-reference-plugins
, lambdabot-social-plugins, lambdabot-suggest-plugins
, lambdabot-twitch-plugins, mtl, stdenv
}:
mkDerivation {
  pname = "lambdabot";
  version = "5.1.0.4";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    base dependent-sum lambdabot-core lambdabot-hangman-plugins
    lambdabot-irc-plugins lambdabot-misc-plugins
    lambdabot-novelty-plugins lambdabot-points-plugins
    lambdabot-reference-plugins lambdabot-social-plugins
    lambdabot-suggest-plugins lambdabot-twitch-plugins mtl
  ];
  homepage = "https://wiki.haskell.org/Lambdabot";
  description = "Lambdabot is a development tool and advanced IRC bot";
  license = "GPL";
}
