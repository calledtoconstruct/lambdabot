# Lambdabot: A friendly IRC bot and apprentice coder, written in Haskell.

[![Build Status](https://secure.travis-ci.org/lambdabot/lambdabot.svg)](http://travis-ci.org/lambdabot/lambdabot/)

Lambdabot is an IRC bot written over several years by those on the #haskell
IRC channel.

It operates as a command line tool, embedded in an editor, embedded in GHCi,
via internet relay chat and on the web.

# Prerequisites

cabal install network happy
sudo apt -y install zlib1g-dev
sudo apt -y install libpcre2-dev libpcre3-dev
sudo apt -y install libghc-curl-dev

# Create Template base on this project (Using nix)

In order to create a template for a new project base on this project 
you could run:

```
nix-build  -E 'with import <nixpkgs> {}; 
                     import "${fetchFromGitHub {
                            owner = "countoren";
                            repo = "lambdabot";
                            rev = "3c925a242f79f8d3dc47d779a6ca31e836837ece";
                            sha256 = "0yldd5s3l6pcimf1l6kdvd1g7l8lhcmylfv0q6fgazlpnca0z92v";
                        }}/nix/template.nix"' && ./result/bin/template-haskell;
```
