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
                            rev = "b63aa61";
                            sha256 = "05z7i9hlhl4brlrhwn85jgrw1zd8ya83klqkp13170fd3cnwjhy9";
                        }}/nix/template.nix"' && ./result/bin/template-haskell;
```
