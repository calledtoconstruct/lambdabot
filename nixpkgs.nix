# let
#     rev = "1222e289b5014d17884a8b1c99f220c5e3df0b14";
#     src = builtins.fetchGit {
#       url = "https://github.com/nixos/nixpkgs";
#       inherit rev;
#     };
# in
# import src

# with import <nixpkgs> {};

# import (fetchFromGitHub {
#   owner="NixOS";
#   repo="nixpkgs";
#   rev="61e59fb";
#   sha256="0lgfkbfpch9rdv6izi300s8s4xxnpbvwvfc04smdklcsr889yjjr";
# })

# rev = "083220867c71443b0473374e8abe871cecb8b7d9";
# sha256 = "0ycx7zc4hsy61y1hddxb771mxdkzwhsrsdim4rw0ghb8bha5jpkr";

with import <nixpkgs> {};

import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # hie-nix ?.?.?.?
    # rev = "083220867c71443b0473374e8abe871cecb8b7d9";
    # sha256 = "1136wl9r6h15b4vg776zl89ng41kwq8knakqdz8lmbih2qkrpdf4";
    # hie-nix 0.6.0.0
    rev = "95ca9c8b28311e8e4374da323b3d3341fa477ee6";
    sha256 = "0na7cjqwl7srvxqkmwm7pgg48j5p3c129kqlsmqwk0q0kdqy5mq7";
    # sha256 = "11g74b3y93bikm8dqzpr1wpc96qvwl44kbsnspv5684qpi9kj405";
})
