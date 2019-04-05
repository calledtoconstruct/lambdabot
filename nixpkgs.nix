
with import <nixpkgs> {};

import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # hie-nix 0.6.0.0 nixpkgs 18.09
    # rev = "95ca9c8b28311e8e4374da323b3d3341fa477ee6";
    # sha256 = "0na7cjqwl7srvxqkmwm7pgg48j5p3c129kqlsmqwk0q0kdqy5mq7";
    # hie-nix 0.6.0.0 nixpkgs 19.03-beta
    rev = "7f35ed9df40f12a79a242e6ea79b8a472cf74d42";
    sha256 = "1wr6dzy99rfx8s399zjjjcffppsbarxl2960wgb0xjzr7v65pikz";
})
