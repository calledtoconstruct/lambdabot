
with import <nixpkgs> {};

import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # hie-nix 0.6.0.0 nixpkgs 18.09
    # rev = "95ca9c8b28311e8e4374da323b3d3341fa477ee6";
    # sha256 = "0na7cjqwl7srvxqkmwm7pgg48j5p3c129kqlsmqwk0q0kdqy5mq7";
    # hie-nix 0.6.0.0 nixpkgs 19.03-beta
    # rev = "7f35ed9df40f12a79a242e6ea79b8a472cf74d42";
    # sha256 = "1wr6dzy99rfx8s399zjjjcffppsbarxl2960wgb0xjzr7v65pikz";
    # hie-nix 0.6.0.0 nixpkgs 19.03-beta :: includes vscode 1.33
    # rev = "82b8ff405bd151b682b9f5dfb14c672cb2b93866";
    # sha256 = "08j7rdnamlqzs22w4cgxyrjhphwazv3smr6crk2iqb5q1v92q0in";
    # nixpkgs 19.03
    # rev = "f52505fac8c82716872a616c501ad9eff188f97f";
    # sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
    # nixpkgs 19.09
    rev = "316a0e9";
    sha256 = "1vm4k373aqk11l0h99ixv7raw1mfkirhl7f7pr4kpiz7bpfavrwz";
})
