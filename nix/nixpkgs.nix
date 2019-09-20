with import <nixpkgs> {};

import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # nixpkgs 18.09
    # rev = "95ca9c8b28311e8e4374da323b3d3341fa477ee6";
    # sha256 = "0na7cjqwl7srvxqkmwm7pgg48j5p3c129kqlsmqwk0q0kdqy5mq7";
    # nixpkgs 19.03
    # rev = "f52505fac8c82716872a616c501ad9eff188f97f";
    # sha256 = "0q2m2qhyga9yq29yz90ywgjbn9hdahs7i8wwlq7b55rdbyiwa5dy";
    # nixpkgs 19.09
    rev = "316a0e9";
    sha256 = "1vm4k373aqk11l0h99ixv7raw1mfkirhl7f7pr4kpiz7bpfavrwz";
    # nixpkgs 19.09 (Unstable)
    # rev = "54f385241e6649128ba963c10314942d73245479";
    # sha256 = "0bd4v8v4xcdbaiaa59yqprnc6dkb9jv12mb0h5xz7b51687ygh9l";
})
