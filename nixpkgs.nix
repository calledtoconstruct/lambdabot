
with import <nixpkgs> {};

import (fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    # nixpkgs 19.09
    rev = "54f385241e6649128ba963c10314942d73245479";
    sha256 = "0bd4v8v4xcdbaiaa59yqprnc6dkb9jv12mb0h5xz7b51687ygh9l";
})
