{ compiler ? import ../compiler.nix {}
, haskell-src-exts-simple ?  import (pkgs.fetchFromGitHub {
                   owner="countoren";
                   repo="haskell-src-exts-simple";
                   rev="cfe44a9c88831357479676a48202ed213f8e9898";
                   sha256="0xldrsq1h2zblk3ckd0a1n9lkx62c0ryyyisza4pmsp5jn6bddpy";
  }) { inherit compiler; }#version 1.20.0
, hoogle ? import (pkgs.fetchFromGitHub {
                 owner="countoren";
                 repo="hoogle";
                 rev="0badca69acb9a266f3d0c3dd3c94b4f36cac3316";
                 sha256="1bfwp1iak66ds05kixb4rvx30hmafgckp99g8i3mbzwykr5d77bp";
  }) { inherit compiler; }#version 5.0.17.3
}:
compiler.callPackage ./project.nix {
    inherit haskell-src-exts-simple hoogle;
    lambdabot-core = (import ../lambdabot-core { inherit compiler; });
    lambdabot-reference-plugins = (import ../lambdabot-reference-plugins { inherit compiler;}); 
    lambdabot-trusted-plugins = (import ../lambdabot-trusted-plugins { inherit compiler; });
}
