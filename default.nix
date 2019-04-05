
{ nixpkgs   ? import ./nixpkgs.nix {}
, bootghc   ? "ghc863"
, version   ? "0.2"
, useClang  ? false  # use Clang for C compilation
, withLlvm  ? false
, withDocs  ? true
, withDwarf ? nixpkgs.stdenv.isLinux  # enable libdw unwinding support
, withNuma  ? nixpkgs.stdenv.isLinux
, mkFile    ? null
, cores     ? 16
}:

with nixpkgs;

let
    stdenv =
      if useClang
      then nixpkgs.clangStdenv
      else nixpkgs.stdenv;

    noTest = pkg: haskell.lib.dontCheck pkg;

    hspkgs = haskell.packages.${bootghc};

    ourtexlive = nixpkgs.texlive.combine {
      inherit (nixpkgs.texlive)
      scheme-small
      collection-xetex
      fncychap
      titlesec
      tabulary
      varwidth
      framed
      capt-of
      wrapfig
      needspace
      dejavu-otf
      helvetic;
    };
    # fonts = nixpkgs.makeFontsConf { fontDirectories = [ nixpkgs.dejavu_fonts ]; };
    docsPackages = if withDocs then [ python3Packages.sphinx ourtexlive ] else [];

    deps = [
      autoconf automake m4
      gmp.dev gmp.out glibcLocales
      ncurses.dev ncurses.out
      perl git file which python3
      (hspkgs.ghcWithPackages (ps: [ ps.alex ps.happy ]))
      xlibs.lndir  # for source distribution generation
      cabal-install
      zlib.out
      zlib.dev
      zlib
    ]
    ++ docsPackages
    ++ stdenv.lib.optional withLlvm llvm_6
    ++ stdenv.lib.optional withNuma numactl
    ++ stdenv.lib.optional withDwarf elfutils
    ++ stdenv.lib.optional (! stdenv.isDarwin) pxz 
    ++ [ pcre pcre2 zlib ]
    ++ stdenv.lib.optionals stdenv.isDarwin
    [ libiconv darwin.libobjc darwin.apple_sdk.frameworks.Foundation ];

in
stdenv.mkDerivation rec {
  name = "ghc-for-lambdabot-${version}";
  buildInputs = deps;
  libraryHaskellDepends = with haskellPackages; [
    zlib
    pcre
    pcre2
  ];
  hardeningDisable = [ "fortify" ];
  phases = ["nobuild"];
  postPatch = "patchShebangs .";

  # ${lib.optionalString withDocs "export FONTCONFIG_FILE=${fonts}"}

  enableParallelBuilding = true;
  NIX_BUILD_CORES = cores;
  stripDebugFlags = [ "-S" ];

  nobuild = ''
    echo Do not run this derivation with nix-build, it can only be used with nix-shell
  '';
}