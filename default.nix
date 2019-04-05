
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

    deps =
      [ autoconf automake m4
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
      ++ stdenv.lib.optional (! stdenv.isDarwin) pxz ;

    env = buildEnv {
      name = "ghc-build-environment";
      paths = deps;
    };

    hie = (import (fetchFromGitHub {
      owner="domenkozar";
      repo="hie-nix";
      # 0.6.0.0
      rev="6794005";
      sha256="0pc90ns0xcsa6b630d8kkq5zg8yzszbgd7qmnylkqpa0l58zvnpn";
    }) { });

    hieWrapper = writeShellScriptBin "hieWrapper" ''
      ${hie.hie86}/bin/hie "$@"
    '';

    # redhatJava = vscode-utils.buildVscodeMarketplaceExtension {
    #   mktplcRef = {
    #     name = "java";
    #     publisher = "redhat";
    #     version = "0.38.0";
    #     sha256 = "1dhprs62vg4r75yv67ad2c78plqhnlsah1d0cavrdr4vcjkvcqw9";
    #   };
    #   fixupPhase = ''
    #       chmod 777 $out/share/vscode/extensions/redhat.java/server
    #   '';
    # };

    vscode = vscode-with-extensions.override {
      # When the extension is already available in the default extensions set.
      vscodeExtensions = with vscode-extensions;
      [
        bbenoist.Nix
      ]
      ++
      # Concise version from the vscode market place when not available in the default set.
      vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "theme-dracula-at-night";
          publisher = "bceskavich";
          version = "2.5.0";
          sha256 = "03wxaizmhsq8k1mlfz246g8p8kkd21h2ajqvvcqfc78978cwvx3p";
        }
        {
          name = "vsc-material-theme";
          publisher = "equinusocio";
          version = "2.7.0";
          sha256 = "1rs4ygsqh3hv2lg4kqj8vfkkvins2zfdqniyap3m1xr5kxfa96pn";
        }
        {
          name = "gitlens";
          publisher = "eamodio";
          version = "9.5.1";
          sha256 = "10s2g98wv8i0w6fr0pr5xyi8zmh229zn30jn1gg3m5szpaqi1v92";
        }
        {
          name = "vscode-hie-server";
          publisher = "alanz";
          version = "0.0.25";
          sha256 = "0m21w03v94qxm0i54ki5slh6rg7610zfxinfpngr0hfpgw2nnxvc";
        }
        {
          name = "language-haskell";
          publisher = "justusadam";
          version = "2.5.0";
          sha256 = "10jqj8qw5x6da9l8zhjbra3xcbrwb4cpwc3ygsy29mam5pd8g6b3";
        }
        {
          name = "brittany";
          publisher="maxgabriel";
          version="0.0.6";
          sha256= "1v47an1bad5ss4j5sajxia94r1r4yfyvbim5wax4scr0d5bdgv54";
        }
      ];
    };

    codeWrapper = runCommand "${vscode.name}" { nativeBuildInputs = [ makeWrapper ]; } ''
      mkdir -p $out/bin
      makeWrapper \
        ${vscode}/bin/code \
        $out/bin/code \
        --prefix PATH : ${lib.makeBinPath [ cabal-install ]}
    '';

in
stdenv.mkDerivation rec {
  name = "vscode-with-hie-${version}";
  buildInputs = 
    [ pcre pcre2 zlib ] ++
    [ env codeWrapper hieWrapper ] ++
    stdenv.lib.optionals stdenv.isDarwin
    [ libiconv darwin.libobjc darwin.apple_sdk.frameworks.Foundation ];
  hardeningDisable = [ "fortify" ];
  phases = ["nobuild"];
  postPatch = "patchShebangs .";
  preConfigure = ''
    echo Running preConfigure...
    echo ${version} > VERSION
    ((git log -1 --pretty=format:"%H") || echo dirty) > GIT_COMMIT_ID
    ./boot
  '' + stdenv.lib.optionalString (mkFile != null) ''
    cp ${mkFile} mk/build.mk
  '';
  # N.B. CC gets overridden by stdenv
  CC                  = "${stdenv.cc}/bin/cc"         ;
  CC_STAGE0           = CC                            ;
  CFLAGS              = "-I${env}/include"            ;
  CPPFLAGS            = "-I${env}/include"            ;
  LDFLAGS             = "-L${env}/lib"                ;
  LD_LIBRARY_PATH     = "${env}/lib"                  ;
  GMP_LIB_DIRS        = "${env}/lib"                  ;
  GMP_INCLUDE_DIRS    = "${env}/include"              ;
  CURSES_LIB_DIRS     = "${env}/lib"                  ;
  CURSES_INCLUDE_DIRS = "${env}/include"              ;
  configureFlags      = lib.concatStringsSep " "
    ( lib.optional withDwarf "--enable-dwarf-unwind" ) ;

  # ${lib.optionalString withDocs "export FONTCONFIG_FILE=${fonts}"}

  shellHook           = let toYesNo = b: if b then "YES" else "NO"; in ''
    # somehow, CC gets overriden so we set it again here.
    export CC=${stdenv.cc}/bin/cc

    # "nix-shell --pure" resets LANG to POSIX, this breaks "make TAGS".
    export LANG="en_US.UTF-8"

    echo Entering a GHC development shell with CFLAGS, CPPFLAGS, LDFLAGS and
    echo LD_LIBRARY_PATH correctly set, to be picked up by ./configure.
    echo
    echo "    CC              = $CC"
    echo "    CC_STAGE0       = $CC_STAGE0"
    echo "    CFLAGS          = $CFLAGS"
    echo "    CPPFLAGS        = $CPPFLAGS"
    echo "    LDFLAGS         = $LDFLAGS"
    echo "    LD_LIBRARY_PATH = ${env}/lib"
    echo "    LLVM            = ${toYesNo withLlvm}"
    echo "    libdw           = ${toYesNo withDwarf}"
    echo "    numa            = ${toYesNo withNuma}"
    echo "    configure flags = ${configureFlags}"
    echo
    echo Please report bugs, problems or contributions to
    echo https://github.com/alpmestan/ghc.nix

  '';
  enableParallelBuilding = true;
  NIX_BUILD_CORES = cores;
  stripDebugFlags = [ "-S" ];

  # Without this, we see a whole bunch of warnings about LANG, LC_ALL and locales in general.
  # In particular, this makes many tests fail because those warnings show up in test outputs too...
  # The solution is from: https://github.com/NixOS/nix/issues/318#issuecomment-52986702
  LOCALE_ARCHIVE = if stdenv.isLinux then "${glibcLocales}/lib/locale/locale-archive" else "";

  nobuild = ''
    echo Do not run this derivation with nix-build, it can only be used with nix-shell
  '';
}
