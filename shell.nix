{
  nixpkgs   ? import ./nixpkgs.nix {}
, version   ? "ghc864"
, compiler  ? import ./compiler.nix { inherit nixpkgs version; }
, default   ? import ./default.nix { inherit nixpkgs version withDwarf cores; }
, hie       ? import ./hie.nix { inherit nixpkgs version compiler; }
, vscode    ? import ./vscode.nix { inherit nixpkgs version; }
, mkFile    ? null
, useClang  ? false  # use Clang for C compilation
, withLlvm  ? false
, withDocs  ? true
, withDwarf ? nixpkgs.stdenv.isLinux  # enable libdw unwinding support
, withNuma  ? nixpkgs.stdenv.isLinux
, cores     ? 16
}:

with default;

let
    env = buildEnv {
      name = "development-environment";
      paths = default.buildInputs ++ [ vscode.codeWrapper hie.hie nixpkgs.figlet zlib pcre pcre2 ];
    };


in
stdenv.mkDerivation rec {
  name = "vscode-with-hie-${version}";
  buildInputs = [ env ];
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

  shellHook           = let toYesNo = b: if b then "YES" else "NO"; in ''
    # somehow, CC gets overriden so we set it again here.
    export CC=${stdenv.cc}/bin/cc

    # "nix-shell --pure" resets LANG to POSIX, this breaks "make TAGS".
    export LANG="en_US.UTF-8"

    figlet 'Welcome to the Lambdabot Shell'

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