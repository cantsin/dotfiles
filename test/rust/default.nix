with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    cmake
    ctags
    cargo
    gdb
    libffi
    patchelf
    ps
    python2
    removeReferencesTo
    rustc
    rustup
    which
  ];
}
