with import <nixpkgs> { };
let
  manyLinuxFile = writeTextDir "_manylinux.py" ''
    print("in _manylinux.py")
    manylinux1_compatible = True
  '';
in stdenv.mkDerivation {
  name = "setup-pipenv";
  buildInputs = with pkgs; [
    python37Full
    python37Packages.ipython
    pipenv
    which
    gcc
    binutils

    # extra configuration for django
    postgresql
    sass

    # All the C libraries that a manylinux_1 wheel might depend on:
    ncurses
    xorg.libX11
    xorg.libXext
    xorg.libXrender
    xorg.libICE
    xorg.libSM
    glib
  ];

  runScript = "$SHELL";

  LD_LIBRARY_PATH = "${stdenv.cc.cc.lib}/lib64:$LD_LIBRARY_PATH";

  shellHook = ''
    export PYTHONPATH=${manyLinuxFile.out}:/usr/lib/python3.7/site-packages
  '';
}
