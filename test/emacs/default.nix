with import <nixpkgs> { };

stdenv.mkDerivation rec {
  version = "27.0.1";

  name = "emacs-${version}";

  buildInputs = [
    ctags
    bear
    irony-server

    systemd
    texinfo
    libjpeg
    libtiff
    giflib
    xorg.libXpm
    gtk3
    gnutls
    ncurses
    libxml2
    xorg.libXt
    imagemagick
    librsvg
    gpm
    dbus
    libotf
    clang_6
    pkgconfig
    autoconf
    rustup
  ];

  shellHook = ''
    export LIBCLANG_PATH="${llvmPackages_6.libclang.lib}/lib";
    ctags -o emacs/TAGS -e -R emacs/src/*.c emacs/src/*.h
  '';
}
