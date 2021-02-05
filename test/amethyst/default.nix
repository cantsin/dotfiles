with import <nixpkgs> { };

let
  src = fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "50bae918794d3c283aeb335b209efd71e75e3954";
    sha256 = "07b7hgq5awhddcii88y43d38lncqq9c8b2px4p93r5l7z0phv89d";
    # date = 2019-04-02T09:39:52+02:00;
  };
in with import "${src.out}/rust-overlay.nix" pkgs pkgs;
let
  rustWithSource = (latest.rustChannels.stable.rust.override {
    extensions = [ "rust-src" "clippy-preview" "rls-preview" "rust-analysis" ];
  });
in stdenv.mkDerivation rec {
  name = "env";

  buildInputs = [
    rustWithSource
    rustfmt
    rustracer
    ctags

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
    openssl
    gdb
    rr
    gcc
    openssl
    cmake
    python3
    gawk
    xorg.libxcb
    alsaLib
    freetype
    shaderc
    vulkan-loader
    udev

    libGL
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
  ];

  shellHook = ''
    export NIX_PATH="nixpkgs=${toString <nixpkgs>}"
    export LIBCLANG_PATH="${llvmPackages_6.libclang.lib}/lib";
  '';

  LD_LIBRARY_PATH = with pkgs.xorg;
    "${
      pkgs.lib.makeLibraryPath [ libGL libX11 libXcursor libXrandr libXi ]
    }:/run/opengl-driver/lib:${vulkan-loader}/lib";
  RUST_BACKTRACE = 1;
  RUST_SRC_PATH = "${rustWithSource}/lib/rustlib/src/rust/src";
}
