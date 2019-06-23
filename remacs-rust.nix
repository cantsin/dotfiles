{ remacsSource ? ./remacs }:

with import <nixpkgs> {};

let
  fetchcargo = import <nixpkgs/pkgs/build-support/rust/fetchcargo.nix> {
    inherit (pkgs) stdenv cacert git cargo cargo-vendor python3;
  };
in

# we want to version cargo vendored sources (via -x or
# --explicit-version) since remacs uses different versions of the same
# library. we hack this into the custom cargoUpdateHook.
let doVersionedUpdate = ''
  mkdir -p $out/versioned
  cargo vendor -x $out/versioned
'';
in

let remacsRustBindings = fetchcargo rec {
  name = "remacsRustBindings";
  sourceRoot = null;
  srcs = null;
  src = "${remacsSource}/rust_src/remacs-bindings";
  cargoUpdateHook = doVersionedUpdate;
  patches = [];
  sha256 = "0wgyzy73hlxzb375nmi1vpwinx9hwrgczl6jl45wlp9653920845";
};
in

let remacsRustSrc = fetchcargo rec {
  name = "remacsRustSrc";
  sourceRoot = null;
  srcs = null;
  src = "${remacsSource}/rust_src";
  cargoUpdateHook = ''
    sed -e 's/@CARGO_.*@//' Cargo.toml.in > Cargo.toml
  '' + doVersionedUpdate;
  patches = [];
  sha256 = "10w1y5f5pzaq24cd0r2ksx72mi16za8gakzla713ca86y0fwxd6j";
};
in

let remacsHashdir = fetchcargo rec {
  name = "remacsHashdir";
  sourceRoot = null;
  srcs = null;
  src = "${remacsSource}/lib-src/hashdir";
  cargoUpdateHook = doVersionedUpdate;
  patches = [];
  sha256 = "0054hrqc3yab0y11mmv1r97fngcpp9w191049ivawwmmg9yp8xri";
};
in

stdenv.mkDerivation {
  name = "remacsRust";
  srcs = [ remacsRustBindings remacsHashdir remacsRustSrc ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p $out/.cargo/registry
    cat > $out/.cargo/config << EOF
      [source.crates-io]
      registry = "https://github.com/rust-lang/crates.io-index"
      replace-with = "vendored-sources"
      [source.vendored-sources]
      directory = "$out/.cargo/registry"
    EOF
    cp -R remacsRustBindings-vendor/versioned/* $out/.cargo/registry
    cp -R remacsHashdir-vendor/versioned/* $out/.cargo/registry
    cp -R remacsRustSrc-vendor/versioned/* $out/.cargo/registry
  '';
}
