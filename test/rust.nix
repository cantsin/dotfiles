with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "env";
  buildInputs = [ rustc cargo ];
}
