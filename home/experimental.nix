{ pkgs ? import <nixpkgs> { }, ... }:
let
  nixfmt = import (pkgs.fetchFromGitHub {
    owner = "serokell";
    repo = "nixfmt";
    rev = "740ad29db77ede6cf7b052ff8f170b0b1163d619";
    sha256 = "0ww96d5yyhl4akcwr9nbgq5kvm7j3q08ikwyqwf3ivil4a0wqc5k";
    # date = 2019-08-25T19:42:47+02:00;
  }) { };
  catt = import ./catt.nix;
  afew = import ./afew.nix;

in { home.packages = [ nixfmt catt afew ]; }
