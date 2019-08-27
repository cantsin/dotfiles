{ pkgs ? import <nixpkgs> { }, ... }:
let
  nixfmt = import (pkgs.fetchFromGitHub {
    owner = "serokell";
    repo = "nixfmt";
    rev = "740ad29db77ede6cf7b052ff8f170b0b1163d619";
    sha256 = "0ww96d5yyhl4akcwr9nbgq5kvm7j3q08ikwyqwf3ivil4a0wqc5k";
    # date = 2019-08-25T19:42:47+02:00;
  }) { };
  catt = pkgs.python37.pkgs.buildPythonPackage rec {
    pname = "catt";
    version = "0.9.2";

    src = pkgs.python37.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1qmydig8xy5wi0wqq5yirg18fcgz6jhjwhmyqkva8ahqvb0nphqs";
    };

    doCheck = false;
    propagatedBuildInputs = with pkgs.python37.pkgs; [
      PyChromecast
      youtube-dl
      click
    ];

    meta = {
      homepage = "https://github.com/skorokithakis/catt";
      description =
        "Cast All The Things allows you to send videos from many, many online sources to your Chromecast.";
    };
  };
in { home.packages = [ catt nixfmt ]; }
