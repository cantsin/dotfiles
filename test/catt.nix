with import <nixpkgs> { };
(let
  catt = python37.pkgs.buildPythonPackage rec {
    pname = "catt";
    version = "0.9.2";

    src = python37.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1qmydig8xy5wi0wqq5yirg18fcgz6jhjwhmyqkva8ahqvb0nphqs";
    };

    doCheck = false;
    propagatedBuildInputs = with python37.pkgs; [
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
in python37.withPackages (ps: [ catt ]))
