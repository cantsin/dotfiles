with import <nixpkgs> {};

let
  gems = bundlerEnv {
    name = "octobox-gems";
    ruby = ruby_2_6;
    gemdir  = ./.;
  };
in

stdenv.mkDerivation rec {
  pname = "octobox";
  version = "2019-05-08";

  src = fetchFromGitHub {
    owner = "octobox";
    repo = "octobox";
    fetchSubmodules = true;
    deepClone = true;
    branchName = "master";
    rev = "477cb7c002d0eba20f27031d6f40ab0dc9f1c625";
    sha256 = "1jsmrf9ljaa0klhcvd431w173lcqa62xb5zhazclizvz9x1kx99p";
  };

  buildInputs = [
    gems
    ruby_2_6
    ruby_2_6.devEnv
    postgresql
    zlib

    curl
    git
    libxml2
    libxslt
    bundix
  ];

  shellHook = ''
    mkdir -p .nix-gems
    export GEM_HOME=$PWD/.nix-gems
    export GEM_PATH=$GEM_HOME
    export PATH=$GEM_HOME/bin:$PATH
  '';

  installPhase = ''
    exec ${gems}/bin/bundle exec rake db:create db:migrate
  '';

  meta = with stdenv.lib; {
  description = "Untangle your GitHub Notifications";
    homepage = https://octobox.io;
    license = licenses.agpl3;
    platforms = [ "x86_64-linux" ];
  };
}
