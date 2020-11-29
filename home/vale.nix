with import <nixpkgs> { };
let
  writeGood = pkgs.fetchFromGitHub {
    owner = "errata-ai";
    repo = "write-good";
    rev = "2d116619b7662d9d59201e8808254e715fc83cc8";
    sha256 = "0nzs6wp7xsbcm481rwrijvpm5ks9z4hwcc2ydz16dfjxh1bywjq3";
    # date = 2020-05-04T13:16:50-07:00;
  };
  proseLint = pkgs.fetchFromGitHub {
    owner = "errata-ai";
    repo = "proselint";
    rev = "f9cc800a459bddebdac1ad517bc2bc4b5522153d";
    sha256 = "05bbbfi5x7ngh0pd74dgw4pkh436dy4g2i1fkpscn7yrvgp91r6c";
    # date = 2020-11-06T18:45:32-08:00;
  };
  valeStyles = stdenv.mkDerivation {
    name = "vale-styles";
    buildCommand = ''
      mkdir -p $out/styles
      cp -r ${writeGood}/write-good $out/styles
      cp -r ${proseLint}/proselint $out/styles
    '';
  };
in {
  home.packages = with pkgs; [ vale ];
  home.file = {
    ".vale.ini" = {
      text = ''
        StylesPath = ${valeStyles}/styles
        MinAlertLevel = suggestion

        [*.{org,md}]
        BasedOnStyles = proselint, write-good
        proselint.Annotations = NO
      '';
    };
  };
}
