with import <nixpkgs> { };
python37.pkgs.buildPythonApplication rec {
  pname = "afew";
  version = "2.0.0";

  src = python37.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "0j60501nm242idf2ig0h7p6wrg58n5v2p6zfym56v9pbvnbmns0s";
  };

  nativeBuildInputs = with python37.pkgs; [ sphinx setuptools_scm ];

  propagatedBuildInputs = with python37.pkgs; [
    setuptools
    notmuch
    chardet
    dkimpy
  ];

  makeWrapperArgs = [ ''--prefix PATH ':' "${notmuch}/bin"'' ];

  meta = with pkgs.lib; {
    homepage = "https://github.com/afewmail/afew";
    description = "An initial tagging script for notmuch mail";
    license = licenses.isc;
    maintainers = with maintainers; [ andir flokli ];
  };
}
