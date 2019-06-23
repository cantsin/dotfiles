{ local ? false }:

with import <nixpkgs> {};

let
  remacsSource = import ./remacs-source.nix { inherit local; };
  remacs = import ./remacs.nix { inherit remacsSource local; } ;
  customEmacs = dontRecurseIntoAttrs (emacsPackagesNgFor remacs);
  packages = import ../emacs-packages.nix { inherit pkgs; };
in

customEmacs.emacsWithPackages packages
