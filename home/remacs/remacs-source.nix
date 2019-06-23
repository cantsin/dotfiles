{ local ? true }:

with import <nixpkgs> {};

let
  lib = import <nixpkgs/lib>;
in

if local then
  if lib.pathExists ./remacs then ./remacs else abort "local remacs source not found"
else fetchFromGitHub {
  owner = "remacs";
  repo = "remacs";
  rev = "fb2e5d73bbd221568e9fbeaa2e6d169a422455de";
  sha256 = "07nr6k7iamv05lpv8zxkanyl6n8629vs5rsv0c6q4rxqyfq51i0w";
  # date = 2019-06-13T13:01:58-07:00;
}
