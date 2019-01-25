.PHONY: system home

system:
	sudo cp system/configuration.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild --upgrade switch

init-home:
	nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
	nix-channel --update

home:
	cp home/*.nix ~/.config/nixpkgs/
	home-manager switch

clean:
	nix-collect-garbage

emacs:
	echo "not implemented"

all: system home
