.PHONY: system home

system:
	sudo cp system/configuration.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild --upgrade switch

init:
	nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
	nix-channel --update
	echo "Now log out and run 'make init-home-manager'"

init-home-manager:
	nix-shell '<home-manager>' -A install
	git submodule init
	git submodule update

home:
	cp home/*.{nix,h,patch} ~/.config/nixpkgs/
	home-manager switch

clean:
	nix-collect-garbage

all: system home
