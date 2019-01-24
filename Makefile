.PHONY: system home

init:
	nix-channel --add https://github.com/rycee/home-manager/archive/master.tar.gz home-manager
	nix-channel --update

system:
	sudo cp system/configuration.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild --upgrade switch

home:
	home-manager switch

all: system home
