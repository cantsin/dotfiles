.PHONY: system home

system:
	sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
	sudo cp system/configuration.nix /etc/nixos/configuration.nix
	if [ -f home/secrets/default.nix ]; then \
		sudo cp home/secrets/system-*.nix /etc/nixos/; \
	fi
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
	rm ~/.config/nixpkgs/*.{nix,h,patch}
	cp home/*.{nix,h,patch} ~/.config/nixpkgs/
	if [ -f home/secrets/default.nix ]; then \
		mkdir -p ~/.config/nixpkgs/secrets; \
		cp home/secrets/default.nix ~/.config/nixpkgs/secrets; \
	fi
	home-manager switch

clean:
	nix-collect-garbage

all: system home
