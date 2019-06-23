.PHONY: system home

system:
	sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos
	sudo cp system/configuration.nix /etc/nixos/
	if [ ! -f system/hostname.nix ]; then \
		echo "Hostname is required."; \
		exit 1; \
	fi
	sudo cp system/hostname.nix /etc/nixos/
	if [ -f home/secrets/default.nix ]; then \
		sudo cp home/secrets/system-*.nix /etc/nixos/; \
	fi
	sudo nixos-rebuild --upgrade switch

hostname:
	echo "{ hostName = \"${HOSTNAME}\"; }" > system/hostname.nix


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
	cp -r home/remacs ~/.config/nixpkgs/
	cp emacs.d/packages.nix ~/.config/nixpkgs/emacs-packages.nix
	if [ -f home/secrets/default.nix ]; then \
		mkdir -p ~/.config/nixpkgs/secrets; \
		cp home/secrets/default.nix ~/.config/nixpkgs/secrets; \
	fi
	home-manager switch

clean:
	nix-collect-garbage

all: system home
