.PHONY: system home

system:
	sudo nix-channel --add https://nixos.org/channels/nixos-19.09 nixos
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
	rm -f ~/.config/nixpkgs/*.{nix,h,patch}
	cp home/*.nix ~/.config/nixpkgs/
	mkdir -p ~/.config/nixpkgs/st
	cp home/st/*.{h,patch} ~/.config/nixpkgs/st
	mkdir -p ~/.config/nixpkgs/org-task
	cp home/org-task/*.{sh,el} ~/.config/nixpkgs/org-task
	cp test/{catt,afew}.nix ~/.config/nixpkgs/
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
