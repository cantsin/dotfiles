all:
	nixops create ./nas.nix ./nas-vbox.nix -d nas

deploy:
	nixops deploy -d nas --allow-reboot

clean:
	nixops destroy -d nas
	nixops delete -d nas
