buildXMonad:
	cd pkgs/xmonad-phv && \
	nix-build
	
update: 
	nix flake update

editSopsSecret:
	cd ./sops && \
	sops secrets/nyx.yaml  