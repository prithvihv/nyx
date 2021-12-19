buildXMonad:
	cd pkgs/xmonad-phv && \
	nix-build
	
update: 
	nix flake update
