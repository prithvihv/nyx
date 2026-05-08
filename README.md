## Issues:
- [ ] after unlocking from lock screen / hibernation keyboard stops working. Only Mouse is working.
- [ ] when to auto hibernate? (for work-station atleast)
- [ ] tmux resurrect continuum not restoring automatically 
- [X] GC for nix
- [X] i3 multi lock screen
- [ ] toggling notifications


## Docs management linux
- use convert to convert images into pdfs
- pandoc to convert from other formats to pdfs like markdown

# XPS hardware config references:
https://gist.github.com/matthewbauer/68775d50d371eafb0de41a49f81f9cca

## Secret management
- using a sops to modules that accepts file params
- if other items need to be secured then we move to git-crypt

## Sops adding secrets
- run `make editSopsSecret` with secrets in env
- need to add key in configuration.nix


## Scanner
https://nixos.wiki/wiki/Scanners


## Random
- https://www.reddit.com/r/programming/comments/q0oqai/what_is_wrong_with_geeksforgeeks_why_forcing_to/ <- add this to ublock origin


### LUKS:
- https://scs.community/2023/02/24/impact-of-disk-encryption/
- overview: https://infosecwriteups.com/how-luks-works-with-full-disk-encryption-in-linux-6452ad1a42e8
- broke pendrive: https://ubuntuforums.org/showthread.php?t=2472411
    `cryptsetup -y -v luksFormat /dev/sdb1` failed then pendrive was write protected

## wipe
- fastest way to wipe whole disk might be to use in build feature: https://unix.stackexchange.com/questions/553143/how-can-i-speed-up-secure-erasing-of-a-disk, might now be applicable to your harddisk
- https://superuser.com/questions/831486/complete-wiping-of-hard-drive-shred-wipe-or-dd
- https://manpages.ubuntu.com/manpages/impish/en/man1/srm.1.html

### YUBIKEY:
https://github.com/drduh/YubiKey-Guide#nixos






## General notes on darwin:
-  need to install homebrew, gpg suite manually
