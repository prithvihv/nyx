{ ... }:

{
  services.tailscale = {
    enable = true;
    openFirewall = true;
    authKeyFile = "/var/lib/tailscale/authkey";
    # we have pi-hole :)
    extraUpFlags = [ "--accept-dns=false" ];
  };
}
