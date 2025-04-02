{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "kubectl-iexec";
  version = "1.19.14";

  src = fetchFromGitHub {
    owner = "gabeduke";
    repo = "kubectl-iexec";
    rev = "3a425253efdc0be92a3fad3d05656e9487a753f0";
    hash = "sha256-WU9NJExrD+umEPCUsGsNdbmTSpJTh7FtNHf9Z9F4SzM="; # You'll need to replace this
  };

  vendorHash = "sha256-2P9z3n2Knydn1hq8jVLWY7IRkguSKn+lcn0tMItrRiE="; # You'll need to replace this

  ldflags = [ "-s" "-w" ];

  meta = with lib; {
    description = "Kubectl plugin to interactively exec into a pod";
    homepage = "https://github.com/gabeduke/kubectl-iexec";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "kubectl-iexec";
  };
}
