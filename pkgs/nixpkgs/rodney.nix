{ lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "rodney";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "rodney";
    rev = "v${version}";
    hash = "sha256-/iGsaMfK8zeUkTXwU63mAAb4VpsllG87EH8ycoFZs5k=";
  };

  vendorHash = "sha256-h4U43W3hLoF+p25/jNRaW8okeEzAZQEmKtwB5l4kGW4=";
  subPackages = [ "." ];
  doCheck = false;

  ldflags = [
    "-s"
    "-w"
    "-X main.version=v${version}"
  ];

  meta = with lib; {
    description = "CLI tool for interacting with the web";
    homepage = "https://github.com/simonw/rodney";
    license = licenses.asl20;
    mainProgram = "rodney";
    platforms = platforms.unix;
  };
}
