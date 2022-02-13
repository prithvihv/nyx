{ stdenv
, lib
, fetchFromGitHub
, bash
, subversion
, makeWrapper
}:
  stdenv.mkDerivation {
    pname = "rofi-bluetooth";
    version = "893db1f";
    src = fetchFromGitHub {
      owner = "nickclyde";
      repo = "rofi-bluetooth";
      rev = "893db1f2b549e7bc0e9c62e7670314349a29cdf2";
      sha256 = "3oROJKEQCuSnLfbJ+JSSc9hcmJTPrLHRQJsrUcaOMss=";
    };
    buildInputs = [ bash subversion ];
    # nativeBuildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp rofi-bluetooth $out/bin/rofi-bluetooth.sh
    '';
    #   wrapProgram $out/bin/github-downloader.sh \
        # --prefix PATH : ${lib.makeBinPath [ bash subversion ]}
  }
