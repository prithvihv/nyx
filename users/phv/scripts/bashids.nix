{ stdenv
, lib
, fetchFromGitHub
, bash
, subversion
, makeWrapper
}:
  stdenv.mkDerivation {
    pname = "bashids";
    version = "635304b";
    src = fetchFromGitHub {
      owner = "benwilber";
      repo = "bashids";
      rev = "635304bd21d75aabb4dad1a2a5e1a4c98b2abfa7";
      sha256 = "skt1wJT65ZzpFJXyxP/fltM7oIvhGZqGKiaEXwNwrA4=";
    };
    buildInputs = [ bash subversion ];
    # nativeBuildInputs = [ makeWrapper ];
    installPhase = ''
      mkdir -p $out/bin
      cp bashids  $out/bin/bashids.sh
    '';
    #   wrapProgram $out/bin/github-downloader.sh \
        # --prefix PATH : ${lib.makeBinPath [ bash subversion ]}
  }
