SECRETS := common common_server common_cloud latitude_7390_server

# Create any missing secrets file, encrypted to the recipients in .sops.yaml.
# Safe to re-run: existing files are left untouched. On failure the plaintext
# seed is deleted rather than left in a directory git would happily commit.
initSecrets:
	mkdir -p secrets
	for f in $(SECRETS); do \
	  if [ -e secrets/$$f.yaml ]; then \
	    echo "exists   secrets/$$f.yaml"; \
	  else \
	    printf 'placeholder: replace-me\n' > secrets/$$f.yaml; \
	    if sops -e -i secrets/$$f.yaml; then \
	      echo "created  secrets/$$f.yaml"; \
	    else \
	      rm -f secrets/$$f.yaml; \
	      echo "FAILED   secrets/$$f.yaml (plaintext removed)"; \
	      exit 1; \
	    fi; \
	  fi; \
	done

# Re-encrypt every secrets file after changing recipients in .sops.yaml.
updateSopsKeys:
	for f in secrets/*.yaml; do sops updatekeys -y "$$f"; done
