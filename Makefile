.PHONY: doctest repl watch ;\
	cic ci formatc format lint lintc ;\
	haddock hackage

# core

T = ""

doctest:
	cabal build --write-ghc-environment-files=always; \
	RUN_DOCTEST=1 cabal test doctest; \
	rm .ghc.environment.*

repl:
	if [ -z "$(T)" ]; then \
		cabal repl; \
	else \
		cabal repl $(T); \
	fi

watch:
	ghcid --command "cabal repl $(T)"

# ci

cic: formatc lintc

ci: lint format

# formatting

formatc:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --check ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode check

format:
	nix run github:tbidne/nix-hs-tools/0.8#nixpkgs-fmt ;\
	nix run github:tbidne/nix-hs-tools/0.8#cabal-fmt -- --inplace ;\
	nix run github:tbidne/nix-hs-tools/0.8#ormolu -- --mode inplace

# linting

lint:
	nix run github:tbidne/nix-hs-tools/0.8#hlint -- --refact

lintc:
	nix run github:tbidne/nix-hs-tools/0.8#hlint

haddock:
	cabal haddock --haddock-hyperlink-source --haddock-quickjump ;\
	mkdir -p docs/ ;\
	find docs/ -type f | xargs -I % sh -c "rm -r %" ;\
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.4.4/smart-math-0.1/doc/html/smart-math/* docs/
