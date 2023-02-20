.PHONY: build clean repl watch ;\
	test unit integration functional ;\
	cic ci formatc format lint lintc ;\
	haddock haddockc hackage

# core

T = ""

build:
	if [ -z "$(T)" ]; then \
		cabal build; \
	else \
		cabal build $(T); \
	fi

clean:
	cabal clean

test:
	if [ -z "$(T)" ]; then \
		RUN_DOCTEST=1 cabal test; \
	else \
		RUN_DOCTEST=1 cabal test $(T); \
	fi

repl:
	if [ -z "$(T)" ]; then \
		cabal repl; \
	else \
		cabal repl $(T); \
	fi

watch:
	ghcid --command "cabal repl $(T)"

# ci

cic: formatc lintc haddockc

ci: lint format haddockc

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
	cp -r dist-newstyle/build/x86_64-linux/ghc-9.2.4/smart-math-0.1/doc/html/smart-math/* docs/

haddockc:
	nix run github:tbidne/nix-hs-tools/0.8#haddock-cov -- -t 90
