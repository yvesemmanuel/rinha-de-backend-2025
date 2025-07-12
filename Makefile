SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c -e

.PHONY: install-haskell
install-haskell:
	@echo "Install GHCup"
	@curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

	@echo "Add GHCup to PATH (add this to your shell profile)"
	@export PATH="$HOME/.ghcup/bin:$PATH"

	@echo "Install Stack"
	@sudo apt update
	@sudo apt install haskell-stack

.PHONY: build-dev
build-dev:
	@stack clean
	@stack init --force
	@stack install

.PHONY: test
test:
	@stack test

.PHONY: check
check:
	hlint .

.PHONY: build
build:
	@stack init
	@stack build --copy-bins

.PHONY: run
run:
	@stack run
