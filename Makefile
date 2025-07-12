

install-haskell:
	@echo "Install GHCup"
	@curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh

	@echo "Add GHCup to PATH (add this to your shell profile)"
	@export PATH="$HOME/.ghcup/bin:$PATH"

	@echo "Install Stack"
	@sudo apt update
	@sudo apt install haskell-stack

build-dev:
	@stack init --force
	@stack install

test:
	@stack test

check:
	hlint .

build:
	@stack init
	@stack build --copy-bins

run:
	@stack run
