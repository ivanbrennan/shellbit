PROJECT_NAME ?= nix-shell-bit
PROJECT_ROOT ?= $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

NIXPKGS_OWNER ?= NixOS
NIXPKGS_REPO  ?= nixpkgs

TEST_FLAGS  = --failure-report=$$PWD/.hspec-failures
TEST_FLAGS += --rerun-all-on-success --rerun

.PHONY: build
build: default.nix
	@echo "Running build"
	@nix-shell \
		--pure ./nix \
		--attr dev \
		--run "cabal v2-build --ghc-option=-Werror"

.PHONY: test
test: default.nix
	@echo "Running tests"
	@nix-shell \
		--pure ./nix \
		--attr dev \
		--run "cabal v2-run nix-shell-bit-test -- $(TEST_FLAGS)"

.PHONY: integration-test
integration-test: default.nix
	@echo "Running integration tests"
	@nix-shell \
		--pure ./nix \
		--keep NIX_PATH \
		--attr dev \
		--run "test/integration"

.PHONY: nix-build
nix-build: default.nix
	@echo "Running nix-build"
	@nix-build ./nix --attr full

.PHONY: completions
completions: default.nix
	@echo "Generating completions"
	@nix-shell \
		--pure ./nix/scripts/generate-completions.nix \
		--argstr projectRoot $(PROJECT_ROOT)

.PHONY: install
install: default.nix
	@echo "Installing to user profile with nix-env"
	@nix-env --install --file ./nix --attr full

.PHONY: uninstall
uninstall: default.nix
	@echo "Uninstalling from user profile with nix-env"
	@nix-env --uninstall nix-shell-bit

default.nix: nix-shell-bit.cabal
	@echo "Generating default.nix"
	@nix-shell \
		--pure ./nix/scripts/generate-default.nix \
		--argstr projectRoot $(PROJECT_ROOT)

.PHONY: update-nixpkgs
update-nixpkgs: require-rev
	@nix-shell \
		--pure ./nix/scripts/update-nixpkgs.nix \
		--argstr projectRoot $(PROJECT_ROOT) \
		--argstr owner $(NIXPKGS_OWNER) \
		--argstr repo $(NIXPKGS_REPO) \
		--argstr rev $(REV)

.PHONY: require-rev
require-rev:
ifndef REV
	$(error REV=<revision> must be specified)
endif

.PHONY: clean
clean:
	rm -rf $(PROJECT_ROOT)/dist
	rm -rf $(PROJECT_ROOT)/dist-newstyle
	rm -f  $(PROJECT_ROOT)/.ghc.environment.*
	rm -f  $(PROJECT_ROOT)/result
