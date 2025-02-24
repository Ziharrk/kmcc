########################################################################
# Makefile for KMCC
########################################################################

# Essential system dependencies
STACK := $(shell which stack)
CYPM  := $(shell which cypm)

ifeq ($(STACK),)
$(error Please make sure that 'stack' (the Haskell Stack build tool) is on your PATH or specify it explicitly by passing 'make STACK=...')
endif

ifeq ($(CYPM),)
$(error Please make sure that 'cypm' (the Curry Package Manager) is on your PATH or specify it explicitly by passing 'make CYPM=...')
endif

# The root directory of the installation
export ROOT = $(CURDIR)
# The binary directory
export BINDIR = $(ROOT)/bin
# The directory containaing the REPL sources
export REPLDIR = $(ROOT)/repl

# The template Installation module for use by the REPL
INSTALLCURRYIN = $(REPLDIR)/src/Installation.curry.in
# The (generated) Installation module for use by the REPL
INSTALLCURRY = $(REPLDIR)/src/Installation.curry

# Git history might be unavailable in distributions so that we use
# a flag to check for it: 1 if true, 0 otherwise
GIT_HISTORY_AVAILABLE := $(shell ! test -d "$(ROOT)/.git"; echo $$?)

# Compiler version from the compiler cabal file
export VERSION := $(shell head -3 $(ROOT)/compiler/kmcc.cabal | tail -1 | cut -c21-)
export MAJORVERSION    = $(word 1,$(subst ., ,$(VERSION)))
export MINORVERSION    = $(word 2,$(subst ., ,$(VERSION)))
export REVISIONVERSION = $(word 3,$(subst ., ,$(VERSION)))
# Compiler date from the last log entry of the git repo
ifeq ($(GIT_HISTORY_AVAILABLE),1)
export COMPILERDATE := $(shell git log -1 --format="%ci" | cut -c-10)
else
export COMPILERDATE := $(shell date "+%Y-%m-%d")
endif
# Actual installation date
export INSTALLDATE := $(shell date)

##############################################################################
.PHONY: all
all: bin/kmcc_c frontend repl prebuild_prelude generate_distribution

.PHONY: bin/kmcc_c
bin/kmcc_c:
	$(STACK) build kmcc:exe:kmcc_c --copy-bins

.PHONY: frontend
frontend: bin/kmcc-frontend

.PHONY: bin/kmcc-frontend
bin/kmcc-frontend:
	$(STACK) build curry-frontend:exe:curry-frontend --copy-bins
	mv bin/curry-frontend bin/kmcc-frontend

.PHONY: repl
repl: bin/kmcc_repl

bin/kmcc_repl: $(INSTALLCURRY) repl/src/KMCC/ReplConfig.curry repl/package.json
	$(CYPM) update
	cd repl && $(CYPM) -d BININSTALLPATH=$(BINDIR) install
	# add alias `bin/curry`:
	cd $(BINDIR) && rm -f curry && ln -s kmcc curry

# Generate a source module with metadata about the KMCC installation for use by the compiler
$(INSTALLCURRY): $(INSTALLCURRYIN) compiler/kmcc.cabal Makefile
	@echo "Generating REPL installation module"
	@envsubst < $< > $@

.PHONY: clean
clean:
	rm -rf bin/kmcc_repl
	rm -rf bin/kmcc_c
	cd repl && $(CYPM) clean
	rm -rf libs/src/.curry
	rm -f $(INSTALLCURRY)
	$(STACK) clean

.PHONY: prebuild_prelude
prebuild_prelude: bin/kmcc_c
	$(info "Pre-Compiling Prelude")
	bin/kmcc_c libs/src/Prelude.curry

.PHONY: generate_distribution
generate_distribution:
	./genDistribution.sh
