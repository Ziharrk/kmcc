########################################################################
# Makefile for KMCC
########################################################################

# Essential system dependencies
STACK := $(shell which stack)
CYPM  := $(shell which cypm)
PAKCS := $(shell which pakcs)

ifeq ($(STACK),)
$(error Please make sure that 'stack' (the Haskell Stack build tool) is on your PATH or specify it explicitly by passing 'make STACK=...')
endif

# The name of the Curry system (required by scripts)
export CURRYSYSTEM = kmcc

# The root directory of the installation
export ROOT = $(CURDIR)
# The binary directory
export BINDIR = $(ROOT)/bin
# The directory containaing the REPL sources
export REPLDIR = $(ROOT)/repl
# The directory where the actual libraries are located
export LIBDIR  = $(ROOT)/libs/src

# The template Installation module for use by the REPL
INSTALLCURRYIN = $(REPLDIR)/src/Installation.curry.in
# The (generated) Installation module for use by the REPL
INSTALLCURRY = $(REPLDIR)/src/Installation.curry

# Git history might be unavailable in distributions so that we use
# a flag to check for it: 1 if true, 0 otherwise
GIT_HISTORY_AVAILABLE := $(shell ! test -d "$(ROOT)/.git"; echo $$?)

# If using a dockerized PAKCS/CYPM, avoid passing a pakcs executable to it,
# since that would try to run a dockerized pakcs from within a dockerized pakcs-cypm, which is not possible.
# On CI, this is fixed by overriding this variable with no argument.
CURRYBIN_REPL_PAKCS_ARG = -d CURRYBIN=$(PAKCS)
CURRYBIN_REPL_PAKCS_ARG_2 =

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
all: bin/kmcc_c frontend generate_distribution repl prebuild_prelude
	# pre-compile all libraries to produce up-to-date intermediate files:
	$(MAKE) compile-all-libs
	$(MAKE) tools

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
repl: $(INSTALLCURRY)
ifeq (,$(wildcard .git)) # build REPL with script if this is not a git repo
	scripts/compile-repl.sh
else
	$(MAKE) bin/kmcc_repl
endif

bin/kmcc_repl: $(INSTALLCURRY) repl/src/KMCC/ReplConfig.curry repl/package.json
	@if [ ! -x "$(CYPM)" ] ; then \
	  echo "Executable 'cypm' not found!" ; \
	  echo "Please make sure that 'cypm' (the Curry Package Manager) is" ; \
	  echo "on your PATH or specify it explicitly by 'make CYPM=...'" ; \
	  exit 1 ; \
	fi
	$(CYPM) update
ifeq (,$(wildcard bin/kmcc_repl)) # build REPL using PAKCS, since we need the REPL to compile the REPL using KMCC
	@if [ ! -x "$(PAKCS)" ] ; then \
	  echo "Executable 'pakcs' not found!" ; \
	  echo "Please make sure that 'pakcs' is on your PATH or" ; \
	  echo "specify it explicitly by 'make PAKCS=...'" ; \
	  exit 1 ; \
	fi
	cd repl && $(CYPM) $(CURRYBIN_REPL_PAKCS_ARG) -d BININSTALLPATH=$(BINDIR) install
	# recompile using the newly built REPL:
	cd repl && $(CYPM) $(CURRYBIN_REPL_PAKCS_ARG_2) -d BININSTALLPATH=$(BINDIR) install
else
	cd repl && $(CYPM) -d BININSTALLPATH=$(BINDIR) install
endif
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

# compile the targets for all libraries:
.PHONY: compile-all-libs
compile-all-libs:
	scripts/compile-all-libs.sh

# compile CPM if its sources are present:
export REPL = $(BINDIR)/$(CURRYSYSTEM) # required to build CPM
.PHONY: tools
tools:
	@if [ -d currytools ] ; then $(MAKE) -C currytools/cpm ; fi

# URL of the curry-tools git repository:
CURRYTOOLSURL = https://github.com/cau-placc/curry-tools.git

# download curry-tools directory containing sources of CPM:
currytools:
	git clone $(CURRYTOOLSURL) currytools

################################DISTRIBUTION##################################
# Create distribution version as tar file kmcc*.tar.gz:

# directory name of distribution
DISTDIR       = kmcc-$(VERSION)

.PHONY: dist
dist:
	rm -rf kmcc*.tar.gz $(DISTDIR) # remove any old distribution
	git clone . $(DISTDIR)         # create copy of git version
	cd $(DISTDIR) && git submodule update --init
	cd $(DISTDIR)/repl && $(CYPM) install --noexec
	cd $(DISTDIR) && $(MAKE) currytools
	tar cfvz $(DISTDIR).tar.gz --exclude-vcs --exclude-from=./.tarignore $(DISTDIR)
	rm -rf $(DISTDIR)
	@echo "----------------------------------------------------------------"
	@echo "Distribution file $(DISTDIR).tar.gz generated."


################################DISTRIBUTION##################################
