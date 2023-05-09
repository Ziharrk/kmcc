# The root directory of the installation
export ROOT = $(CURDIR)
# The binary directory
export BINDIR = $(ROOT)/bin

.PHONY: all
all: bin/kmcc_c bin/kmcc-frontend bin/kmcc_repl prebuild_prelude generate_distribution

.PHONY: bin/kmcc_c
bin/kmcc_c:
	stack build kmcc:exe:kmcc_c --copy-bins

.PHONY: bin/kmcc-frontend
bin/kmcc-frontend:
	stack build curry-frontend:exe:curry-frontend --copy-bins
	mv bin/curry-frontend bin/kmcc-frontend

bin/kmcc_repl: repl/src/KMCC/ReplConfig.curry repl/package.json
	cd repl && cypm -d BININSTALLPATH=$(BINDIR) install

.PHONY: clean
clean:
	rm -rf bin/kmcc_repl
	rm -rf bin/kmcc_c
	rm -rf repl/src/.curry
	rm -rf lib/.curry
	stack clean

.PHONY: prebuild_prelude
prebuild_prelude: bin/kmcc_c
	$(info "Pre-Compiling Prelude")
	bin/kmcc_c lib/Prelude.curry

.PHONY: generate_distribution
generate_distribution:
	./genDistribution.sh