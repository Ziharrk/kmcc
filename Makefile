.PHONY: all
all: bin/kmcc_c bin/kmcc-frontend bin/kmcc_repl

.PHONY: bin/kmcc_c
bin/kmcc_c:
	stack build kmcc:exe:kmcc_c --copy-bins

.PHONY: bin/kmcc-frontend
bin/kmcc-frontend:
	stack build curry-frontend:exe:curry-frontend --copy-bins
	mv bin/curry-frontend bin/kmcc-frontend

bin/kmcc_repl: repl/src/KMCC/ReplConfig.curry
	cd repl && cypm curry :l KMCC.ReplConfig :save :q
	mv repl/KMCC.ReplConfig bin/kmcc_repl

.PHONY: clean
clean:
	rm -rf bin/kmcc_repl
	rm -rf bin/kmcc_c
	rm -rf repl/src/.curry
	rm -rf lib/.curry
	stack clean
