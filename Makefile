.PHONY: test doc

all: build

build:
	dune build src/lib/go2b.cma
	dune build src/lib/go2b.cmxa
#	dune build src/bin/go2b_top.bc

utop:
	dune utop src/lib

doc: 
	dune build @doc
	cp -r _build/default/_doc/_html/* ./docs

clean:
	dune clean

clobber: clean
	\rm -f *~


