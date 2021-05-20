.PHONY: examples doc

all: build examples

build:
	dune build src/lib/go2b.cma
#	dune build src/lib/go2b.cmxa
#	dune build src/bin/go2b_top.bc

examples:
	dune build examples/ex1/main.bc

utop:
	dune utop src/lib

doc: 
	dune build @doc
	cp -r _build/default/_doc/_html/* ./docs

clean:
	dune clean

clobber: clean
	\rm -f *~


