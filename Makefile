
all: fmt build test

fmt: 
	ocp-indent --inplace src/*.ml test/*.ml

build:
	dune build

test:
	dune runtest

.PHONY: all fmt build test
