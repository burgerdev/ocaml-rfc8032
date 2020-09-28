
_build/ctest: src/f25519.c src/f25519.h src/f25519_test.c Makefile
	gcc -Wall -Wextra -Wno-unused-parameter --pedantic --std=c99 -o $@ src/f25519.c src/f25519_test.c

_build/cstuff: src/f25519.c Makefile
	gcc -Wall -Wextra -Wno-unused-parameter --pedantic --std=c99 -o $@ $<

all: fmt build test

fmt: 
	ocp-indent --inplace src/*.ml test/*.ml

build:
	dune build

test:
	dune runtest

.PHONY: all fmt build test
