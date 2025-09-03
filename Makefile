.PHONY: all build test clean examples

all: build

build:
	dune build

test: build
	dune test

examples: test

run: build
	@echo "Example: (if b (:= x 1) (:= x \"hello\"))" > /tmp/test.gt
	dune exec graded-typing -- /tmp/test.gt

ghost: build
	@echo "(seq (if b (:= x 1) (:= x \"hello\")) (:= y (+ x 1)))" > /tmp/test.gt
	dune exec graded-typing -- -ghost /tmp/test.gt

clean:
	dune clean
	rm -f /tmp/test.gt

install:
	opam install dune qcheck

format:
	ocamlformat -i src/*.ml examples/*.ml

doc:
	dune build @doc
	@echo "Documentation built in _build/default/_doc/_html/"
