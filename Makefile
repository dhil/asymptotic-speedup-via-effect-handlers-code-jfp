.PHONY: build
build: bin/dune lib/dune
	opam exec -- dune build
	mkdir -p data

.PHONY: run
run:	data
	opam exec -- dune exec bin/runner.exe

.PHONY: single-threaded-run
single-threaded-run:
	opam exec -- dune exec bin/runner.exe -- --sequential

.PHONY: clean
clean:
	opam exec -- dune clean
	rm -rf data
