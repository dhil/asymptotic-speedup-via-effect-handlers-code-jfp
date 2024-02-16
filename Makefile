.PHONY: build
build: bin/dune lib/dune
	opam exec -- dune build

.PHONY: run
run:
	opam exec -- dune exec bin/runner.exe

.PHONY: single-threaded-run
single-threaded-run:
	opam exec -- dune exec bin/runner.exe -- --sequential

.PHONY: clean
clean:
	opam exec -- dune clean
	rm -rf data && mkdir data
