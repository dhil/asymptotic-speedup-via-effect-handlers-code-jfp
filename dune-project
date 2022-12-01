(lang dune 3.4)

(name benchmarks)

(generate_opam_files true)

(source
 (github dhil/asymptotic-speedup-via-effect-handlers-code-jfp))

(authors "Daniel Hillerström")

(maintainers "Daniel Hillerström")

(license LICENSE)

(documentation https://github.com/dhil/asymptotic-speedup-via-effect-handlers-code-jfp)

(package
 (name benchmarks)
 (synopsis "Accompanying code for the 'Asymptotic Speedup via Effect Handlers' article")
 (description "Source code and data for the experiments described in Section 10")
 (depends (ocaml (>= 5.0.0~beta1)) zarith dune)
 (allow_empty)
 (tags
  (topics "scientific paper" "effect handlers")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
