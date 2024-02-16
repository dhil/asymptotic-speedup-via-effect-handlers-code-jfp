(lang dune 3.14)

(name benchmarks)

(generate_opam_files true)

(source
 (github dhil/asymptotic-speedup-via-effect-handlers-code-jfp))
(authors "Daniel Hillerström")
(maintainers "Daniel Hillerström")
(license GPL-3.0-or-later)

(documentation https://github.com/dhil/asymptotic-speedup-via-effect-handlers-code-jfp)

(package
 (name benchmarks)
 (synopsis "Accompanying code for the 'Asymptotic Speedup via Effect Handlers' article")
 (description "Source code and data for the experiments described in Section 10")
 (depends (ocaml (= 5.1.1))
          (multicont (= 1.0.1))
          (zarith (= 1.13))
           dune)
 (allow_empty)
 (tags
  (topics "scientific paper" "effect handlers")))

