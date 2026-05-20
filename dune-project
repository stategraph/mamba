(lang dune 3.16)

(name mamba)

(generate_opam_files true)

(authors "Josh Pollara <josh@terrateam.io>")
(maintainers "josh@terrateam.io")
(license ISC)
(source (github stategraph/mamba))

(package
 (name mamba)
 (synopsis "A Cobra-inspired CLI library for OCaml")
 (description
  "Mamba is a library for building modern command-line applications in OCaml,
inspired by Go's Cobra. It provides a declarative API for nested subcommands,
typed flags with inheritance, lifecycle hooks, did-you-mean suggestions, shell
completion script generation, and man-page generation -- with no dependencies
beyond the OCaml standard library.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (alcotest (and :with-test (>= 1.7.0)))))

(package
 (name mamba-cli)
 (synopsis "Scaffolding generator for mamba CLI projects")
 (description
  "A small companion binary that scaffolds new mamba-based CLI projects.")
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.16))
  (mamba (= :version))))
