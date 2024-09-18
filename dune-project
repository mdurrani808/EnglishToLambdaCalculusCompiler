(lang dune 3.6)

(name lcc)

(generate_opam_files false)

(source
 (github umd-cmsc330/summer23-ta))

(authors "Cliff Bakalian")

(maintainers "Cliff Bakalian")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lcc)
 (synopsis "Lamda Calc Compiler")
 (description "A Interpreter for Lambda Calc")
 (depends ocaml dune)
 (allow_empty)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
