(lang dune 3.14)
(using menhir 3.0)
(name acppo)

(generate_opam_files true)

(source
 (github MizurenNanako/another-c-preprocessor))

(authors "Cherry Sigma")

(maintainers "Cherry Sigma")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name acppo)
 (synopsis "acppo")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
