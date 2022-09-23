(lang dune 3.3)

(using menhir 2.0)

(name Dyri)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/Dyri))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name Cli)
 (synopsis "Compiler cli")
 (depends ocaml dune llvm))
