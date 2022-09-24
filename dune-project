(lang dune 3.3)

(using menhir 2.0)

(name Dyri)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/Dyri))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(package
 (name Cli)
 (synopsis "Compiler cli")
 (depends ocaml dune))

(package
 (name Compiler)
 (synopsis "Compiler library")
 (depends ocaml dune llvm))

(package
 (name Lex)
 (synopsis "Lexer library")
 (depends ocaml dune))
