(lang dune 3.3)

(using menhir 2.0)

(name Dyri)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/Dyri))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(package
 (name cli)
 (synopsis "Compiler cli")
 (depends ocaml dune))

(package
 (name compiler)
 (synopsis "Compiler library")
 (depends ocaml dune llvm))

(package
 (name lex)
 (synopsis "Lexer library")
 (depends ocaml dune))
