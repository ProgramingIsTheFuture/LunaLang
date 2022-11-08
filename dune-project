(lang dune 3.3)

(using menhir 2.0)

(name Dyri)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/Dyri))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(documentation https://programingisthefuture.github.io/Dyri/)

(package
 (name cli)
 (synopsis "Compiler cli")
 (depends ocaml dune))

(package
 (name ast)
 (synopsis "Abstract syntax tree")
 (depends ocaml dune))

(package
 (name ast_test)
 (synopsis "Abstract syntax tree")
 (depends ocaml dune alcotest))

(package
 (name lex)
 (synopsis "Lexer library")
 (depends ocaml dune ast))

(package
 (name compiler)
 (synopsis "Compiler library")
 (depends ocaml dune ast llvm))

(package
 (name typing)
 (synopsis "Check the AST types")
 (depends ocaml dune ast))
