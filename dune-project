(lang dune 3.3)

(using menhir 2.0)

(name LunaLang)

(generate_opam_files true)

(source
 (github ProgramingIsTheFuture/LunaLang))

(authors "ProgramingIsTheFuture")

(maintainers "ProgramingIsTheFuture")

(documentation https://programingisthefuture.github.io/LunaLang/)

(package
 (name cli)
 (allow_empty)
 (synopsis "Compiler cli")
 (depends ocaml dune))

(package
 (name ast)
 (synopsis "Abstract syntax tree")
 (depends ocaml dune))

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

(package
 (name luna_lang)
 (synopsis "Compiler library")
 (depends ocaml dune ast llvm))
