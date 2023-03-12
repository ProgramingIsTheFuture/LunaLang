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
 (name ast)
 (synopsis "Abstract syntax tree")
 (depends ocaml dune))

(package
 (name parse)
 (synopsis "Parse library")
 (depends ocaml dune ast))

(package
 (name gen_llvm)
 (synopsis "Compiler library")
 (depends ocaml dune ast llvm))

(package
 (name typecheck)
 (synopsis "Check the AST types")
 (depends ocaml dune ast))
