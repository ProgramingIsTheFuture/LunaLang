# Dyri Language

# Design

Run:

```sh
dune build @doc
```

```sh
open _build/default/_doc/_html/index.html
```

After the browser is opened with this html page,
click in cli.

# TODO

- [ ] Deploy docs to github pages with github actions
- Update language design
  - [ ] Better base type system
  - [ ] Type definition
  - [ ] More about the blocks
  - [ ] Import and Export (support multiple files in single project)
- Better AST
  - [ ] Standard Ast
  - [ ] Typed Ast
- [ ] Lexer to lex and parse all the syntax into the AST
- [ ] Check the types that can be type-inferred and statically typed
- [ ] Compile to LLVM

# Plan

- Lex and Parse the language
- Check the types
- Compile to LLVM
- Support cross-compilation
- Import and Export libraries
- Start building externals libraries to general use of the language
