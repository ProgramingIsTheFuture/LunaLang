# Project structure

This project will contain two executables:
- Compiler
- Top level

### Compiler

The compiler will have the ability to compile or check your program.

### Top level

The top level will evaluate expression by expression and give you the results of each expression.

---

Both of these main executables will be stored inside the `/src` directory.
As for the rest of the project, will be stored inside multiple libraries inside a directory called compiler.

## Compiler internals

All compiler internals can be found inside the `/compiler` directory.
Every module inside the compiler internals have a `docs` directory associated with it.
Available modules:
- AST (Abstract syntax tree): where we define the Syntax tree for the language
- Parse: where we parse the text into the AST 
- Type check: module to check the types and the correctness of our program
- Gen Llvm: generate the desired llvm code
