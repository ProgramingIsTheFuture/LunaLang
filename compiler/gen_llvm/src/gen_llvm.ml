(* 1. Convert AST types to LLVM types *)
(* 2. Compile basic expressions *)
(* 3. Add the printf function to the environment inside the typechecker *)

let compile _ast = Convert.main_prog ()
