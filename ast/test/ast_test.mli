open Alcotest

type test_ast_case = string * Ast.Ast.code * Ast.Ast.code
type test_typed_code_case = string * Ast.TypedAst.code * Ast.TypedAst.code

val run_ast : string -> test_ast_case list -> return
val run_typedast : string -> string -> test_typed_code_case list -> return
