open Alcotest

val desc_of_astcode: Ast.Ast.code -> Ast.Ast.desc list 
val desc_of_typedcode: Ast.TypedAst.code -> Ast.TypedAst.desc list 

type test_ast_case =
  string * Ast.Ast.desc list * Ast.Ast.desc list

type test_typed_case =
  string * Ast.TypedAst.desc list * Ast.TypedAst.desc list

type test_typed_code_case =
  string * Ast.TypedAst.code * Ast.TypedAst.code;;

val run_ast: string -> test_ast_case list -> return
val run_typedast: string -> string -> string -> test_typed_case list -> test_typed_code_case list -> return
