module AstAstTest = struct
  open Ast.Ast
  let rec to_str = function
    | Var(s) :: ll->
      Fmt.str "Variable: %s\n" s ^ to_str ll
    | Const (VInt v) :: ll ->
      Fmt.str "Const: int - %d\n" (Int64.to_int v) ^ to_str ll
    | _ -> assert false 
end

module TypedAstTest = struct 
  open Ast.TypedAst
  let rec to_str = function
    | Var(s) :: ll->
      Fmt.str "Variable: %s\n" s ^ to_str ll
    | Const (VInt v) :: ll ->
      Fmt.str "Const: int - %d\n" (Int64.to_int v) ^ to_str ll
    | _ -> assert false 
end

let pp_ast ppf v = 
  Fmt.pf ppf "%s" (AstAstTest.to_str v)
;;

let pp_typed_ast ppf v = 
  Fmt.pf ppf "%s" (TypedAstTest.to_str v)
;;

let desc_of_aststmt (s: Ast.Ast.stmt) =
  s.desc;;

let desc_of_typedstmt (s: Ast.TypedAst.stmt) =
  s.desc;;

open Alcotest
let astastcode = testable pp_ast ( = );; 
let typedastcode = testable pp_typed_ast ( = );; 

let desc_of_astcode (l: Ast.Ast.code): Ast.Ast.desc list =
  l |> List.map desc_of_aststmt;;

let desc_of_typedcode (l: Ast.TypedAst.code): Ast.TypedAst.desc list =
  l |> List.map desc_of_typedstmt;;

let test_astast (s: string) (ast_expt: Ast.Ast.desc list) (ast_val: Ast.Ast.desc list) () = 
  Alcotest.(check astastcode) s ast_expt ast_val;;

let test_typedast (s: string) (ast_expt: Ast.TypedAst.desc list) (ast_val: Ast.TypedAst.desc list) () = 
  Alcotest.(check typedastcode) s ast_expt ast_val;;

type test_ast_case =
  string * Ast.Ast.desc list * Ast.Ast.desc list

type test_typed_case =
  string * Ast.TypedAst.desc list * Ast.TypedAst.desc list

let run_ast (modu: string) (tests: test_ast_case list)=
  run modu [
    modu, (List.map 
      (fun (s, vall, expt) -> 
        test_case s `Quick (test_astast s expt vall)
      ) 
      tests);
  ];;

let run_typedast (modu: string) (tests: test_typed_case list) =
  run modu [
    modu, (List.map
      (fun (s, vall, expt) ->
        test_case s `Quick (test_typedast s expt vall)
      )
      tests)
  ]
