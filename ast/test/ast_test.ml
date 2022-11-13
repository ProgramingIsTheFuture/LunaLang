module AstAstTest = struct
  open Ast.Ast

  let str_of_typ = function
    | TTyp (Some s) -> s
    | TTyp (None) -> ""
  ;;

  let rec to_str = function
    | Var(s) :: ll->
      Fmt.str "Variable: %s\n" s ^ to_str ll
    | Const (VInt v) :: ll ->
      Fmt.str "Const: int - %d\n" (Int64.to_int v) ^ to_str ll
    | Let ((s, t), ds) :: ll ->
      Fmt.str "Let %s (typ %s) = " s (str_of_typ t) ^ (to_str [ds]) ^ to_str ll
    | Fun ((s, t), ds) :: ll ->
      Fmt.str "Fun (param: %s (typ %s)) = " s (str_of_typ t) ^ (to_str [ds]) ^ to_str ll
    | AnFun (s, t, ds) :: ll ->
      Fmt.str "AnFun (param: %s (typ %s)) = " s (str_of_typ t) ^ (to_str [ds]) ^ to_str ll
    | Block (dsl) :: ll ->
      Fmt.str "Block {\n" ^ (to_str dsl) ^ "};" ^ to_str ll
    | [] -> ""

    | _ -> assert false 
end

module TypedAstTest = struct 
  open Ast.TypedAst

  let str_of_primitive = function
    | TInt -> "int"
    | TInt32 -> "int32"
    | TString -> "string"
    | TBool -> 
      "bool"
    | TGeneric ->
      "any"
    | TCustom s ->
      s
  ;;


  let rec str_of_typ = function
    | TSeq (s, Some n) ->
      (str_of_primitive s) ^ " -> " ^ str_of_typ n
    | TSeq (s, None) ->
      str_of_primitive s
  ;;

  let str_of_op = function
    | Add -> "+"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Sub -> "-"
  ;;

  let str_of_val = function
    | VInt i -> Fmt.str "%d" (Int64.to_int i)
    | VInt32 i -> Fmt.str "%d" (Int32.to_int i)
    | VString s -> s
    | VBool b -> Fmt.str "%b" b
  ;;

  let get_typ = function
    | VInt _ -> TSeq(TInt, None)
    | VInt32 _ -> TSeq(TInt32, None)
    | VString _ -> TSeq(TString, None)
    | VBool _ -> TSeq(TBool, None)
  ;;

  let rec to_str = function
    | Var(s) :: ll->
      Fmt.str "[Variable: %s]" s ^ to_str ll
    | Const (v) :: ll ->
      Fmt.str "(Const: %s = %s)" (str_of_typ (get_typ v)) (str_of_val v) ^ to_str ll
    | Let ((s, t), ds) :: ll ->
      Fmt.str "[Let %s (typ %s) = " s (str_of_typ t) ^ (to_str [ds]) ^ "];\n" ^ to_str ll
    | Fun ((s, t), ds) :: ll ->
      Fmt.str "[Fun (param: %s (typ %s)) = " s (str_of_typ t) ^ (to_str [ds]) ^ "]" ^ to_str ll
    | AnFun (s, t, ds) :: ll ->
      Fmt.str "[AnFun (param: %s (typ %s)) = " s (str_of_typ t) ^ (to_str [ds]) ^ "]" ^ to_str ll
    | Block (dsl) :: ll ->
      Fmt.str "Block {\n" ^ (to_str dsl) ^ "};\n" ^ to_str ll
    | Op (ds1, op, ds2) :: ll->
      Fmt.str "[Op: " ^ (to_str [ds1]) ^ " " ^ (str_of_op op) ^ " " ^ (to_str [ds2]) ^ "]" ^ to_str ll
    | Apply (s, dsl) :: ll ->
      Fmt.str "[Aplying " ^ s ^ "with " ^ (to_str dsl) ^ "]" ^ to_str ll
    | [] -> ""
    | _ :: ll -> to_str ll

  let rec to_str_stmt = function
    | { desc = ss; typ = _pos } :: l ->
      to_str [ss] ^ (to_str_stmt l)
    | [] -> ""
end

let pp_ast ppf v = 
  Fmt.pf ppf "%s" (AstAstTest.to_str v)
;;

let pp_typed_ast ppf v = 
  Fmt.pf ppf "%s" (TypedAstTest.to_str v)
;;

let pp_typed_stmt ppf v = 
  Fmt.pf ppf "%s" (TypedAstTest.to_str_stmt v)
;;

let desc_of_aststmt (s: Ast.Ast.stmt) =
  s.desc;;

let desc_of_typedstmt (s: Ast.TypedAst.stmt) =
  s.desc;;

open Alcotest
let astastcode = testable pp_ast ( = );; 
let typedastcode = testable pp_typed_ast ( = );; 
let typedaststmt = testable pp_typed_stmt ( = );; 

let desc_of_astcode (l: Ast.Ast.code): Ast.Ast.desc list =
  l |> List.map desc_of_aststmt;;

let desc_of_typedcode (l: Ast.TypedAst.code): Ast.TypedAst.desc list =
  l |> List.map desc_of_typedstmt;;

let test_astast (s: string) (ast_expt: Ast.Ast.desc list) (ast_val: Ast.Ast.desc list) () = 
  Alcotest.(check astastcode) s ast_expt ast_val;;

let test_typedast (s: string) (ast_expt: Ast.TypedAst.desc list) (ast_val: Ast.TypedAst.desc list) () = 
  Alcotest.(check typedastcode) s ast_expt ast_val;;

let test_typedast_code (s: string) (ast_expt: Ast.TypedAst.code) (ast_val: Ast.TypedAst.code) () = 
  Alcotest.(check typedaststmt) s ast_expt ast_val;;

type test_ast_case =
  string * Ast.Ast.desc list * Ast.Ast.desc list;;

type test_typed_case =
  string * Ast.TypedAst.desc list * Ast.TypedAst.desc list;;

type test_typed_code_case =
  string * Ast.TypedAst.code * Ast.TypedAst.code;;

let run_ast (modu: string) (tests: test_ast_case list)=
  run modu [
    modu, (List.map 
      (fun (s, vall, expt) -> 
        test_case s `Quick (test_astast s expt vall)
      ) 
      tests);
  ];;

let run_typedast (modu: string) (t1: string) (t2: string) (tests1: test_typed_case list) (tests2: test_typed_code_case list) =
  run modu [
    t1, (List.map
      (fun (s, vall, expt) ->
        test_case s `Quick (test_typedast s expt vall)
      )
      tests1
    );
    t2, (
      List.map
      (fun (s, vall, expt) ->
        test_case s `Quick (test_typedast_code s expt vall)
      )
      tests2
    )
  ];;
