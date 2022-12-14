module AstAstTest = struct
  open Ast.Ast

  let rec compare_desc exp res (acc : bool list) =
    match (exp, res) with
    | ( { desc = Var _ as v1; pos = _pos } :: ll1,
        { desc = Var _ as v2; pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2 ((v1 = v2) :: acc)
    | ( { desc = Const _ as v1; pos = _pos } :: ll1,
        { desc = Const _ as v2; pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2 ((v1 = v2) :: acc)
    | ( { desc = Let (v1, ds1); pos = _pos } :: ll1,
        { desc = Let (v2, ds2); pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2
          (compare_desc [ ds1 ] [ ds2 ] [] @ ((v1 = v2) :: acc))
    | ( { desc = Fun (v1, ds1); pos = _pos } :: ll1,
        { desc = Fun (v2, ds2); pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2
          (compare_desc [ ds1 ] [ ds2 ] [] @ ((v1 = v2) :: acc))
    | ( { desc = AnFun (s1, t1, ds1); pos = _pos } :: ll1,
        { desc = AnFun (s2, t2, ds2); pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2
          (compare_desc [ ds1 ] [ ds2 ] [] @ (((s1, t1) = (s2, t2)) :: acc))
    | ( { desc = Block dsl1; pos = _pos } :: ll1,
        { desc = Block dsl2; pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2 (compare_desc dsl1 dsl2 [] @ acc)
    | ( { desc = Op (e11, op1, e12); pos = _pos } :: ll1,
        { desc = Op (e21, op2, e22); pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2
          (compare_desc [ e11 ] [ e21 ] []
          @ compare_desc [ e12 ] [ e22 ] []
          @ ((op1 = op2) :: acc))
    | ( { desc = Apply (s1, dsl1); pos = _pos } :: ll1,
        { desc = Apply (s2, dsl2); pos = _pos2 } :: ll2 ) ->
        compare_desc ll1 ll2 (compare_desc dsl1 dsl2 [] @ ((s1 = s2) :: acc))
    | _ :: ll1, _ :: ll2 -> compare_desc ll1 ll2 (false :: acc)
    | [], [] -> acc
    | _ -> assert false

  let str_of_typ = function TTyp (Some s) -> s | TTyp None -> ""

  let op_to_str = function
    | Add -> "+"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Sub -> "-"

  let rec to_str : stmt list -> string = function
    | { desc = Var s; pos = _pos } :: ll -> Fmt.str "Variable: %s" s ^ to_str ll
    | { desc = Const (VInt v); pos = _pos } :: ll ->
        Fmt.str "Const: int - %d" (Int64.to_int v) ^ to_str ll
    | { desc = Const (VInt32 v); pos = _pos } :: ll ->
        Fmt.str "Const: int32 - %d" (Int32.to_int v) ^ to_str ll
    | { desc = Const (VString v); pos = _pos } :: ll ->
        Fmt.str "Const: string - %s" v ^ to_str ll
    | { desc = Const (VBool v); pos = _pos } :: ll ->
        Fmt.str "Const: bool - %b" v ^ to_str ll
    | { desc = Let ((s, t), ds); pos = _pos } :: ll ->
        Fmt.str "Let %s (typ %s) = " s (str_of_typ t)
        ^ to_str [ ds ] ^ ";\n" ^ to_str ll
    | { desc = Fun ((s, t), ds); pos = _pos } :: ll ->
        Fmt.str "Fun (param: %s (typ %s)) = " s (str_of_typ t)
        ^ to_str [ ds ] ^ to_str ll
    | { desc = AnFun (s, t, ds); pos = _pos } :: ll ->
        Fmt.str "AnFun (param: %s (typ %s)) = " s (str_of_typ t)
        ^ to_str [ ds ] ^ to_str ll
    | { desc = Block dsl; pos = _pos } :: ll ->
        Fmt.str "Block {\n" ^ to_str dsl ^ "};" ^ to_str ll
    | { desc = Op (e1, op, e2); pos = _pos } :: ll ->
        to_str [ e1 ] ^ op_to_str op ^ to_str [ e2 ] ^ to_str ll
    | [] -> ""
    | _ -> assert false
end

module TypedAstTest = struct
  open Ast.TypedAst

  let to_desc stmt = stmt.desc

  let rec str_of_typ = function
    | TInt -> "int"
    | TString -> "string"
    | TBool -> "bool"
    | TSeq (s1, s2) -> str_of_typ s1 ^ " -> " ^ str_of_typ s2
    | TInt32 -> "int32"
    | TCustom s -> s
    | TVar { def = None; _ } -> "None"
    | TVar { def = Some t; _ } -> str_of_typ t

  let get_typ = function
    | VInt _ -> TInt
    | VInt32 _ -> TInt32
    | VString _ -> TString
    | VBool _ -> TBool

  let str_of_op = function
    | Add -> "+"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Sub -> "-"

  let str_of_val = function
    | VInt i -> Fmt.str "%d" (Int64.to_int i)
    | VInt32 i -> Fmt.str "%d" (Int32.to_int i)
    | VString s -> s
    | VBool b -> Fmt.str "%b" b

  let rec to_str = function
    | Var s :: ll -> Fmt.str "[Variable: %s]" s ^ to_str ll
    | Const v :: ll ->
        Fmt.str "(Const: %s = %s) " (str_of_typ (get_typ v)) (str_of_val v)
        ^ to_str ll
    | Let (s, ds) :: ll ->
        Fmt.str "Let %s = " s ^ to_str [ ds.desc ] ^ ";\n" ^ to_str ll
    | Fun (s, ds) :: ll ->
        Fmt.str "[Fun (param: %s) = " s ^ to_str [ ds.desc ] ^ "]" ^ to_str ll
    | AnFun (s, ds) :: ll ->
        Fmt.str "[AnFun (param: %s) = " s ^ to_str [ ds.desc ] ^ "]" ^ to_str ll
    | Block dsl :: ll ->
        Fmt.str "Block {\n" ^ to_str (List.map to_desc dsl) ^ "};\n" ^ to_str ll
    | Op (ds1, op, ds2) :: ll ->
        Fmt.str "[Op: " ^ to_str [ ds1.desc ] ^ " " ^ str_of_op op ^ " "
        ^ to_str [ ds2.desc ] ^ "]" ^ to_str ll
    | Apply (s, dsl) :: ll ->
        Fmt.str "[Aplying " ^ s ^ " with "
        ^ to_str (List.map to_desc dsl)
        ^ "]" ^ to_str ll
    | [] -> ""
    | _ :: ll -> to_str ll

  let rec to_str_stmt = function
    | { desc = ss; typ } :: l ->
        str_of_typ typ ^ "\n" ^ to_str [ ss ] ^ "\n" ^ to_str_stmt l ^ "\n"
    | [] -> ""

  let rec cmp_typs a b =
    match (a, b) with
    | TVar { def = None; _ }, TVar { def = None; _ } -> true
    | TVar { def = Some t1; _ }, TVar { def = Some t2; _ } -> cmp_typs t1 t2
    | TSeq (t11, t12), TSeq (t21, t22) -> cmp_typs t11 t21 && cmp_typs t12 t22
    | TVar { def = Some t1; _ }, t2 -> cmp_typs t1 t2
    | t1, TVar { def = Some t2; _ } -> cmp_typs t1 t2
    | t1, t2 -> t1 = t2

  let rec cmp_desc a b =
    match (a, b) with
    | { desc = Let (s1, ds1); typ = t1 }, { desc = Let (s2, ds2); typ = t2 } ->
        let r1 = cmp_desc ds1 ds2 in
        let r2 = cmp_typs t1 t2 in
        r1 && r2 && s1 = s2
    | { desc = Fun (s1, ds1); typ = t1 }, { desc = Fun (s2, ds2); typ = t2 } ->
        let r1 = cmp_desc ds1 ds2 in
        let r2 = cmp_typs t1 t2 in
        r1 && r2 && s1 = s2
    | { desc = AnFun (s1, ds1); typ = t1 }, { desc = AnFun (s2, ds2); typ = t2 }
      ->
        cmp_desc ds1 ds2 && s1 = s2 && cmp_typs t1 t2
    | ( { desc = Op (ds11, op1, ds12); typ = t1 },
        { desc = Op (ds21, op2, ds22); typ = t2 } ) ->
        op1 = op2 && cmp_desc ds11 ds21 && cmp_desc ds12 ds22 && cmp_typs t1 t2
    | ( { desc = Apply (s1, dsl1); typ = t1 },
        { desc = Apply (s2, dsl2); typ = t2 } ) ->
        cmp_typs t1 t2 && s1 = s2
        && List.fold_left
             (fun a (b, c) -> cmp_desc b c && a)
             true
             (List.map2 (fun a b -> (a, b)) dsl1 dsl2)
    | { desc = Const _ as ds1; typ = t1 }, { desc = Const _ as ds2; typ = t2 }
    | { desc = Var _ as ds1; typ = t1 }, { desc = Var _ as ds2; typ = t2 } ->
        ds1 = ds2 && cmp_typs t1 t2
    | { desc = ds1; typ = t1 }, { desc = ds2; typ = t2 } ->
        cmp_typs t1 t2 && ds1 = ds2

  let cmp_stmt = cmp_desc
end

let pp_ast ppf v = Fmt.pf ppf "\n%s\n" (AstAstTest.to_str v)
let pp_typed_stmt ppf v = Fmt.pf ppf "\n%s\n" (TypedAstTest.to_str_stmt v)

open Alcotest

let comp_astcode exp resul =
  AstAstTest.compare_desc exp resul []
  |> List.filter (fun a -> a = false)
  |> List.length = 0

let astastcode = testable pp_ast comp_astcode

let typedaststmt =
  testable pp_typed_stmt (fun al bl ->
      List.map2 TypedAstTest.cmp_stmt al bl
      |> List.filter (fun a -> a = false)
      |> List.length = 0)

let test_astast (s : string) (ast_expt : Ast.Ast.code) (ast_val : Ast.Ast.code)
    () =
  Alcotest.(check astastcode) s ast_expt ast_val

let test_typedast_code (s : string) (ast_expt : Ast.TypedAst.code)
    (ast_val : Ast.TypedAst.code) () =
  Alcotest.(check typedaststmt) s ast_expt ast_val

type test_ast_case = string * Ast.Ast.code * Ast.Ast.code
type test_typed_code_case = string * Ast.TypedAst.code * Ast.TypedAst.code

let run_ast (modu : string) (tests : test_ast_case list) =
  run modu
    [
      ( modu,
        List.map
          (fun (s, vall, expt) -> test_case s `Quick (test_astast s expt vall))
          tests );
    ]

let run_typedast (modu : string) (t1 : string)
    (tests1 : test_typed_code_case list) =
  run modu
    [
      ( t1,
        List.map
          (fun (s, vall, expt) ->
            test_case s `Quick (test_typedast_code s expt vall))
          tests1 );
    ]
