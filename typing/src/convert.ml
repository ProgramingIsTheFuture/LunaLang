(**
  convert.ml
  Will convert Ast.Ast.* 
  to 
  Ast.TypedAst.*
 *)

open Ast.Ast
open Ast.TypedAst

let rec typ_of_string pos = function
  | "int" -> TInt
  | "int32" -> TInt32
  | "bool" -> TBool
  | "string" -> TString
  | s -> 
    if s = "" then
      raise (Error.InvalidType (pos, "Expected type but got empty"))
    else
    let sl = String.split_on_char ' ' s in
    if List.length sl = 1 then
      TCustom s
    else
      let tips_seq = List.filteri (fun i a -> 
        if i mod 2 <> 0 && a <> "->" then 
          raise (Error.InvalidType (pos, "Invalid type!"))
        else if i mod 2 <> 0 then
          false
        else true
        ) sl
      in
      let rec h = function
        | [tl] -> typ_of_string pos tl
        | typ :: tl -> 
          let t2 = h tl in
          TSeq (typ_of_string pos typ, t2) 
        | [] -> assert false
      in
      h tips_seq
;;


let typ_of_ast (pos) = function
  | TTyp (Some s) ->
    typ_of_string pos s
  | TTyp None -> TGeneric
;;

let typ_of_value = function
  | VInt _ -> TInt 
  | VInt32 _ -> TInt32
  | VBool _ -> TBool
  | VString _ -> TString
;;

let typ_of_desc _pos = function
  | Const v -> typ_of_value v
  | Var _ -> TGeneric
  | _ -> assert false

let value_of_ast: Ast.Ast.value -> Ast.TypedAst.value = function
  | VInt v -> VInt v
  | VInt32 v -> VInt32 v
  | VBool v -> VBool v
  | VString v -> VString v
;;

let op_of_ast: Ast.Ast.op -> Ast.TypedAst.op = function
  | Add -> Add
  | Mod -> Mod
  | Mul -> Mul
  | Div -> Div
  | Sub -> Sub
;;

let rec stmt_of_ast: Ast.Ast.stmt -> Ast.TypedAst.stmt = function
  | { desc = Const _ as v; pos = pos } ->
    let v = desc_of_ast pos v in
    {desc = v; typ = typ_of_desc pos v}
  | { desc = Var _ as v; pos = pos } ->
    let v = desc_of_ast pos v in
    {desc = v; typ = typ_of_desc pos v}
  | _ -> assert false

and desc_of_ast (pos) (ast_desc: Ast.Ast.desc): Ast.TypedAst.desc =
  match ast_desc with
  | Const v ->
    Const (value_of_ast v)
  | Var s -> Var s
  | Let ((s, t), ds) ->
    let t = typ_of_ast pos t in
    let ds = stmt_of_ast ds in
    Let ((s, t), ds)
  | Fun ((s, t), ds) ->
    Fun ((s, typ_of_ast pos t), stmt_of_ast ds)
  | AnFun (s, t, ds) ->
    AnFun (s, typ_of_ast pos t, stmt_of_ast ds)
  | Op (ds1, op, ds2) ->
    Op(stmt_of_ast ds1, op_of_ast op, stmt_of_ast ds2)
  | Apply (s, dsl) ->
    let dsl = List.map stmt_of_ast dsl in
    Apply (s, dsl)
  | _ -> assert false

