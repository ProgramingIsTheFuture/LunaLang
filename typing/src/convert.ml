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

let rec tseq_in t dsl =
  match dsl with
  | TSeq (_, next) ->
    tseq_in t next
  | v -> v = t

let rec apply_args fn dl pos =
  match fn, dl with
  | TSeq (v1, ft), [v2] ->
    if v1 = v2 then
      ft
    else 
      raise (Error.InvalidType 
        (pos, 
        "Expected type: " 
        ^ Debug.string_of_typ v1 
        ^ " but got: " 
        ^ Debug.string_of_typ v2))
  | TSeq(v1, tl1), v2 :: tl2 ->
    if v1 = v2 then
      apply_args tl1 tl2 pos
    else
      raise (Error.InvalidType 
      (pos, 
      "Expected type: " 
      ^ Debug.string_of_typ v1 
      ^ " but got: " 
      ^ Debug.string_of_typ v2)) 
  | _ -> 
    raise (Error.InvalidType 
      (pos, 
      "Expected type: " 
      ^ Debug.string_of_typ fn))

let rec typ_of_desc ctx pos = function
  | Const v -> typ_of_value v
  | Var s ->
    Hashtbl.find ctx s
  | Let ((_, t), ds) ->
    let dst = typ_of_desc ctx pos ds.desc in
    if t = TGeneric then
      dst
    else if t = dst then
      dst
    else raise (Error.InvalidType (pos, "Expected type " ^ (Debug.string_of_typ t) ^ " but got " ^ (Debug.string_of_typ dst)))
  | Op _ ->
    (* This must work with strings, floats, ints *)
    TInt
  | Fun ((_, t), ds) ->
    TSeq(t, typ_of_desc ctx pos ds.desc)
  | AnFun (_, t, ds) ->
    TSeq(t, typ_of_desc ctx pos ds.desc)
  | Apply (s, dl) ->
    let fn = Hashtbl.find ctx s in
    let dlt = List.map (fun a -> a.typ) dl in
    apply_args fn dlt pos
  | _ -> assert false;;

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

let rec stmt_of_ast (ctx: (string, Ast.TypedAst.typ) Hashtbl.t) : Ast.Ast.stmt -> Ast.TypedAst.stmt = function
  | { desc = _ as v; pos = pos } ->
    let v = desc_of_ast ctx pos v in
    let t = typ_of_desc ctx pos v in
    { desc = v; typ = t }

and desc_of_ast ctx (pos) (ast_desc: Ast.Ast.desc): Ast.TypedAst.desc =
  match ast_desc with
  | Const v ->
    Const (value_of_ast v)
  | Var s -> Var s
  | Let ((s, t), ds) ->
    let t = typ_of_ast pos t in
    let ds = stmt_of_ast ctx ds in
    let dst = typ_of_desc ctx pos ds.desc in
    let ft = if t = TGeneric then 
        dst 
      else if t = dst then 
        dst 
      else if tseq_in t dst then
        dst
      else
        raise (
          Error.InvalidType 
            (pos, 
            "Expected type: " ^ Debug.string_of_typ t ^ " but got: " ^ Debug.string_of_typ dst
            ))  in
    Hashtbl.add ctx s ft;
    Let ((s, ft), ds)
  | Fun ((s, t), ds) ->
    let new_ctx = Hashtbl.copy ctx in
    let t = typ_of_ast pos t in
    Hashtbl.add new_ctx s t;
    let ds = stmt_of_ast new_ctx ds in
    Fun ((s, t), ds)
  | AnFun (s, t, ds) ->
    let new_ctx = Hashtbl.copy ctx in
    let t = typ_of_ast pos t in
    Hashtbl.add new_ctx s t;
    let ds = stmt_of_ast new_ctx ds in
    AnFun (s, t, ds)
  | Op (ds1, op, ds2) ->
    Op(stmt_of_ast ctx ds1, op_of_ast op, stmt_of_ast ctx ds2)
  | Apply (s, dsl) ->
    let dsl = List.map (stmt_of_ast ctx) dsl in
    Apply (s, dsl)
  | _ -> assert false

