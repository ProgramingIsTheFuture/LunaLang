(** convert.ml Will convert Ast.Ast.* to Ast.TypedAst.* *)

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
        if List.length sl = 1 then TCustom s
        else
          let tips_seq =
            List.filteri
              (fun i a ->
                if i mod 2 <> 0 && a <> "->" then
                  raise (Error.InvalidType (pos, "Invalid type!"))
                else if i mod 2 <> 0 then false
                else true)
              sl
          in
          let rec h = function
            | [ tl ] -> typ_of_string pos tl
            | typ :: tl ->
                let t2 = h tl in
                TSeq (typ_of_string pos typ, t2)
            | [] -> assert false
          in
          h tips_seq

let typ_of_ast pos = function
  | TTyp (Some s) -> typ_of_string pos s
  | TTyp None -> TGeneric

let typ_of_value = function
  | VInt _ -> TInt
  | VInt32 _ -> TInt32
  | VBool _ -> TBool
  | VString _ -> TString

let rec tseq_in t dsl =
  match dsl with TSeq (_, next) -> tseq_in t next | v -> v = t

let rec apply_args fn dl pos =
  match (fn, dl) with
  | TSeq (v1, ft), [ v2 ] ->
      if v1 = v2 then ft
      else
        raise
          (Error.InvalidType
             ( pos,
               "Expected type: " ^ Debug.string_of_typ v1 ^ " but got: "
               ^ Debug.string_of_typ v2 ))
  | TSeq (v1, tl1), v2 :: tl2 ->
      if v1 = v2 then apply_args tl1 tl2 pos
      else
        raise
          (Error.InvalidType
             ( pos,
               "Expected type: " ^ Debug.string_of_typ v1 ^ " but got: "
               ^ Debug.string_of_typ v2 ))
  | _ ->
      raise
        (Error.InvalidType (pos, "Expected type: " ^ Debug.string_of_typ fn))

let rec typ_of_desc ctx pos = function
  | Const v -> typ_of_value v
  | Var s -> Env.Env.find s ctx
  | Let ((_, t), ds) ->
      let dst = typ_of_desc ctx pos ds.desc in
      if t = TGeneric then dst
      else if t = dst then dst
      else
        raise
          (Error.InvalidType
             ( pos,
               "Expected type " ^ Debug.string_of_typ t ^ " but got "
               ^ Debug.string_of_typ dst ))
  | Op _ ->
      (* This must work with strings, floats, ints *)
      TInt
  | Fun ((_, t), ds) -> TSeq (t, typ_of_desc ctx pos ds.desc)
  | AnFun (_, t, ds) -> TSeq (t, typ_of_desc ctx pos ds.desc)
  | Apply (s, dl) ->
      let fn = Env.Env.find s ctx in
      let dlt = List.map (fun a -> a.typ) dl in
      apply_args fn dlt pos
  | _ -> assert false

let value_of_ast : Ast.Ast.value -> Ast.TypedAst.value = function
  | VInt v -> VInt v
  | VInt32 v -> VInt32 v
  | VBool v -> VBool v
  | VString v -> VString v

let op_of_ast : Ast.Ast.op -> Ast.TypedAst.op = function
  | Add -> Add
  | Mod -> Mod
  | Mul -> Mul
  | Div -> Div
  | Sub -> Sub

let rec stmt_of_ast (ctx : Env.env) :
    Ast.Ast.stmt -> Ast.TypedAst.stmt * Env.env option = function
  | { desc = _ as v; pos } ->
      let v, ctx' = desc_of_ast ctx pos v in
      let t = typ_of_desc ctx pos v in
      ({ desc = v; typ = t }, ctx')

and desc_of_ast ctx pos (ast_desc : Ast.Ast.desc) :
    Ast.TypedAst.desc * Env.env option =
  match ast_desc with
  | Const v -> (Const (value_of_ast v), None)
  | Var s -> (Var s, None)
  | Let ((s, t), ds) ->
      let t = typ_of_ast pos t in
      let ds, ctx' = stmt_of_ast ctx ds in
      let ctx' = if Option.is_some ctx' then Option.get ctx' else ctx in
      let dst = typ_of_desc ctx' pos ds.desc in
      let ft =
        if t = TGeneric then dst
        else if t = dst then dst
        else if tseq_in t dst then dst
        else
          raise
            (Error.InvalidType
               ( pos,
                 "Expected type: " ^ Debug.string_of_typ t ^ " but got: "
                 ^ Debug.string_of_typ dst ))
      in
      let ctx = Env.Env.add s ft ctx in
      (Let ((s, ft), ds), Some ctx)
  | Fun ((s, t), ds) ->
      let t = typ_of_ast pos t in
      let ds, _ = stmt_of_ast (Env.Env.add s t ctx) ds in
      (Fun ((s, t), ds), None)
  | AnFun (s, t, ds) ->
      let t = typ_of_ast pos t in
      let ds, _ = stmt_of_ast (Env.Env.add s t ctx) ds in
      (AnFun (s, t, ds), None)
  | Op (ds1, op, ds2) ->
      let ds1, _ = stmt_of_ast ctx ds1 in
      let op = op_of_ast op in
      let ds2, _ = stmt_of_ast ctx ds2 in
      (Op (ds1, op, ds2), None)
  | Apply (s, dsl) ->
      let dsl =
        List.map
          (fun a ->
            let d, _ = stmt_of_ast ctx a in
            d)
          dsl
      in
      (Apply (s, dsl), None)
  | _ -> assert false
