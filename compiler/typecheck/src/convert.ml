(** convert.ml Will convert Ast.Ast.* to Ast.TypedAst.* *)

open Ast

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
  | TTyp None -> TVar (Tvar.V.create ())

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

let cmp_typs t1 t2 =
  match (Tvar.head t1, Tvar.head t2) with
  | TVar { def = None; _ }, TVar { def = None; _ } -> Tvar.head t1
  | TVar ({ def = None; _ } as t), t2 ->
      t.def <- Some t2;
      t2
  | t1, TVar ({ def = None; _ } as t) ->
      t.def <- Some t1;
      t1
  | t1, t2 when t1 = t2 -> t1
  | t1, (TSeq (t21, _) as t) when t1 = t21 -> t
  | t1, t2 ->
      failwith
        (Format.sprintf "Expected %s but got %s" (Debug.string_of_typ t1)
           (Debug.string_of_typ t2))

let typ_of_desc ctx _pos : Ast.desc -> Ast.typ = function
  | Const v -> typ_of_value v
  | Var s -> Env.find s ctx
  | Let (x, ds) ->
      let t = Env.find x ctx in
      let dst = ds.typ in
      cmp_typs t dst
  (*     if t = dst then dst *)
  (*     else *)
  (*       raise *)
  (*         (Error.InvalidType *)
  (*            ( pos, *)
  (*              "Expected type " ^ Debug.string_of_typ t ^ " but got " *)
  (*              ^ Debug.string_of_typ dst )) *)
  | Op _ ->
      (* This must work with strings, floats, ints *)
      TInt
  | Fun (x, ds) ->
      let t = Env.find x ctx in
      TSeq (t, ds.typ)
  | AnFun (x, ds) ->
      let t = Env.find x ctx in
      TSeq (t, ds.typ)
  | Apply (s, dl) ->
      let fn = Env.find s ctx in
      let dlt = List.map (fun a -> a.typ) dl in
      let t =
        List.fold_left
          (fun a b ->
            Tvar.unify a b;
            match a with TSeq (_, t2) -> t2 | _ -> assert false)
          fn dlt
      in
      t
  | _ -> assert false
