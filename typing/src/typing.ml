module Error = Error

open Convert
open Ast.Ast
open Ast.TypedAst

let tbl = Hashtbl.create 10;;

let add_var n t =
  Hashtbl.add tbl ("var_"^n) t;;

let find_var n =
  Hashtbl.find tbl ("var_"^n);;

let eval_typ_desc (position: string) = function
  | Const v ->
    begin match v with
      | VInt _ -> TInt
      | VInt32 _ -> TInt32
      | VString _ -> TString
      | VBool _ -> TBool
    end
  | Var s ->
    (* Find the var s *)
    begin
      try
        find_var s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf "%s| Var %s not found" position s))
    end
  | Fun ((s, _), _) ->
    (* find local variable *)
    begin
      try
        find_var s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf"%s| Fun %s not found" position s))
    end
  | _ -> TCustom "" 

let rec check_types: Ast.Ast.code -> Ast.TypedAst.code = function
  | {desc = Const v; pos = pos} :: ll ->
    (* Checking the const type *)
    {
      desc = desc_of_ast (Error.format_position pos) (Const v);
      typ = eval_typ_desc (Error.format_position pos) (Const (value_of_ast v))
    }
    :: check_types ll
  | {desc = Var _ as v; pos = pos} :: ll ->
    (* Variable already declared *)
    {
      desc = desc_of_ast (Error.format_position pos) v;
      typ = eval_typ_desc (Error.format_position pos) (desc_of_ast (Error.format_position pos) v)
    }
    :: check_types ll
  | {desc = Let ((s, t), ds); pos = pos} :: ll ->
    (* Declaring a variable *)
    (* Typ if it has one *)
    let t = typ_of_ast (Error.format_position pos) t in
    (* desc *)
    let ds = desc_of_ast (Error.format_position pos) ds in
    (* desc typ *)
    let ds_t = eval_typ_desc (Error.format_position pos) ds in
    begin
      match t with
      | TGeneric ->
        (* if t is a generic typ (No type) *)
        (* it will acept any type*)
        add_var s ds_t;
        {desc = Let ((s, ds_t), ds); typ = eval_typ_desc (Error.format_position pos) (Var s)} :: check_types ll
      | _ as t ->
        if t <> ds_t then
          raise (Error.InvalidType (Format.sprintf "line: %d, character: %d-%d, Invalid type \n" pos.line pos.starts pos.ends))
        else
          (* If T is a valid type and the same as ds_t *)
          let () = add_var s ds_t in
          {desc = Let ((s, ds_t), ds); typ = eval_typ_desc (Error.format_position pos) (Var s)} :: check_types ll
    end
  | {desc = Fun((s, t), ds); pos = pos} :: ll ->
    let t = typ_of_ast (Error.format_position pos) t in
    let ds = desc_of_ast (Error.format_position pos) ds in
    let ds_t = eval_typ_desc (Error.format_position pos) ds in

    begin
      match t with
      | TGeneric ->
        (* This is work, the parameter can be different from the
           desc inside the fun
         *)
        add_var s (TSeq (ds_t, Some (eval_typ_desc (Error.format_position pos) (Var s))));
        {
          desc = Fun ((s, ds_t), ds); 
          typ = TSeq (ds_t, Some (eval_typ_desc (Error.format_position pos) (Var s)))
        }
        :: check_types ll
      | _ as t ->
        if t <> ds_t then
          raise (Error.InvalidType (Format.sprintf "%s| Invalid type\n" (Error.format_position pos)))
        else
          let () = add_var 
            s 
            (TSeq (ds_t, Some (eval_typ_desc (Error.format_position pos) (Var s)))) in
          {
            desc = Fun ((s, ds_t), ds); 
            typ = TSeq (ds_t, Some (eval_typ_desc (Error.format_position pos) (Var s)))
          } 
          :: check_types ll
    end

  | {desc = Op (v1, op, v2); pos = pos} :: ll ->

    let check_int = function
      | Const (VInt _) ->
        true
      | Var s ->
        find_var s = TInt
      | _ ->
        raise 
          (Error.InvalidType (Format.sprintf "line: %d, character: %d-%d, Invalid type\n" pos.line pos.starts pos.ends)) 
    in

    let v1 = desc_of_ast (Error.format_position pos) v1 in
    let v2 = desc_of_ast (Error.format_position pos) v2 in

    let vv1 = check_int v1 in
    let vv2 = check_int v2 in

    if vv1 = true && vv2 = true then
      {desc=Op(v1, op_of_ast op, v2); typ=TInt } :: check_types ll
    else
      raise (Error.InvalidType (Format.sprintf "line: %d, character: %d-%d, Invalid type\n" pos.line pos.starts pos.ends))

  | {desc = Apply (s, dsl); pos = pos } :: ll ->
    (*  *)
    let fn = find_var s in
    let rec apply_l = function
      | (TSeq (s, Some next), l :: []) ->
        if l = s then
          next
        else
          raise (Error.InvalidType (Format.sprintf "%s| Invalid param" (Error.format_position pos)))
      | (TSeq (s, Some next), l :: tl) ->
        if l = s then
          apply_l (next, tl)
        else 
          raise (Error.InvalidType (Format.sprintf "%s| Invalid param" (Error.format_position pos)))
      | _ -> assert false
    in
    let dsl = List.map (desc_of_ast (Error.format_position pos)) dsl in
    let ft = apply_l (fn, (List.map (eval_typ_desc (Error.format_position pos)) dsl)) in
    {
      desc = Apply (s, dsl);
      typ = ft
    }
    :: check_types ll

  | s :: _ ->
    Printf.printf "%s \n" (Error.format_position s.pos);
    assert false
  | [] -> []
