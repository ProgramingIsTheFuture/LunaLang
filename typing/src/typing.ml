module Error = Error

open Ast.Ast
open Ast.TypedAst

let tbl = Hashtbl.create 10;;

let _add_fun n t =
  Hashtbl.add tbl ("fun_"^n) t;;

let find_fun n =
  Hashtbl.find tbl ("fun_"^n);;

let add_var n t =
  Hashtbl.add tbl ("var_"^n) t;;

let find_var n =
  Hashtbl.find tbl ("var_"^n);;


let asttyp_typedtyp = function
  | TTyp (Some s) ->
    begin
      match s with
      | "int" -> TInt
      | "bool" -> TBool
      | "string" -> TString
      | s -> TCustom s
    end
  | TTyp None -> TGeneric;;

let to_typed_value: Ast.Ast.value -> Ast.TypedAst.value = function
  | VInt v -> VInt v
  | VInt32 v -> VInt32 v
  | VBool v -> VBool v
  | VString v -> VString v

let op_to_op: Ast.Ast.op -> Ast.TypedAst.op = function
  | Add -> Add
  | Mod -> Mod
  | Mul -> Mul
  | Div -> Div
  | Sub -> Sub

let desc_desc: Ast.Ast.desc -> Ast.TypedAst.desc = function
  | Const v ->
    Const (to_typed_value v)
  | Var s -> Var s
  | _ -> assert false

let eval_typ_desc = function
  | Const v ->
    begin match v with
      | VInt _ -> TInt
      | VInt32 _ -> TInt32
      | VString _ -> TString
      | VBool _ -> TBool
    end
  | Var s ->
    (* Find the var s in this scope *)
    begin
      try
        find_var s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf"Var %s not found" s))
    end
  | Fun ((s, _), _) ->
    begin
      try
        find_fun s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf"Fun %s not found" s))
    end

  | _ -> TCustom "" 

let rec check_types: Ast.Ast.code -> Ast.TypedAst.code = function
  | {desc = Const v; pos = _pos} :: ll ->
    {
      desc = desc_desc (Const v);
      typ = eval_typ_desc (Const (to_typed_value v))
    }
    :: check_types ll
  | {desc = Var _ as v; pos = _pos} :: ll ->
    {
      desc = desc_desc v;
      typ = eval_typ_desc (desc_desc v)
    }
    :: check_types ll
  | {desc = Let ((s, t), ds); pos = pos} :: ll ->
    let t = asttyp_typedtyp t in
    let ds = desc_desc ds in
    let ds_t = eval_typ_desc ds in
    begin
      match t with
      | TGeneric ->
        add_var s ds_t;
        {desc = Let ((s, ds_t), ds); typ = eval_typ_desc (Var s)} :: check_types ll
      | _ as t ->
        if t <> ds_t then
          raise (Error.InvalidType (Format.sprintf "line: %d, character: %d-%d, Invalid type \n" pos.line pos.starts pos.ends))
        else
          let () = add_var s ds_t in
          {desc = Let ((s, ds_t), ds); typ = eval_typ_desc (Var s)} :: check_types ll
    end
  | {desc = Fun((_s, _t), _ds); pos = _pos} :: _ll ->
      assert false
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

    let v1 = desc_desc v1 in
    let v2 = desc_desc v2 in

    let vv1 = check_int v1 in
    let vv2 = check_int v2 in

    if vv1 = true && vv2 = true then
      {desc=Op(v1, op_to_op op, v2); typ=TInt } :: check_types ll
    else
      raise (Error.InvalidType (Format.sprintf "line: %d, character: %d-%d, Invalid type\n" pos.line pos.starts pos.ends))

  | s :: _ ->
    Printf.printf "%d %d %d\n" s.pos.line s.pos.starts s.pos.ends;
    assert false
  | [] -> []
