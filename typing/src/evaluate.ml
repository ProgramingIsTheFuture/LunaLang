open Ast.TypedAst

let tbl = Hashtbl.create 10;;

let add_var n t =
  Hashtbl.add tbl n t;;

let find_var n =
  Hashtbl.find tbl n;;

let eval_typ_desc (position: string) = function
  | Const v ->
    begin match v with
      | VInt _ -> TSeq(TInt, None)
      | VInt32 _ -> TSeq(TInt32, None)
      | VString _ -> TSeq(TString, None)
      | VBool _ -> TSeq(TBool, None)
    end
  | Var s ->
    (* Find the var s *)
    begin
      try
        find_var s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf "%s| Var with name: %s not found" position s))
    end
  | Fun ((s, _), _) ->
    (* find local variable *)
    begin
      try
        find_var s
      with
        Not_found -> raise (Error.InvalidType (Format.sprintf"%s| Fun %s not found" position s))
    end
  | _ -> TSeq(TCustom "", None)

