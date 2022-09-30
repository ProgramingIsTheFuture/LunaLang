open Ast.Ast
module Error = Error

let typ_tbl = Hashtbl.create 1;;

let add_var n t =
  Hashtbl.add typ_tbl (n^"_var") t;;

let add_fun n t =
  Hashtbl.add typ_tbl (n^"_fun") t;;

let find_var n =
  Hashtbl.find typ_tbl (n^"_var");;

let remove_var n =
  Hashtbl.remove typ_tbl (n^"_var");;

let find_fun n =
  Hashtbl.find typ_tbl (n^"_fun");;

let param_typ = function
  | PTyp (_, t) -> t
  | PName _ -> TInference

let eval_typ = function
  | Const v ->
    begin
      match v with
      | VInt _ -> TTyp "int"
      | VString _ -> TTyp "string"
      | VBool _ -> TTyp "bool"
    end
  | Var s ->
    begin
      try
        find_var s
      with
      | Not_found -> raise (Error.InvalidType (Format.sprintf "Variable: %s is not declared.\n" s))
    end
  | _ -> assert false

let rec check_types = function
  | _ :: _ ->
    assert false
  | [] -> []
