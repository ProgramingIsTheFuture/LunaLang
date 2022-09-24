open Ast

let typ_tbl = Hashtbl.create 1;;

let vtyp_to_str = function
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"

let ftyp_to_str = function
  | FTFun l ->
    List.map (fun t -> vtyp_to_str t) l |>
    String.concat " "

let ttyp_to_str = function
  | TTyp v -> vtyp_to_str v
  | TFTyp f -> ftyp_to_str f
  | TInference -> "inference"

let eval_typ = function
  | Const v ->
    begin
      match v with
      | VInt _ -> TTyp TInt
      | VString _ -> TTyp TString
      | VBool _ -> TTyp TBool
    end
  | Var s ->
    begin
      try
        Hashtbl.find typ_tbl s
      with
      | Not_found -> raise (Error.InvalidVariable (Format.sprintf "Variable: %s is not declared.\n" s))
    end
  | _ -> assert false

let rec check_types = function
  | Let (n, t, e) as l :: ll ->
    begin
      match t with
      | TInference ->
        Let (n, eval_typ e, e) :: check_types ll
      | TTyp _ | TFTyp _ ->
        let etyp = eval_typ e in
        if t <> etyp then
          raise (Error.InvalidType (Format.sprintf "Expected type %s but got %s.\n" (ttyp_to_str t) (ttyp_to_str etyp)))
        else
          l :: check_types ll
    end
  | l :: ll ->
    l :: check_types ll
  | [] -> []
