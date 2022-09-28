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

let vtyp_to_str = function
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"

let ftyp_to_str = function
  | FTFun l ->
    List.map (fun t -> vtyp_to_str t) l |>
    String.concat " -> "

let ttyp_to_str = function
  | TTyp v -> vtyp_to_str v
  | TFTyp f -> ftyp_to_str f
  | TInference -> "inference"

let ttyp_to_str_l l =
  List.map ttyp_to_str l |> String.concat " "


let param_typ = function
  | PTyp (_, t) -> t
  | PName _ -> TInference

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
        find_var s
      with
      | Not_found -> raise (Error.InvalidType (Format.sprintf "Variable: %s is not declared.\n" s))
    end
  | _ -> assert false

let rec check_types = function
  | Let (n, t, e) as l :: ll ->
    begin
      match t with
      | TInference ->
        (* If the type t if Inference *)
        (* then it must be equal to the type of e  *)
        let t = eval_typ e in
        add_var n t;
        Let (n, t, e) :: check_types ll
      | TTyp _ | TFTyp _ ->
        (* If the value is ftyp or ttyp *)
        (* we check the e type and compare it to the expr type *)
        let etyp = eval_typ e in
        if t = etyp then
          let () = add_var n t in
          l :: check_types ll
        else
          raise (Error.InvalidType (Format.sprintf "Expected type %s but got %s.\n" (ttyp_to_str t) (ttyp_to_str etyp)))
    end
  | Fun (n, t, p, e) as l :: ll ->
    (* let sum (a: int): int -> int = a; *)
    (* let sum a = a; *)
    begin
      (* Checking the type of t *)
      match t with
      | TFTyp FTFun tl ->
        (* Compare the size of inputs and the type of n *)
        if List.length tl - 1 <> List.length p then
          raise (Error.InvalidType (Format.sprintf "Expected type %s but got %s.\n"
                                      (FTFun tl |> ftyp_to_str) (List.map param_typ p |> ttyp_to_str_l) ));

        let params = List.mapi (fun i param ->
            match param with
            | PName (s) ->
              let t = TTyp (List.nth tl i) in
              add_var s t;
              (PTyp (s, t), s)
            | PTyp (s, pt) ->
              let expt = TTyp (List.nth tl i) in
              if expt  <> pt then
                raise (Error.InvalidType (Format.sprintf "Expected type %s but got %s.\n" (ttyp_to_str expt) (ttyp_to_str pt)))
              else
                let () = add_var s pt in
                (PTyp (s, pt), s)
          ) p in

        let te = eval_typ e in
        let rec last_t = function
          | tt :: [] -> tt
          | _ :: tl ->
            last_t tl
          | [] -> raise (Error.InvalidType "Should never enter here!") in
        let last = last_t tl in
        if TTyp last <> te then
          raise (Error.InvalidType (Format.sprintf "Expected type %s but got %s.\n" (vtyp_to_str last) (ttyp_to_str te)));

        let params = List.map (fun (a, s) -> remove_var s; a) params in

        add_fun n t;
        Fun (n, t, params, e) :: check_types ll
      | TInference ->
        (* TODO *)
        l :: check_types ll
      | _ ->
        raise (Error.InvalidType (Format.sprintf "Expected type but got .\n"))
    end
  | Apply (s, e) :: ll ->
    let t = find_fun s in
    let t =
      match t with
      | TFTyp FTFun f ->
        f
      | _ ->
        raise (Error.InvalidType (Format.sprintf "Expected type but got .\n")) in
    if List.length e > (List.length t - 1) then
      raise (Error.InvalidType (Format.sprintf"Expected type %s but got %s.\n" (FTFun t |> ftyp_to_str) (List.map eval_typ e |> ttyp_to_str_l) ));
    List.mapi (fun i a ->
        let te = eval_typ a in
        let t =
          TTyp (List.nth t i) in
        if te <> t then
          raise (Error.InvalidType (Format.sprintf"Expected type %s but got %s.\n" (ttyp_to_str t) (ttyp_to_str te) ));
        a
      ) e |> ignore;
    check_types ll
  | l :: ll ->
    l :: check_types ll
  | [] -> []
