open Ast.TypedAst

let err_fmt = Error.format_position

let rec out_typ = function
  | TSeq (TInt, Some t) -> Printf.printf "TInt -> "; out_typ t; 
  | TSeq (TInt32, Some t) -> Printf.printf "TInt32 -> "; out_typ t; 
  | TSeq(TString, Some t) -> Printf.printf "TString -> "; out_typ t;
  | TSeq (TBool, Some t) -> Printf.printf "TBool -> "; out_typ t; 
  | TSeq (TGeneric, Some t) -> Printf.printf "any -> "; out_typ t;
  | TSeq (TCustom s, Some t) -> Printf.printf "[%s] -> " s; out_typ t;
  | TSeq (TInt, None) -> Printf.printf "TInt"
  | TSeq (TInt32, None) -> Printf.printf "TInt32"
  | TSeq(TString, None) -> Printf.printf "TString"
  | TSeq (TBool, None) -> Printf.printf "TBool"
  | TSeq (TGeneric, None) -> Printf.printf "Any";
  | TSeq (TCustom s, None) -> Printf.printf "[%s]" s;
  ;;

let is_primitive_type = function
  | TSeq (TInt, None)
  | TSeq (TInt32, None)
  | TSeq (TString, None)
  | TSeq (TBool, None) ->
    true
  | _ -> false

let is_any = function
  | TSeq(TGeneric, None) ->
    true
  | _ -> false

let consts c pos = 
  {
    desc = c;
    typ = Evaluate.eval_typ_desc (err_fmt pos) c
  };;

(** Transforms v into a Ast.Typed stmt *)
let vars v pos =
  match v with
  | Var (s) ->
    let t = 
      try 
        Evaluate.find_var s
      with Not_found ->
        Error.invalid_type (Format.sprintf "%s| Variable %s not defined!\n" (err_fmt pos) s)
    in

    {
      desc = v;
      typ = t;
    }
  | _ -> assert false
  ;;

let lets l pos =
  let (name, typ', ds, ds_t) = 
    match l with
    | Let ((name, typ), ds) ->
      let dst =  Evaluate.eval_typ_desc (err_fmt pos) ds in
      (name, typ, ds, dst)
    | _ -> assert false
  in

  let rec tseq_last = function
    | TSeq (_, Some tt) ->
      tseq_last tt
    | TSeq (_, None) as t ->
      t
  in

  let typ =
    (* 
     * Cases:
     *        typ = ds_t
     * let a: int = (int...);
     * let a: int -> int = (int -> int...);
     * let a = (int...);
     *
     * int -> int
     * let a (b:int) : int= (int...);
     *
     * if ds_t is primitive type (single type) 
        and typ is primitive then
          they must be equals

      if typ is not primitive than
        ds_t must not be primitive
        and the must be equals

      if typ is primitive and ds_t is not 
        then the last ds_t_typ from ds_t 
        myst be equal to typ
     **)
    let t = ds_t in
    if is_any typ' then
      t
    else if is_primitive_type typ' && is_primitive_type t then
      if t = typ' then
        t
      else
        Error.invalid_type (Format.sprintf "%s| Invalid type\n" (err_fmt pos))
    else if is_primitive_type typ' && not(is_primitive_type t) then
      if (tseq_last t) = typ' then
        t
      else
        Error.invalid_type (Format.sprintf "%s| Invalid type\n" (err_fmt pos))
    else
      Error.invalid_type (Format.sprintf "%s| Invalid type\n" (err_fmt pos))
  in

  Evaluate.add_var name typ; 

  {
    desc = Let((name, typ), ds);
    typ = typ;
  };;

let ops op pos =
  match op with
  | Op(ds1, op', ds2) ->
    let t1 = Evaluate.eval_typ_desc (err_fmt pos) ds1 in
    let t2 = Evaluate.eval_typ_desc (err_fmt pos) ds2 in
    if t1 = t2 then
      {
        desc = Op(ds1, op', ds2);
        typ = t1;
      }
    else
      Error.invalid_type (Format.sprintf "%s| Operator with different types\n" (err_fmt pos))
  | _ -> assert false
;;

let funs fn pos =
  Printf.printf "Funs\n\n";
  let (s, t,ds_t) = 
    match fn with
    | Fun ((s, tt), ds) ->
      let ds_t = Evaluate.eval_typ_desc (err_fmt pos) ds in
      let t =
        match tt with 
        | TSeq (t, None) -> t 
        | _ -> assert false 
      in
      (s, t, ds_t)
    | _ -> assert false
  in

  Printf.printf "Fun tips";
  out_typ (TSeq(t, None));
  out_typ ds_t; 
  Printf.printf "\n";

  Evaluate.add_var s (TSeq(t, None));

  {
    desc = fn;
    typ = TSeq (t, Some ds_t);
  }

;;

(*
  let a b =
    b + 10;

  a TSeq (TInt, Some (TSeq (TInt, None)))
  a 10;
 *)
let apply ap pos =
  let (s, dsl) = 
    match ap with
    | Apply (s, dsl) ->
      (s, dsl)
    | _ -> assert false
  in

  let t = Evaluate.find_var s in

  let params =
    dsl |> List.map (Evaluate.eval_typ_desc (err_fmt pos)) in

  let rec ft = function
    | (TSeq (_, None) as t, []) ->
      t
    | (t, []) ->
      t
    | (TSeq (t, Some tsl), l :: ll) ->
      if TSeq(t, None) <> l then
        Error.invalid_type (Format.sprintf "%s| Expected one type but got another\n" (err_fmt pos))
      else
        ft (tsl, ll)
    | (TSeq (_, None), _::_) ->
      Error.invalid_type (Format.sprintf "%s| Expected few arguments\n" (err_fmt pos))
  in

  {
    desc = ap;
    typ = ft (t, params);
  }
;;

