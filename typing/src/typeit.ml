open Ast.TypedAst

let err_fmt = Error.format_position

let rec out_typ = function
  | TSeq (TInt, Some t) -> Printf.printf "TInt -> "; out_typ t; 
  | TSeq (TInt32, Some t) -> Printf.printf "TInt32 -> "; out_typ t; 
  | TSeq(TString, Some t) -> Printf.printf "TString -> "; out_typ t;
  | TSeq (TBool, Some t) -> Printf.printf "TBool -> "; out_typ t; 
  | TSeq (TInt, None) -> Printf.printf "TInt"
  | TSeq (TInt32, None) -> Printf.printf "TInt32"
  | TSeq(TString, None) -> Printf.printf "TString"
  | TSeq (TBool, None) -> Printf.printf "TBool"
  | _ -> assert false
  ;;

let is_primitive_type = function
  | TSeq (TInt, None)
  | TSeq (TInt32, None)
  | TSeq(TString, None)
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
    match ds_t with
    | TSeq _ as t ->
      if is_any typ' && is_primitive_type t then
        t
      else if is_any typ' && not (is_primitive_type t) then
        tseq_last t
      else if is_primitive_type typ' && is_primitive_type t then
        if tseq_last t = typ' then
          t
        else 
          Error.invalid_type (Format.sprintf "%s| Invalid type\n" (err_fmt pos))
      else if is_primitive_type typ' && not(is_primitive_type t) then
        if tseq_last t = typ' then
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

