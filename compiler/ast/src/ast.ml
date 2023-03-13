(** AST that we want to parse *)

(** [value] are the possible values for the dyri language *)
type value =
  | VInt of int64
  | VInt32 of int32
  | VString of string
  | VBool of bool

(** [typ] are the possible types for the language
 * Examples:
 * let a = 10;
 * let a: int = 10;
 *)
type ut_typ =
  | TTyp of string option
      (** [typ] are the possible types for the language
    After the typing stage every variable
    will have one of these types
    typ ::= 
      | int
      | bool
      | str
      | typ -> typ
      | None
 *)

type typ =
  (* sum: TSeq (int, TSeq (int, TSeq (int, None) ) )
     sum: int -> int -> int
     let sum = (a: int) -> (b: int) -> a + b *)
  | TSeq of typ * typ
  | TInt
  | TInt32
  | TString
  | TBool
  | TCustom of string
  | TVar of tvar

and tvar = { id : int; mutable def : typ option }

(** [op] are the available operatores
    [Add] +
    [Sub] -
    [Div] /
    [Mul] *
    [Mod] % *)
type op = Add | Sub | Div | Mul | Mod

type pos = { starts : int; line : int; ends : int }
(** [pos] stores the line and the position of the specific desc *)

type ut_stmt = { pos : pos; desc : ut_desc }
(** [stmt] this is a stmt, it's created one for each stmt parsed *)

(** [desc] possible statements to use inside the Dyri language *)
and ut_desc =
  (*  *)
  | Const of value
  | Op of ut_stmt * op * ut_stmt
  | Var of string
  | Apply of string * ut_stmt list
  | Let of string * ut_typ * ut_stmt
  | Fun of string * ut_typ * ut_stmt  (** (a: typ) -> a + 10 *)
  | AnFun of string * ut_typ * ut_stmt
  (* need to be implemented *)
  | If
  | For
  | Loop
  | Block of ut_stmt list

type ut_code = ut_stmt list
(** [code] stores all the AST code from a string/file *)

type stmt = { typ : typ; desc : desc }
(** [stmt] this is a stmt, it's created one for each stmt parsed *)

(** [desc] possible statements to use inside the Dyri language *)
and desc =
  (*  *)
  | Const of value
  | Op of stmt * op * stmt
  | Var of string
  | Apply of string * stmt list
  | Let of string * stmt
  | Fun of string * stmt
  | AnFun of string * stmt
  (* need to be implemented *)
  | If
  | For
  | Loop  (** Block have a return typ *)
  | Block of stmt list

type code = stmt list
(** [code] stores all the AST code from a string/file *)

let value_pprint v f =
  match v with
  | VInt v ->
      Format.fprintf f "%s" (Int64.to_string v);
      f
  | VInt32 v ->
      Format.fprintf f "%s" (Int32.to_string v);
      f
  | VBool b ->
      Format.fprintf f "%b" b;
      f
  | VString s ->
      Format.fprintf f "%s" s;
      f

let op_pprint v f =
  match v with
  | Add ->
      Format.fprintf f " + ";
      f
  | Mod ->
      Format.fprintf f " %% ";
      f
  | Div ->
      Format.fprintf f " / ";
      f
  | Sub ->
      Format.fprintf f " - ";
      f
  | Mul ->
      Format.fprintf f " * ";
      f

let rec ut_desc_pprint (v : ut_desc) f =
  match v with
  | Const v -> value_pprint v f
  | Op (st1, op, st2) ->
      ut_stmt_pprint st1 f |> op_pprint op |> ut_stmt_pprint st2
  | Let (s, t, st) ->
      (match t with
      | TTyp None ->
          Format.fprintf f "let %s = " s;
          f
      | TTyp (Some t) ->
          Format.fprintf f "let %s : %s = " s t;
          f)
      |> ut_stmt_pprint st
  | Apply (s, stl) ->
      Format.fprintf f "%s" s;
      List.fold_left
        (fun acc st ->
          Format.fprintf f "(";
          ut_stmt_pprint st acc |> ignore;
          Format.fprintf f ") ";
          f)
        f stl
  | Fun (s, t, st) ->
      (match t with
      | TTyp None ->
          Format.fprintf f "(%s) -> " s;
          f
      | TTyp (Some t) ->
          Format.fprintf f "(%s:%s) -> " s t;
          f)
      |> ut_stmt_pprint st
  | Var s ->
      Format.fprintf f " %s " s;
      f
  | Block stl -> List.fold_left (fun acc st -> ut_stmt_pprint st acc) f stl
  | _ -> assert false

and ut_stmt_pprint (v : ut_stmt) f =
  match v with
  | { desc; pos } ->
      Format.fprintf f "[pos: %d:%d-%d][" pos.line pos.starts pos.ends;
      ut_desc_pprint desc f |> ignore;
      Format.fprintf f "]";
      f

let flush f = Format.fprintf f "@."
