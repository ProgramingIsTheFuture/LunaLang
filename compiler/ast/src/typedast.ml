(** AST with types *)

(** [value] are the possible values for the dyri language *)
type value =
  | VInt of int64
  | VInt32 of int32
  | VString of string
  | VBool of bool

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

type stmt = { typ : typ; desc : desc }
(** [stmt] this is a stmt, it's created one for each stmt parsed *)

(** [desc] possible statements to use inside the Dyri language *)
and desc =
  (*  *)
  | Const of value
  | Op of stmt * op * stmt
  | Var of string
  | Apply of (string * stmt list)
  | Let of string * stmt
  | Fun of string * stmt
  | AnFun of (string * stmt)
  (* need to be implemented *)
  | If
  | For
  | Loop  (** Block have a return typ *)
  | Block of stmt list

type code = stmt list
(** [code] stores all the AST code from a string/file *)
