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
type typ = TTyp of string option

(** [op] are the available operatores
    [Add] +
    [Sub] -
    [Div] /
    [Mul] *
    [Mod] % *)
type op = Add | Sub | Div | Mul | Mod

type pos = { starts : int; line : int; ends : int }
(** [pos] stores the line and the position of the specific desc *)

type stmt = { pos : pos; desc : desc }
(** [stmt] this is a stmt, it's created one for each stmt parsed *)

(** [desc] possible statements to use inside the Dyri language *)
and desc =
  (*  *)
  | Const of value
  | Op of stmt * op * stmt
  | Var of string
  | Apply of (string * stmt list)
  | Let of (string * typ) * stmt
  | Fun of (string * typ) * stmt  (** (a: typ) -> a + 10 *)
  | AnFun of (string * typ * stmt)
  (* need to be implemented *)
  | If
  | For
  | Loop
  | Block of stmt list

type code = stmt list
(** [code] stores all the AST code from a string/file *)
