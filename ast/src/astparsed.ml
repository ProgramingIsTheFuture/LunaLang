(** AST that we want to parse *)

(** [value] are the possible values for the dyri language *)
type value =
  | VInt of int64
  | VInt32 of int32
  | VString of string
  | VBool of bool
;;

(** [typ] are the possible types for the language
 * Examples:
 * let a = 10;
 * let a: int = 10;
 *)
type typ =
  | TTyp of string option
;;

(** [op] are the available operatores
    [Add] +
    [Sub] -
    [Div] /
    [Mul] *
    [Mod] % *)
type op =
  | Add
  | Sub
  | Div
  | Mul
  | Mod;;

(** [desc] possible statements to use inside the Dyri language *)
type desc =
  (*  *)
  | Const of value
  | Op of desc * op * desc
  | Var of string
  | Apply of (string * desc list)
  | Let of (string * typ) * desc
  | Fun of (string * typ) * desc
  (** a -> a + 10 *)
  | AnFun of (string * desc)
  (* need to be implemented *)
  | If
  | For
  | Loop
  | Block of desc list
;;

(** [pos] stores the line and the position of the specific desc *)
type pos = {
  starts: int;
  line: int;
  ends: int;
};;

(** [stmt] this is a stmt, it's created one for each stmt parsed *)
type stmt = {
  pos: pos;
  desc: desc;
};;

(** [code] stores all the AST code from a string/file *)
type code = stmt list
