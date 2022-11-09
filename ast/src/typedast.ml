(** AST with types *)

(** [value] are the possible values for the dyri language *)
type value =
  | VInt of int64
  | VInt32 of int32
  | VString of string
  | VBool of bool
;;

(** [typ] are the possible types for the language
    After the typing stage every variable
    will have one of these types *)
type typ =
  | TCustom of string
  | TGeneric
  (** sum: TSeq (int, TSeq (int, TSeq (int, None) ) ) 
    sum: int -> int -> int 
    let sum = (a: int) -> (b: int) -> a + b *)
  | TSeq of typ * typ option
  | TInt
  | TInt32
  | TString
  | TBool;;

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
  | AnFun of (string * typ * desc)
  (* need to be implemented *)
  | If
  | For
  | Loop
  (** Block have a return typ *)
  | Block of desc list
;;

(** [stmt] this is a stmt, it's created one for each stmt parsed *)
type stmt = {
  typ: typ;
  desc: desc;
};;

(** [code] stores all the AST code from a string/file *)
type code = stmt list
