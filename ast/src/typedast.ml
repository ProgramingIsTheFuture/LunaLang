(** AST with types *)

(** [value] are the possible values for the dyri language *)
type value =
  | VInt of int
  | VString of string
  | VBool of bool
;;

(** [typ] are the possible types for the language
    After the typing stage every variable
    will have one of these types *)
type typ =
  | TCustom of string
  | TInt
  | TString
  | TBool
  (* We don't know yet or can be any type *)
  | TGeneric

(** [param] is the params for functions *)
type param = string * typ

(** [op] are the available operatores
    [Add] +
    [Sub] -
    [Div] /
    [Mul] *
    [Mod] % *)
type op =
  | Add of (value * value)
  | Sub of (value * value)
  | Div of (value * value)
  | Mul of (value * value)
  | Mod of (value * value)
;;

(** [desc] possible statements to use inside the Dyri language *)
type desc =
  (*  *)
  | Const of value
  | Op of op
  | Var of string
  | Apply of (string * desc list)
  | Let of (string * typ * desc)
  (** Both Fun and AnFun have return typ and typ for each params *)
  | Fun of (string * param list * desc)
  | AnFun of (param list * desc)
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
