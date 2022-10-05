(** AST with types *)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
;;

(** After the typing stage every variable
    will have one of these types *)
type typ =
  | TCustom of string
  | TInt
  | TString
  | TBool
  (* We don't know yet or can be any type *)
  | TGeneric

type param = string * typ

(** Possible statements to use inside the Dyri language *)
type desc =
  (*  *)
  | Const of value
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

type stmt = {
  typ: typ;
  desc: desc;
};;

type code = stmt list
