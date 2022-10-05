(** AST that we want to parse *)

type value =
  | VInt of int
  | VString of string
  | VBool of bool
;;

type typ =
  | TTyp of string
  | TInference
;;

type param =
  | PTyp of (string * typ)
  | PName of string
;;

(** Possible statements to use inside the Dyri language *)
type desc =
  (*  *)
  | Const of value
  | Var of string
  | Apply of (string * desc list)
  | Let of (string * typ * desc)
  | Fun of (string * typ * param list * desc)
  | AnFun of (param list * desc)
  (* need to be implemented *)
  | If
  | For
  | Loop
  | Block of desc list
;;

type pos = {
  starts: int;
  line: int;
  ends: int;
};;

type stmt = {
  pos: pos;
  desc: desc;
};;

type code = stmt list
