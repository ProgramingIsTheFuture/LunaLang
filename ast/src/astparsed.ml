(**  *)

type value =
  | VInt of int
  | VString of string
  | VBool of bool

type typ =
  | TTyp of string
  | TInference

type param =
  | PTyp of (string * typ)
  | PName of string

type expr =
  | Const of value
  | Var of string
  | Apply of (string * expr list)

(** Possible statements to use inside the Dyri language *)
type stmt =
  (*  *)
  | Expr of expr
  | Let of (string * typ * stmt)
  | Fun of (string * typ * param list * stmt)
  | AnFun of (param list * stmt)
  (* need to be implemented *)
  | If
  | For
  | Loop
  | Block of stmt list

type code = stmt list
