(** AST with types *)

(** After the typing stage every variable
    will have one of these types *)
type typ =
  | TCustom of string
  | TInt
  | TString
  | TBool
  (* We don't know yet or can be any type *)
  | TGeneric

type value =
  | VInt of int
  | VString of string
  | VBool of bool

type param = string * typ

type expr =
  | Const of value
  | Var of string
  | Apply of (string * expr list)

(** Possible statements to use inside the Dyri language *)
type stmt =
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
