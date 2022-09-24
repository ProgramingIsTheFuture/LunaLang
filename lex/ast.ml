type value =
  | VInt of int
  | VString of string
  | VBool of bool

type typ =
  | TInference
  | TVal of string
  | TFun of string * typ

type param =
  | PTyp of (string * typ)
  | PName of string

type expr =
  | Const of value
  | Var of string
  | Let of (string * typ * expr)
  | Fun of (string * typ * param list * expr)
  | AnFun of (param list * expr)
  | If of (expr * expr)
  | For of (value * value * expr)
  | Loop of (expr * expr)
  | Block of expr list

type code = expr list
