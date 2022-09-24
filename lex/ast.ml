type value =
  | VInt of int
  | VString of string
  | VBool of bool

type typ =
  | TInt
  | TString
  | TEmpty
  | TInference

type param =
  | PTyp of (string * typ)
  | PName of string

type expr =
  | Const of value
  | Let of (string * typ * value)
  | Fun of (string * typ * param * expr)
  | AnFun of (param * expr)
  | If of (expr * expr)
  | For of (value * value * expr)
  | Loop of (expr * expr)
  | Block of expr list

type code = expr list
