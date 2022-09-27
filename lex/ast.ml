type value =
  | VInt of int
  | VString of string
  | VBool of bool

type vtyp =
  | TInt
  | TString
  | TBool

type ftyp =
  | FTFun of vtyp list

type typ =
  | TTyp of vtyp
  | TFTyp of ftyp
  | TInference

(*
type typ =
  | TTyp of string
  | TInference
*)

type param =
  | PTyp of (string * typ)
  | PName of string

type expr =
  | Const of value
  | Var of string
  | Let of (string * typ * expr)
  | Fun of (string * typ * param list * expr)
  | AnFun of (param list * expr)
  | Apply of (string * expr list)
  | If of (expr * expr)
  | For of (value * value * expr)
  | Loop
  | Block of expr list

type code = expr list
