open Common

type typ = string option

type expr =
  | Const of values
  | LetIn of name * typ * expr * expr
  | Fun of string * typ * expr
  | IfThen of expr * expr * expr

type t = { expr : expr; pos : pos }
