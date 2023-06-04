open Common

type typ = Int | String | Product of typ * typ | Tarrow of typ * typ

type expr =
  | Const of values
  | LetIn of name * expr * expr
  | Fun of string * expr
  | IfThen of expr * expr * expr

type t = { expr : expr; pos : pos; typ : typ }
