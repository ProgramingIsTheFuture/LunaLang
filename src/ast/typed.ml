open Common

type typ = Int | String | Product of typ * typ | Tarrow of typ * typ

type expr =
  | Const of values
  | LetIn of name * t * t
  | Fun of string * t
  | IfThen of t * t * t

and t = { expr : expr; pos : pos; typ : typ }
