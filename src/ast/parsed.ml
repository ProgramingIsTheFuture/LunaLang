open Common

type typ = string option

type expr =
  | Const of values
  | LetIn of name * typ * t * t
  | Fun of string * typ * t
  | IfThen of t * t * t

and t = { expr : expr; pos : pos }
