open Common

type typ = Int | String | Product of typ * typ | Tarrow of typ * typ

type expr =
  | Const of values
  | Var of string
  (* let ... = ...;;
     and
     let = .. in
  *)
  | LetIn of name * t * t option
  | Fun of string * t
  | App of t * t
  | IfThen of t * t * t

and t = { expr : expr; pos : pos; typ : typ }
