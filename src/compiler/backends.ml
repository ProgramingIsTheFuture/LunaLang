open Ast.Common
open Ast.Typed

module Js = struct
  type t = string

  let ( ++ ) = ( ^ )

  let const c =
    match c with
    | Vint i -> string_of_int i
    | Vbool true -> "true"
    | Vbool false -> "false"
    | Vstr s -> Format.sprintf "'%s'" s

  let variable s = s

  let rec funct name expr = Format.sprintf "%s => " name ++ compile_expr expr

  and app expr1 expr2 =
    "(" ++ compile_expr expr1 ++ ")" ++ "(" ++ compile_expr expr2 ++ ")"

  and ifelse b expr1 expr2 =
    compile_expr b ++ "?" ++ compile_expr expr1 ++ ":" ++ compile_expr expr2

  and letin name expr1 expr2 =
    "(() => {let " ++ name ++ " = " ++ compile_expr expr1 ++ ";" ++ " return "
    ++ compile_expr expr2 ++ "})"

  and compile_expr = function
    | { expr = Var s; _ } -> variable s
    | { expr = Const v; _ } -> const v
    | { expr = Fun (name, expr); _ } -> funct name expr
    | { expr = App (expr1, expr2); _ } -> app expr1 expr2
    | { expr = IfThen (b, expr1, expr2); _ } -> ifelse b expr1 expr2
    | { expr = LetIn (s, expr1, expr2); _ } -> letin s expr1 expr2

  and compile_let e = function
    | Let { name; expr; _ } ->
        e ++ "let " ++ name ++ " = " ++ compile_expr expr ++ ";"

  let compile = List.fold_left compile_let ""
end
