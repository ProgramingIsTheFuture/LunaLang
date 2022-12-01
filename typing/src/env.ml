module Env = Map.Make (String)

type env = Ast.TypedAst.typ Env.t
