module Env = Env
module Errors = Errors

val type_ast : env:Env.t -> Ast.Parsing.t list -> Ast.Typed.t list
