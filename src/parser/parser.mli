type t

module Errors = Errors

val of_string : string -> t
val of_file : string -> t
val parse : t -> Ast.Parsing.t list
