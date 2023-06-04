type t

val of_string : string -> t
val of_file : in_channel -> t
val parse : t -> Ast.Parsing.t list
