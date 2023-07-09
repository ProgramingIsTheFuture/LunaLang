type t

val default : t
val empty : t
val add : string -> Ast.Typed.typ -> t -> t
val find : string -> t -> Ast.Typed.typ
