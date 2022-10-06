(** [Error] is the error module for the typing module *)
module Error = Error

(** [check_types] will check every types inside the Ast. *)
val check_types: Ast.Ast.code -> Ast.TypedAst.code
