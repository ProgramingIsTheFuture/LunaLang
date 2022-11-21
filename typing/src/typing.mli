module Error = Error
(** [Error] is the error module for the typing module *)

val check_types : Ast.Ast.code -> Ast.TypedAst.code
(** [check_types] will check every types inside the Ast. *)
