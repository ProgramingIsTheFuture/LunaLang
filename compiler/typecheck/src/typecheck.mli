module Error = Error
(** [Error] is the error module for the typing module *)

val check_types : Ast.ut_code -> Ast.code
(** [check_types] will check every types inside the Ast. *)
