module Error = Error
(** [Error] is the error module for the lex module *)

val parse : ?fname:string -> ?code:string -> unit -> Ast.ut_code
(** [parse] will lex and parse the the code and returns the AST *)
