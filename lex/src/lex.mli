(** [Error] is the error module for the lex module *)
module Error = Error

(** [parse] will lex and parse the the code and returns the AST *)
val parse: ?fname:string -> ?code:string -> unit -> Ast.Ast.code
