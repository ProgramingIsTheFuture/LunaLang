type t = string

let rec read_complete_file s f =
  try read_complete_file (s ^ input_line f) f with End_of_file -> s

let of_string (s : string) : t = s
let of_file (f : in_channel) : t = read_complete_file "" f
let lex_it = Lexer.token

let parse (s : t) =
  let lexbuf = Lexing.from_string s in
  Parse.main lex_it lexbuf
