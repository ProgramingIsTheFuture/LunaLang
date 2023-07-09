module Errors = Errors

type t = { filename : string; content : string }

let rec read_complete_file s f =
  try read_complete_file (s ^ input_line f ^ "\n") f with End_of_file -> s

let of_string (s : string) : t = { content = s; filename = "" }

let of_file (s : string) : t =
  let f = open_in s in
  { content = read_complete_file "" f; filename = s }

let parse (s : t) =
  let lexbuf = Lexing.from_string s.content in
  let () = Lexing.set_filename lexbuf s.filename in
  try Parse.main Lexer.token lexbuf
  with Parse.Error -> Errors.parsing_error lexbuf
