module Error = Error

let set_filename fname lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
                                Lexing.pos_fname = fname;
                              };;

let parse fname =
  let f =
    try
      open_in fname
    with
    | Sys_error s ->
      raise (Error.InvalidFname s)
  in

  let buf = Lexing.from_channel f in
  set_filename fname buf;

  try
    Parser.code Lexer.token buf
  with
  | Parser.Error ->
    let pos = Lexing.lexeme_start_p buf in
    raise (Error.InvalidSyntax (Format.sprintf "File: %s, Line: %d, Character: %d-%d" (pos.pos_fname) pos.pos_lnum (pos.pos_cnum - pos.pos_bol) (pos.pos_cnum - pos.pos_bol + 1)))
