exception LexingError of string
exception ParsingError of string

let position ((starts, ends) : (Lexing.position * 'a) * ('a * Lexing.position))
    =
  let starts = fst starts in
  let ends = snd ends in
  {
    Ast.Common.pos_fname = starts.pos_fname;
    pos_lnum = starts.pos_lnum;
    pos_bol = starts.pos_bol;
    pos_cnum = ends.pos_cnum;
  }

let lexing_error (lexbuf : Lexing.lexbuf) c =
  let pos =
    position
      ( (lexbuf.lex_curr_p, lexbuf.lex_curr_p),
        (lexbuf.lex_curr_p, lexbuf.lex_curr_p) )
  in
  let s =
    Format.sprintf "%s Unknown character: '%c'" (Ast.Common.pp_pos pos) c
  in
  raise (LexingError s)

let parsing_error (lexbuf : Lexing.lexbuf) =
  let pos =
    position
      ( (lexbuf.lex_curr_p, lexbuf.lex_curr_p),
        (lexbuf.lex_curr_p, lexbuf.lex_curr_p) )
  in
  let s = Format.sprintf "%s" (Ast.Common.pp_pos pos) in
  raise (ParsingError s)
