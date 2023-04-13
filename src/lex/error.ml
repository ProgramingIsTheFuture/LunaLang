exception InvalidParseParams of string
exception InvalidKwd of string
exception InvalidSyntax of string
exception InvalidFname of string
exception InvalidVariable of string

let fmt_err msg (pos : Lexing.position) =
  Format.sprintf "File: %s, Line: %d, Character: %d-%d %s" pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    (pos.pos_cnum - pos.pos_bol + 1)
    (if msg <> "" then "| " ^ msg else msg)
