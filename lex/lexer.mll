{
  open Parser

  let keyword_table =
    Hashtbl.create 4;;

  let _ =
    [
      ("let", LET);
      ("for", FOR);
      ("if", IF);
      ("else", ELSE);
    ] |>
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok);;

  let newline lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    };;
}


let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let bool = "true" | "false"
let int = ['0'-'9']+
let float = digit digit* ['.'] digit* | digit digit* ['.'] digit*
let char = letter
let space = [' ' '\t']
let anychar = [^ '"']
let string = '"'(anychar+)'"'

rule token = parse
  | '\n' { newline lexbuf; token lexbuf }
  | space+ { token lexbuf }
  | "->" { RARROW }
  | ';' { SEMICOLON }
  | '?' { QUESTION }
  | ':' { DOUBLEDOT }
  | '=' { EQUAL }
  | '(' { LPARENT }
  | ')' { RPARENT }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | int as s { VALUE (VInt (int_of_string s)) }
  | string as s { VALUE (VString s) }
  | bool as s { VALUE (VBool (if s = "true" then true else false)) }
  | eof { EOF }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id
    { try
        Hashtbl.find keyword_table id
      with
        Not_found -> NAME id
    }
  | _ as c { raise (Error.InvalidKwd (Format.sprintf "Char %c is invalid" c)) }
