{
  open Parse

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl
  
  let keyword_table = 
    create_hashtable 8 [
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("fun", FUN);
      ("let", LET);
      ("in", IN);
    ]

  let next_line (lexbuf: Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' '0'-'9']*

rule token = parse
  | ['\n' '\t' ' '] { next_line lexbuf; token lexbuf }
  | '=' { EQUAL }
  | ':' { DOUBLEDOT }
  | ';' { SEMICOLON }
  | '(' { LPARENT }
  | ')' { RPARENT }
  | "->" { ARROW }
  | "()" { UNIT }
  | digit+ as integer { VALUE (Vint (int_of_string integer)) }
  | "true" {VALUE (Vbool (true))}
  | "false" {VALUE (Vbool (false))}
  | '"' _* as text '"' { VALUE (Vstr (text)) }
  | id as word 
    {
      try Hashtbl.find keyword_table word with Not_found -> IDENT word
    }
  | eof {EOF}
  | _ as c { Errors.lexing_error lexbuf c }
