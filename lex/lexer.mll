{
    open Parser

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
let string = '"'(((letter|digit|space) (letter|digit|space)+) | digit)'"'

rule token = parse
    | '\n' { newline lexbuf; token lexbuf }
    | ':' { DOUBLEDOT }
    | int as s { VALUE (VInt (int_of_string s)) }
    | string as s { VALUE (VString s) }
    | bool as s { VALUE (VBool (if s = "true" then true else false)) }
    | eof { EOF }
    | _ as c { raise (Error.InvalidKwd (Format.sprintf "Char %c is invalid" c)) }
