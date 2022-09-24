{
    open Parser

    exception InvalidKwd of string
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
    | '\n' { token lexbuf }
    | int as s { VALUE (VInt (int_of_string s)) }
    | string as s { VALUE (VString s) }
    | bool as s { VALUE (VBool (if s = "true" then true else false)) }
    | eof { EOF }
    | _ as c { raise (InvalidKwd (Format.sprintf "Char %c is invalid" c)) }
