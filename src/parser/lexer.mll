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
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' '0'-'9']*

rule token = parse
  | '=' { EQUAL }
  | "->" { ARROW }
  | digit* as integer { VALUE (Vint (int_of_string integer)) }
  | "true" {VALUE (Vbool (true))}
  | "false" {VALUE (Vbool (false))}
  | '"' _* as text '"' { VALUE (Vstr (text)) }
  | id as word 
    {
      try Hashtbl.find keyword_table word with Not_found -> NAME word
    }
  | _ as c { failwith (Format.sprintf "Unknown char %c\n" c) }
