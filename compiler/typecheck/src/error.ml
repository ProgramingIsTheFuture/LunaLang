exception InvalidType of Ast.pos * string

let invalid_type pos s = raise (InvalidType (pos, s))
