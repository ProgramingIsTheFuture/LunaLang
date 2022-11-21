exception InvalidType of Ast.Ast.pos * string

let invalid_type pos s =
  raise (InvalidType (pos, s))
