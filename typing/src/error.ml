exception InvalidType of string

let format_position (positions: Ast.Ast.pos) =
  Format.sprintf 
    "Line: %d | chars on %d-%d " 
    positions.line 
    positions.starts 
    positions.ends

let invalid_type s =
  raise (InvalidType s)
