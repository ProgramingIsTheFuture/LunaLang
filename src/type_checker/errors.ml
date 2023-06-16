exception TypeError of string

let type_error t1 t2 _pos =
  raise
    (TypeError
       (Format.sprintf "Expected %s but got %s." (Ast.Typed.pp_typ t1)
          (Ast.Typed.pp_typ t2)))
