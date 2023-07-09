exception TypeError of string
exception UnboundVariableError of string
exception LinearTypeError of string

let type_error t1 t2 pos =
  raise
    (TypeError
       (Format.sprintf "%s Expected %s but got %s." (Ast.Common.pp_pos pos)
          (Ast.Typed.pp_typ t1) (Ast.Typed.pp_typ t2)))

let unbound_variable var_name pos =
  raise
    (TypeError
       (Format.sprintf "%s Unbound variable %s." (Ast.Common.pp_pos pos)
          var_name))

let linear_type_error () = raise (LinearTypeError "")
