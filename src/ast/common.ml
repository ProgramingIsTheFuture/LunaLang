type values = Vint of int | Vstr of string | Vbool of bool
type pos = { starts : int; ends : int }
type name = string

let pp_values = function
  | Vint v -> Format.sprintf "%d" v
  | Vbool v -> Format.sprintf "%b" v
  | Vstr v -> Format.sprintf "%s" v
