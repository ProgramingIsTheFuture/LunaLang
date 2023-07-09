type values = Vint of int | Vstr of string | Vbool of bool

type pos = {
  pos_fname : string; (* file name *)
  pos_lnum : int; (* line number *)
  pos_bol : int; (* the offset of the beginning of the line *)
  pos_cnum : int; (* the offset of the position *)
}

let pp_pos pos =
  Format.sprintf "Error on %s at line %d between %d and %d." pos.pos_fname
    pos.pos_lnum pos.pos_bol pos.pos_cnum

type name = string

let pp_values = function
  | Vint v -> Format.sprintf "%d" v
  | Vbool v -> Format.sprintf "%b" v
  | Vstr v -> Format.sprintf "%s" v
