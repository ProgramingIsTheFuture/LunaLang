open Ast

let rec string_of_typ = function
  | TInt -> "int"
  | TInt32 -> "int32"
  | TBool -> "bool"
  | TString -> "string"
  | TCustom s -> s
  | TSeq (t1, t2) -> string_of_typ t1 ^ " -> " ^ string_of_typ t2
  | TVar { def = None; _ } -> "None"
  | TVar { def = Some t; _ } -> string_of_typ t
