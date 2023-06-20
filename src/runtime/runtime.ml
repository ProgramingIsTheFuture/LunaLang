module SMap = Map.Make (String)

type t = Ast.Typed.typ SMap.t

let empty = SMap.empty
let add = SMap.add
let find = SMap.find
