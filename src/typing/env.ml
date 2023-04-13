module Env = Map.Make (String)

type env = { bindings : Tvar.schema Env.t; fvars : Tvar.Vset.t }

let empty = { bindings = Env.empty; fvars = Tvar.Vset.empty }

let add gen x t env =
  let vt = Tvar.fvars t in
  let s, fvars =
    if gen then
      let env_fvars = Tvar.norm_varset env.fvars in
      let open Tvar in
      ({ vars = Tvar.Vset.diff vt env_fvars; typ = t }, env.fvars)
    else ({ vars = Tvar.Vset.empty; typ = t }, Tvar.Vset.union env.fvars vt)
  in
  { bindings = Env.add x s env.bindings; fvars }

module Vmap = Map.Make (Tvar.V)

(* find x env devolve uma instância fresca de env(x) *)
let find x env =
  let open Ast.TypedAst in
  let tx = Env.find x env.bindings in
  let s =
    Tvar.Vset.fold
      (fun v s -> Vmap.add v (TVar (Tvar.V.create ())) s)
      tx.vars Vmap.empty
  in
  let rec subst t =
    match Tvar.head t with
    | TVar x as t -> ( try Vmap.find x s with Not_found -> t)
    | TSeq (t1, t2) -> TSeq (subst t1, subst t2)
    | t -> t
  in
  subst tx.typ
