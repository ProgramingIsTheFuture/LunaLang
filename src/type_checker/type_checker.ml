open Ast.Parsing
open Ast.Common
open Ast.Typed
module Env = Env
module Errors = Errors

let typ_of_value = function Vint _ -> Tint | Vstr _ -> Tstr | Vbool _ -> Tbool

let typ_of_str = function
  | "int" -> Tint
  | "str" -> Tstr
  | "bool" -> Tbool
  | "unit" -> Tunit
  | _ ->
      (* TODO: convert arrow types, product types, custom types *)
      assert false

let rec typeit env : Ast.Parsing.t -> Env.t * Ast.Typed.t = function
  | { expr = Var s; pos } -> (
      try
        let t = Env.find s env in
        (env, { expr = Var s; pos; typ = t })
      with _ ->
        Format.printf "Not Found! %s@?" s;
        failwith "No var Found")
  | { expr = Const v; pos } ->
      (env, { expr = Const v; pos; typ = typ_of_value v })
  | { expr = IfThen (b, e1, e2); pos } ->
      let _, b = typeit env b in
      Env.unify ~pos:b.pos b.typ Tbool;
      let _, e1 = typeit env e1 in
      let _, e2 = typeit env e2 in
      Env.unify ~pos:e2.pos e1.typ e2.typ;
      (env, { expr = IfThen (b, e1, e2); pos; typ = e1.typ })
  | { expr = LetIn (s, t, e1, None); pos } ->
      let t =
        match t with Some t -> typ_of_str t | None -> Tvar (Env.V.create ())
      in
      let _, e1 = typeit env e1 in
      Env.unify ~pos:e1.pos t e1.typ;
      (* Add var to the env (Permanently) *)
      let env = Env.add_gen s t env in
      (env, { expr = LetIn (s, e1, None); pos; typ = Tunit })
  | { expr = LetIn (s, t, e1, Some e2); pos } ->
      let t =
        match t with Some t -> typ_of_str t | None -> Tvar (Env.V.create ())
      in
      let _, e1 = typeit env e1 in
      Env.unify ~pos:e1.pos t e1.typ;
      (* Add var to the env for e2 *)
      let env2 = Env.add s t env in
      let _, e2 = typeit env2 e2 in
      Env.unify ~pos:e1.pos t e1.typ;
      (env, { expr = LetIn (s, e1, Some e2); pos; typ = e2.typ })
  | { expr = App (e1, e2); pos } ->
      let _, e1 = typeit env e1 in
      let _, e2 = typeit env e2 in
      let v = Tvar (Env.V.create ()) in
      Env.unify ~pos e1.typ (Tarrow (e2.typ, v));
      (env, { expr = App (e1, e2); pos; typ = v })
  | { expr = Fun (s, t, e1); pos } ->
      let v = Tvar (Env.V.create ()) in
      let t =
        match t with Some t -> typ_of_str t | None -> Tvar (Env.V.create ())
      in
      Env.unify ~pos t v;
      let env2 = Env.add s v env in
      let _, e1 = typeit env2 e1 in
      (env, { expr = Fun (s, e1); pos; typ = Tarrow (v, e1.typ) })
(* | _ -> assert false *)

let type_ast ~(env : Env.t) ast =
  List.fold_left
    (fun (env, ast) l ->
      let env, ll = typeit env l in
      (env, ll :: ast))
    (env, []) ast
  |> snd |> List.rev
