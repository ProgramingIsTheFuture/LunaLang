open Ast.Parsing
open Ast.Common
open Ast.Typed
module Env = Env
module Errors = Errors

let typ_of_value = function Vint _ -> Tint | Vstr _ -> Tstr | Vbool _ -> Tbool

type token = Arrow | Tip of string

let rec typ_of_str = function
  | "int" -> Tint
  | "str" -> Tstr
  | "bool" -> Tbool
  | "unit" -> Tunit
  | t ->
      (* Lexical analyzer *)
      let rec lexer str =
        match str with
        | [] -> []
        | hd :: tl -> (
            match hd with "->" -> Arrow :: lexer tl | s -> Tip s :: lexer tl)
      in

      (* Parser *)
      let rec parse_type tokens =
        match tokens with
        | Tip s :: Arrow :: tl -> Tarrow (typ_of_str s, parse_type tl)
        | Tip s :: [] -> typ_of_str s
        | _ -> failwith "Invalid type"
      in

      (* Conversion function *)
      let convert_string_to_type str =
        let tokens = lexer (String.split_on_char ' ' str) in
        if List.length tokens == 1 then
          let () =
            match List.hd tokens with
            | Tip s -> Format.printf "%s \n" s
            | Arrow -> Format.printf "->"
          in
          assert false
        else ();
        parse_type tokens
      in
      (* TODO: convert arrow types, product types, custom types *)
      (* assert false *)
      convert_string_to_type t

let rec type_expr env : Ast.Parsing.expr -> Env.t * Ast.Typed.expr = function
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
      let _, b = type_expr env b in
      Env.unify ~pos:b.pos b.typ Tbool;
      let _, e1 = type_expr env e1 in
      let _, e2 = type_expr env e2 in
      Env.unify ~pos:e2.pos e1.typ e2.typ;
      (env, { expr = IfThen (b, e1, e2); pos; typ = e1.typ })
  | { expr = LetIn (s, t, e1, e2); pos } ->
      let t =
        match t with Some t -> typ_of_str t | None -> Tvar (Env.V.create ())
      in
      let _, e1 = type_expr env e1 in
      Env.unify ~pos:e1.pos t e1.typ;
      (* Add var to the env for e2 *)
      let env2 = Env.add s t env in
      let _, e2 = type_expr env2 e2 in
      Env.unify ~pos:e1.pos t e1.typ;
      (env, { expr = LetIn (s, e1, e2); pos; typ = e2.typ })
  | { expr = App (e1, e2); pos } ->
      let _, e1 = type_expr env e1 in
      let _, e2 = type_expr env e2 in
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
      let _, e1 = type_expr env2 e1 in
      (env, { expr = Fun (s, e1); pos; typ = Tarrow (v, e1.typ) })

let typeit env : Ast.Parsing.t -> Env.t * Ast.Typed.t = function
  | Let { name; expr; pos; typ } ->
      let _, expr = type_expr env expr in
      let t =
        match typ with None -> Tvar (Env.V.create ()) | Some t -> typ_of_str t
      in
      Env.unify ~pos t expr.typ;
      let env = Env.add_gen name t env in
      (env, Let { name; expr; pos; typ = expr.typ })

(* | _ -> assert false *)

let type_ast ~(env : Env.t) ast =
  List.fold_left
    (fun (env, ast) l ->
      let env, ll = typeit env l in
      (env, ll :: ast))
    (env, []) ast
  |> snd |> List.rev
