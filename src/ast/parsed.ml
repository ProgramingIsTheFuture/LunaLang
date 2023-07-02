open Common

type typ = string option

type expr_eval =
  | Const of values
  | Var of string
  (* let ... = ...;;
     and
     let = .. in
  *)
  | LetIn of name * typ * expr * expr
  | Fun of string * typ * expr
  | App of expr * expr
  | IfThen of expr * expr * expr

and expr = { expr : expr_eval; pos : pos }
and t = Let of { name : name; expr : expr; pos : pos; typ : name option }
(* and t = decl *)

let rec pp_expr_t_ident ast =
  match ast with
  | { expr = Const v; _ } -> pp_values v
  | { expr = Var s; _ } -> Format.sprintf "%s" s
  | { expr = LetIn (s, t, e1, e2); _ } ->
      let t =
        match t with None -> " " | Some t -> Format.sprintf " : %s " t
      in
      Format.sprintf "let %s%s= %s in %s" s t (pp_expr_t_ident e1)
        (pp_expr_t_ident e2)
  | { expr = Fun (s, t, e1); _ } ->
      let t =
        match t with
        | None -> Format.sprintf "%s" s
        | Some t -> Format.sprintf "(%s : %s)" s t
      in
      Format.sprintf "fun %s -> %s" t (pp_expr_t_ident e1)
  | { expr = App (e1, e2); _ } ->
      Format.sprintf "(%s %s)" (pp_expr_t_ident e1) (pp_expr_t_ident e2)
  | { expr = IfThen (b, e1, e2); _ } ->
      Format.sprintf "if %s then %s else %s" (pp_expr_t_ident b)
        (pp_expr_t_ident e1) (pp_expr_t_ident e2)

let pp_t = function
  | Let { name; typ; expr; _ } ->
      let e = pp_expr_t_ident expr in
      Format.sprintf "let %s %s= %s;" name
        (match typ with None -> "" | Some t -> Format.sprintf ": %s " t)
        e
