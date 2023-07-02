open Common

type typ =
  | Tbool
  | Tint
  | Tstr
  | Tproduct of typ * typ
  | Tarrow of typ * typ
  | Tvar of tvar
  | Tunit

and tvar = { id : int; mutable def : typ option }

type expr_eval =
  | Const of values
  | Var of string
  (* let ... = ...;;
     and
     let = .. in
  *)
  | LetIn of name * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | IfThen of expr * expr * expr

and expr = { expr : expr_eval; pos : pos; typ : typ }
and t = Let of { name : name; expr : expr; pos : pos; typ : typ }

let rec pp_typ = function
  | Tbool -> "bool"
  | Tint -> "int"
  | Tstr -> "str"
  | Tproduct (t1, t2) -> Format.sprintf "(%s * %s)" (pp_typ t1) (pp_typ t2)
  | Tarrow (t1, t2) -> Format.sprintf "%s -> %s" (pp_typ t1) (pp_typ t2)
  | Tvar { def = Some t; _ } -> pp_typ t
  | Tvar { def = None; _ } -> "'a"
  | Tunit -> "()"

let rec pp_expr_t = function
  | { expr = Const v; typ; _ } ->
      Format.sprintf "([typ %s]%s)" (pp_typ typ) (pp_values v)
  | { expr = Var s; typ; _ } -> Format.sprintf "([typ %s]%s)" (pp_typ typ) s
  | { expr = LetIn (s, e1, e2); typ; _ } ->
      Format.sprintf "([typ %s]let %s = %s in %s)" (pp_typ typ) s (pp_expr_t e1)
        (pp_expr_t e2)
  (* | { expr = LetIn (s, e1, None); typ; _ } -> *)
  (*     Format.sprintf "[typ %s]let %s = %s;\n" (pp_typ typ) s (pp_t e1) *)
  | { expr = Fun (s, e1); typ; _ } ->
      Format.sprintf "[typ %s](fun (%s) -> %s)\n" (pp_typ typ) s (pp_expr_t e1)
  | { expr = App (e1, e2); typ; _ } ->
      Format.sprintf "[typ %s](%s %s)" (pp_typ typ) (pp_expr_t e1)
        (pp_expr_t e2)
  | { expr = IfThen (b, e1, e2); typ; _ } ->
      Format.sprintf "[typ %s](if %s then %s else %s)" (pp_typ typ)
        (pp_expr_t b) (pp_expr_t e1) (pp_expr_t e2)

let pp_t = function
  | Let { name; typ; expr; _ } ->
      let e = pp_expr_t expr in
      Format.sprintf "let %s : %s = %s;" name (pp_typ typ) e
