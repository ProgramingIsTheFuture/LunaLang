open Common

type typ = string option

type expr =
  | Const of values
  | Var of string
  (* let ... = ...;;
     and
     let = .. in
  *)
  | LetIn of name * typ * t * t option
  | Fun of string * typ * t
  | App of t * t
  | IfThen of t * t * t

and t = { expr : expr; pos : pos }

let rec pp_t = function
  | { expr = Const v; _ } -> pp_values v
  | { expr = Var s; _ } -> Format.sprintf "%s" s
  | { expr = LetIn (s, t, e1, Some e2); _ } -> (
      match t with
      | None -> Format.sprintf "let %s = %s in %s" s (pp_t e1) (pp_t e2)
      | Some t ->
          Format.sprintf "let %s : %s = %s in %s" s t (pp_t e1) (pp_t e2))
  | { expr = LetIn (s, t, e1, None); _ } -> (
      match t with
      | None -> Format.sprintf "let %s = %s\n" s (pp_t e1)
      | Some t -> Format.sprintf "let %s : %s = %s\n" s t (pp_t e1))
  | { expr = Fun (s, t, e1); _ } -> (
      match t with
      | None -> Format.sprintf "fun %s -> %s\n" s (pp_t e1)
      | Some t -> Format.sprintf "fun (%s : %s) -> %s\n" s t (pp_t e1))
  | { expr = App (e1, e2); _ } -> Format.sprintf "(%s %s)" (pp_t e1) (pp_t e2)
  | { expr = IfThen (b, e1, e2); _ } ->
      Format.sprintf "if %s then %s else %s" (pp_t b) (pp_t e1) (pp_t e2)
