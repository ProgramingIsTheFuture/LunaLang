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

let rec pp_t_ident ast =
  match ast with
  | { expr = Const v; _ } -> pp_values v
  | { expr = Var s; _ } -> Format.sprintf "%s" s
  | { expr = LetIn (s, t, e1, Some e2); _ } ->
      let t =
        match t with None -> " " | Some t -> Format.sprintf " : %s " t
      in
      Format.sprintf "let %s%s= %s in %s" s t (pp_t_ident e1) (pp_t_ident e2)
  | { expr = LetIn (s, t, e1, None); _ } ->
      let t =
        match t with None -> " " | Some t -> Format.sprintf " : %s " t
      in
      Format.sprintf "let %s%s= %s;\n" s t (pp_t_ident e1)
  | { expr = Fun (s, t, e1); _ } ->
      let t =
        match t with
        | None -> Format.sprintf "%s" s
        | Some t -> Format.sprintf "(%s : %s)" s t
      in
      Format.sprintf "fun %s -> %s" t (pp_t_ident e1)
  | { expr = App (e1, e2); _ } ->
      Format.sprintf "(%s %s)" (pp_t_ident e1) (pp_t_ident e2)
  | { expr = IfThen (b, e1, e2); _ } ->
      Format.sprintf "if %s then %s else %s" (pp_t_ident b) (pp_t_ident e1)
        (pp_t_ident e2)

let pp_t t = pp_t_ident t |> Format.sprintf "%s"
