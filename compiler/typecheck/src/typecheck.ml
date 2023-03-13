module Error = Error

let rec stmt_of_ast (ctx : Env.env) : Ast.ut_stmt -> Ast.stmt * Env.env option =
  function
  | { desc = _ as v; pos } ->
      let v, t, ctx' = desc_of_ast ctx pos v in
      ({ desc = v; typ = t }, ctx')

and desc_of_ast ctx pos (ast_desc : Ast.ut_desc) :
    Ast.desc * Ast.typ * Env.env option =
  let open Ast in
  match ast_desc with
  | Const v ->
      let t = Convert.typ_of_desc ctx pos (Const v) in
      (Const v, t, None)
  | Var s ->
      let v = Var s in
      let t = Convert.typ_of_desc ctx pos v in
      (v, t, None)
  | Op (e1, op, e2) ->
      let e1, _ = stmt_of_ast ctx e1 in
      let e2, _ = stmt_of_ast ctx e2 in
      let t = TInt in
      (Op (e1, op, e2), t, None)
  | Let (x, t, ds1) ->
      let ds1, _ = stmt_of_ast ctx ds1 in
      let t = Convert.typ_of_ast pos t in
      let t = Convert.cmp_typs t ds1.typ in
      let ctx = Env.add true x t ctx in
      (Let (x, ds1), t, Some ctx)
  | Fun (x, t, e1) ->
      let t = Convert.typ_of_ast pos t in
      let ctx = Env.add false x t ctx in
      let ds1, _ = stmt_of_ast ctx e1 in
      let t = TSeq (t, ds1.typ) in
      (Fun (x, ds1), t, None)
  | AnFun (x, t, e1) ->
      let t = Convert.typ_of_ast pos t in
      let ctx = Env.add false x t ctx in
      let ds1, _ = stmt_of_ast ctx e1 in
      let t = TSeq (t, ds1.typ) in
      (Fun (x, ds1), t, None)
  | Apply (x, args) ->
      let args = List.map (fun a -> stmt_of_ast ctx a |> fst) args in
      let v = Apply (x, args) in
      let t = Convert.typ_of_desc ctx pos v in
      (v, t, None)
  | _ -> assert false

let convert_ast ctx c =
  let nc, ctx' = stmt_of_ast ctx c in
  let ctx = if Option.is_some ctx' then Option.get ctx' else ctx in
  (ctx, nc)

(** [check_types]: Ast.Ast.code -> Ast.TypedAst.code 
    This functions will transform any Ast.Ast.code into the equivalent in
    Ast.TypedAst.code.
    After the transformation, it will type every stmt.
 *)
let check_types (ast_code : Ast.ut_code) : Ast.code =
  let ctx : Env.env = Env.empty in
  List.fold_left_map convert_ast ctx ast_code |> snd
