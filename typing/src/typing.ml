module Error = Error

(* open Convert *)

(* let rec check_types: Ast.Ast.code -> Ast.TypedAst.code = function *)
(*   | {desc = Const v; pos = pos} :: ll -> *)
(*     (* Checking the const type *) *)
(*     let cc = desc_of_ast (Error.format_position pos) (Const v) in *)
(*     Typeit.consts cc pos *)
(*     :: check_types ll *)
(*   | {desc = Var _ as v; pos = pos} :: ll -> *)
(*     (* Variable already declared *) *)
(*     let v = desc_of_ast (Error.format_position pos) v in *)
(*     Typeit.vars v pos *)
(*     :: check_types ll *)
(*   | {desc = Let _ as l; pos = pos} :: ll -> *)
(*     let l = desc_of_ast (Error.format_position pos) l in *)
(*     let l = Typeit.lets l pos in  *)
(*     l :: check_types ll *)
(*   | {desc = Fun _ as f; pos = pos} :: ll -> *)
(*     let f = desc_of_ast (Error.format_position pos) f in *)
(*     let l = Typeit.funs f pos in *)
(*     l :: check_types ll *)
(*   | {desc = Op _ as op; pos = pos} :: ll -> *)
(*     let op = desc_of_ast (Error.format_position pos) op in *)
(*     Typeit.ops op pos :: check_types ll *)
(*   | {desc = Apply _ as ap; pos = pos } :: ll -> *)
(*     let ap = desc_of_ast (Error.format_position pos) ap in *)
(*     Typeit.apply ap pos :: check_types ll *)
(*   | s :: _ -> *)
(*     Printf.printf "%s \n" (Error.format_position s.pos); *)
(*     assert false *)
(*   | [] -> [] *)

(** [check_types]: Ast.Ast.code -> Ast.TypedAst.code 
    This functions will transform any Ast.Ast.code into the equivalent in
    Ast.TypedAst.code.
    After the transformation, it will type every stmt.
 *)
let check_types (ast_code: Ast.Ast.code): Ast.TypedAst.code = 
    let ctx = Hashtbl.create 1 in 
    let l = List.map (Convert.stmt_of_ast ctx) ast_code in
    Hashtbl.clear ctx;
    l

