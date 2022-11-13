module Error = Error

open Convert
open Ast.Ast

let rec check_types: Ast.Ast.code -> Ast.TypedAst.code = function
  | {desc = Const v; pos = pos} :: ll ->
    (* Checking the const type *)
    let cc = desc_of_ast (Error.format_position pos) (Const v) in
    Typeit.consts cc pos
    :: check_types ll
  | {desc = Var _ as v; pos = pos} :: ll ->
    (* Variable already declared *)
    let v = desc_of_ast (Error.format_position pos) v in
    Typeit.vars v pos
    :: check_types ll
  | {desc = Let _ as l; pos = pos} :: ll ->
    let l = desc_of_ast (Error.format_position pos) l in
    let l = Typeit.lets l pos in 
    l :: check_types ll
  | {desc = Fun((_s, _t), _ds); pos = _pos} :: _ll ->
    assert false
  | {desc = Op _ as op; pos = pos} :: ll ->
    let op = desc_of_ast (Error.format_position pos) op in
    Typeit.ops op pos :: check_types ll
  | {desc = Apply (_s, _dsl); pos = _pos } :: _ll ->
    assert false
  | s :: _ ->
    Printf.printf "%s \n" (Error.format_position s.pos);
    assert false
  | [] -> []
