(**
  convert.ml
  Will convert Ast.Ast.* 
  to 
  Ast.TypedAst.*
 *)

open Ast.Ast
open Ast.TypedAst

let typ_of_string = function
  | "int" -> TInt
  | "int32" -> TInt32
  | "bool" -> TBool
  | "string" -> TString
  | s -> TCustom s

let typ_of_ast (position: string) = function
  | TTyp (Some s) ->
    begin
      match s with
      | "int" -> TSeq(TInt, None)
      | "bool" -> TSeq(TBool, None)
      | "string" -> TSeq(TString, None)
      | s -> 
        let sl = String.split_on_char ' ' s in
        if List.length sl = 1 then
          TSeq(TCustom s, None)
        else
          (* 0   1  2 *)
          (* int -> int *)
          let r = List.filteri
            (fun i a ->
              if i mod 2 <> 0 && a <> "->" then
                raise (Error.InvalidType (Format.sprintf "%s| Invalid type" position))
              else if i mod 2 <> 0 then
                false
              else true
            ) sl 
          |> List.fold_left 
            (fun prev a ->
              Some (TSeq (typ_of_string a, prev))
            ) None in
          match r with
          | Some v -> v
          | None -> assert false
    end
  | TTyp None -> TSeq(TGeneric, None)
;;

let value_of_ast: Ast.Ast.value -> Ast.TypedAst.value = function
  | VInt v -> VInt v
  | VInt32 v -> VInt32 v
  | VBool v -> VBool v
  | VString v -> VString v
;;

let op_of_ast: Ast.Ast.op -> Ast.TypedAst.op = function
  | Add -> Add
  | Mod -> Mod
  | Mul -> Mul
  | Div -> Div
  | Sub -> Sub
;;


let rec desc_of_ast (position: string) (ast_desc: Ast.Ast.desc): Ast.TypedAst.desc =
  match ast_desc with
  | Const v ->
    Const (value_of_ast v)
  | Var s -> Var s
  | Let ((s, t), ds) ->
    let t = typ_of_ast position t in
    let ds = desc_of_ast position ds in
    Let ((s, t), ds)
  | Fun ((s, t), ds) ->
    Fun ((s, typ_of_ast position t), desc_of_ast position ds)
  | AnFun (s, t, ds) ->
    AnFun (s, typ_of_ast position t, desc_of_ast position ds)
  | Op (ds1, op, ds2) ->
    Op(desc_of_ast position ds1, op_of_ast op, desc_of_ast position ds2)
  | _ -> assert false


