open Ast
open Types
open Llvm
open Constants

let signed_consts = true

let printf =
  let printf_t =
    function_type (void_type context) [| pointer_type (i8_type context) |]
  in
  declare_function "printf" printf_t the_module

let ll_of_ast (ast : desc * typ) =
  match ast with
  | Const v, t ->
      let llv =
        match v with
        | VInt v -> const_of_int64 int_t v signed_consts
        | VInt32 v -> const_int int32_t (Int32.to_int v)
        | VString s -> const_string context s
        | VBool b -> const_int bool_t (if b then 1 else 0)
      in
      (llv, lltype_of_typ t)
  | Op (_, _, _), _
  | Var _, _
  | Apply _, _
  | Let (_, _), _
  | Fun (_, _), _
  | AnFun _, _
  | If, _
  | For, _
  | Loop, _
  | Block _, _ ->
      assert false

let llvalue_of_ast (ast : stmt) =
  match ast with { desc; typ } -> (desc, typ) |> ll_of_ast

let main_prog () =
  let main_t = function_type (i32_type context) [||] in
  let main_f = define_function "main" main_t the_module in

  let builder = builder_at_end context (entry_block main_f) in
  let output_string = "Hello, world!\n" in
  let gv = build_global_stringptr output_string "output_string" builder in

  let _ = build_call printf [| gv |] "" builder in

  let _ = build_ret (const_int (i32_type context) 0) builder in

  Llvm.string_of_llmodule the_module
