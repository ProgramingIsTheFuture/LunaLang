open Constants
open Llvm

let int_t = i64_type context
let int32_t = i32_type context
let string_t = pointer_type (i8_type context)
let bool_t = i1_type context
let any_t = void_type context

open Ast.TypedAst

let rec lltype_of_typ (t : Ast.TypedAst.typ) : lltype =
  match t with
  | TInt -> int_t
  | TInt32 -> int32_t
  | TString -> string_t
  | TBool -> bool_t
  | TCustom _ -> assert false
  | TSeq (param, ret_t) ->
      function_type (lltype_of_typ ret_t) [| lltype_of_typ param |]
  | TVar _ -> any_t
