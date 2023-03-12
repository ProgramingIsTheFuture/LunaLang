let () =
  (*let open Ast.TypedAst in*)
  let _c = Gen_llvm.compile [] in
  let f = "" in
  assert (f = "")
