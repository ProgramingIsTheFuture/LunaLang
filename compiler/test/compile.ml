let () =
  (*let open Ast.TypedAst in*)
  let _c = Compiler.compile ([]) in
  let f = "" in
  assert (f = "");;
