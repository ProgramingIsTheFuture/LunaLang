
let () =
  let open Ast.TypedAst in
  let c = Compiler.compile (
      {desc = Fun ("hello", [("x", TInt)], Var "x"); typ = TInt}
      :: {desc = Fun ("main", [], Apply ("hello", Const (VInt 55) :: [])); typ = TInt}
      :: []) in
  let f = "; ModuleID = 'Dyri'
source_filename = \"Dyri\"

define i32 @hello(i32 %x) {
  ret i32 %x
}

define i32 @main() {
  %1 = call i32 @hello(i32 55)
  ret i32 %1
}\n" in
  assert (f = c);;
