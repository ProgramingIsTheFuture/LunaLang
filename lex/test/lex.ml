let basic_variables () =
  let code =
    "let a = 10;\nlet b = \"Hello World\";\nlet c = false;\na" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Let ("a", TInference, Expr (Const (VInt 10)))
            :: Let ("b", TInference, Expr (Const (VString "\"Hello World\"")))
            :: Let ("c", TInference, Expr (Const (VBool false)))
            :: Expr (Var "a") :: []  in
  assert (exp = t);;

let complex_inference () =
  (* TODO *)
  let code = "let sum (a: int): int = a;" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Fun ("sum", TTyp "int", PTyp ("a", TTyp "int") :: [], Expr (Var "a"))
            :: [] in
  assert (exp = t);;

let () =
  [basic_variables; complex_inference;]
  |> List.map (fun f -> f ())
  |> ignore;;
