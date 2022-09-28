let basic_variables () =
  let code =
    "let a = 10;\nlet b = \"Hello World\";\nlet c = false;\na" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Let ("a", TTyp (TInt), Const (VInt 10))
            :: Let ("b", TTyp (TString), Const (VString "\"Hello World\""))
            :: Let ("c", TTyp (TBool), Const (VBool false))
            :: Var "a" :: []  in
  assert (exp = t);;

let complex_inference () =
  (* TODO *)
  let code = "let sum (a: int): int -> int = a;" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Fun ("sum", TFTyp (FTFun (TInt :: TInt :: [])), PTyp ("a", TTyp (TInt)) :: [], Var "a")
            :: [] in
  assert (exp = t);;

let () =
  [basic_variables; complex_inference;]
  |> List.map (fun f -> f ())
  |> ignore;;
