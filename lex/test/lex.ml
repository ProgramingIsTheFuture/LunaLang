let basic_variables () =
  let code =
    "let a = 10;\nlet b = \"Hello World\";\nlet c = false;\na" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Let ("a", TInference, Const (VInt 10))
            :: Let ("b", TInference, Const (VString "\"Hello World\""))
            :: Let ("c", TInference, Const (VBool false))
            :: Var "a" :: []  in
  List.map2 (fun expp tt ->
      assert (expp.desc = tt)) t exp |> ignore;;

let complex_inference () =
  (* TODO *)
  let code = "let sum (a: int): int = a;" in
  let t = Lex.parse ~code () in

  let open Ast.Ast in
  let exp = Fun ("sum", TTyp "int", PTyp ("a", TTyp "int") :: [], Var "a")
            :: [] in
  List.map2 (fun expp tt ->
      assert (expp.desc = tt)) t exp |> ignore;;

let () =
  [basic_variables; complex_inference;]
  |> List.map (fun f -> f ())
  |> ignore;;
