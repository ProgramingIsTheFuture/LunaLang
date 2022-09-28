let basic_variables () =
  let code =
    "let a = 10;\nlet b = \"Hello World\";\nlet c = false;\na" in
  let t = Lex.parse ~code () in

  let open Ast in
  let exp = Let ("a", TTyp (TInt), Const (VInt 10))
            :: Let ("b", TTyp (TString), Const (VString "\"Hello World\""))
            :: Let ("c", TTyp (TBool), Const (VBool false))
            :: Var "a" :: []  in
  assert (exp = t);;

let complex_inference () =
  (* TODO *)
  let code = "let sum (a: int): int -> int = a;" in
  let t = Lex.parse ~code () in

  let open Ast in
  let exp = Fun ("sum", TFTyp (FTFun (TInt :: TInt :: [])), PTyp ("a", TTyp (TInt)) :: [], Var "a")
            :: [] in
  assert (exp = t);;

let complex_types () =
  (* Is returning a string but expected a int *)
  let code = "let sum (a: string) (b: int): string -> int -> int = a;" in
  try
    Lex.parse ~code () |> ignore;
    assert false
  with
  | Lex.Error.InvalidType _ ->
    assert true;;

let invalid_types () =
  (* First input is a string and a int? = Type error *)
  let code = "let sum (a: string) (b: int): int -> int -> int = a;" in
  try
    Lex.parse ~code () |> ignore;
    assert false
  with
  | Lex.Error.InvalidType _ ->
    assert true;;

let apply_fun_types () =
  (* First input is a string and a int? = Type error *)
  let code =
    "let sum (a: int): int -> int = 10; sum 10" in
  let t = Lex.parse ~code () in

  (*
    :: Apply ("sum", Const (VString "\"Hello World\"") :: [])
  *)
  let open Ast in
  let exp = Fun ("sum", TFTyp (FTFun (TInt :: TInt :: [])), PTyp ("a", TTyp TInt) :: [], Const (VInt 10))
            :: Apply ("sum", Const (VInt 5) :: [])
            :: [] in
  List.nth t 1 |> let h = function
      | Apply (n, t) ->
        Printf.printf "Eval: %s\n" n in
  assert (exp = t);;

let () =
  [basic_variables; complex_inference; complex_types; invalid_types; apply_fun_types;]
  |> List.map (fun f -> f ())
  |> ignore;;
