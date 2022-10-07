let complex_types () =
  (* Is returning a string but expected a int *)
  let code = "let sum (a: int) (b: int): int = a;" in
  try
    Lex.parse ~code () |>
    Typing.check_types |> ignore;
    assert false
  with
  | Typing.Error.InvalidType _ ->
    assert true;;

let invalid_types () =
  (* First input is a string and a int? = Type error *)
  let code = "let sum (a: string) (b: int): int = b;" in
  try
    Lex.parse ~code () |>
    Typing.check_types |> ignore;
    assert false
  with
  | Typing.Error.InvalidType _ ->
    assert true;;

let operator_types () =
  let code = "let a = 10; a + 10" in
  let typedAst = Lex.parse ~code () |>
                 Typing.check_types in

  let open Ast.TypedAst in
  let exp = Let ("a", TInt, Const (VInt 10))
            :: Op ( Add (Var("a"), Const (VInt 10))) :: [] in

  List.map2 (fun tc ex -> assert (tc.desc = ex)) typedAst exp |> ignore;;

let () =
  [complex_types; invalid_types; operator_types;]
  |> List.map (fun a -> a ())
  |> ignore;;
