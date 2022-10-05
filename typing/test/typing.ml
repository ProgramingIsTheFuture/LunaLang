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

let () =
  [complex_types; invalid_types;]
  |> List.map (fun a -> a ())
  |> ignore;;
