let complex_types () =
  (* Is returning a string but expected a int *)
  let code = "let sum (a: string) (b: int): string -> int -> int = a;" in
  try
    Lex.parse ~code () |> ignore;
    assert false
  with
  | Typing.Error.InvalidType _ ->
    assert true;;

let invalid_types () =
  (* First input is a string and a int? = Type error *)
  let code = "let sum (a: string) (b: int): int -> int -> int = a;" in
  try
    Lex.parse ~code () |> ignore;
    assert false
  with
  | Typing.Error.InvalidType _ ->
    assert true;;

let apply_fun_types () =
  (* First input is a string and a int? = Type error *)
  let code =
    "let sum (a: int): int -> int = 10; sum 10" in
  let t = Lex.parse ~code () in

  (*
    :: Apply ("sum", Const (VString "\"Hello World\"") :: [])
  *)
  let open Ast.Ast in
  let exp = Fun ("sum", TFTyp (FTFun (TInt :: TInt :: [])), PTyp ("a", TTyp TInt) :: [], Const (VInt 10))
            :: Apply ("sum", Const (VInt 5) :: [])
            :: [] in
  let h = function
    | Apply (n, _) ->
      Printf.printf "Eval: %s\n" n
    | _ ->  ()
  in
  List.nth t 1 |> h;

  assert (exp = t);;
