(*
let complex_types () =
  (* Is returning a string but expected a int *)
  let code = "let sum (a: int) (b: int): int = a;" in
  try
    let open Ast.TypedAst in
    Lex.parse ~code () |>
    Typing.check_types
    |> List.map2 
      (fun a b -> assert (a = b.desc))
      (
        Let(
          ("sum", TInt), 
          Fun (("a", TInt), 
            Fun(("b", TInt), 
              Var "a"))) :: [])
      |> ignore;
  with
  | Typing.Error.InvalidType _ ->
    assert false;;

let invalid_types () =
  (* First input is a string and a int? = Type error *)
  let code = "let sum (a: string) (b: int): int = a;" in
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
  let exp = Let (("a", TInt), Const (VInt (Int64.of_int 10)))
            :: Op (Var("a"), Add, Const (VInt (Int64.of_int 10))) :: [] in

  List.map2 (fun tc ex -> assert (tc.desc = ex)) typedAst exp |> ignore;;

let sequential_types () =
  let code = "let sum a b = a + b" in
  let typedAst = Lex.parse ~code () 
    |> Typing.check_types in

  let open Ast.TypedAst in
  let exp = Let (("sum", TInt), 
    Fun(("a", TInt), 
      Fun(("b", TInt), 
        Op (Var "a", Add, Var "b")))) 
        :: [] in

  List.map2 
    (fun tc ex -> 
      assert ((tc.desc = ex) 
      && (tc.typ = TSeq (TInt, 
        Some (TSeq (TInt, 
          Some (TSeq (TInt, 
            None)))))) )) 
    typedAst exp |> ignore;;

let () =
  [complex_types; invalid_types; operator_types; sequential_types;]
  |> List.map (fun a -> a ())
  |> ignore;;
 *)
