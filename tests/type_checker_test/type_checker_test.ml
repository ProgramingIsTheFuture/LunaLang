let%test "consts" =
  Parser.of_string "let _ = 10"
  |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun (a : Ast.Typed.t) ->
         let open Ast.Typed in
         match a with Let a -> a.typ = Ast.Typed.Tint)
  |> List.fold_left (fun a b -> a && b) true

let%test "if_expr" =
  Parser.of_string "let _ = if true then 10 else 20"
  |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun a ->
         let open Ast.Typed in
         match a with Let a -> a.typ = Ast.Typed.Tint)
  |> List.fold_left (fun a b -> a && b) true

let%test "Tvar" =
  Parser.of_string "let _ = let a = 10 in a"
  |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun a ->
         let open Ast.Typed in
         match a with Let a -> a.typ = Tint)
  |> List.fold_left (fun a b -> a && b) true

let%expect_test "let" =
  Parser.of_string "let f a = a; let b = f 10; let c = f false;"
  |> Parser.parse
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.iter (fun a -> Ast.Typed.pp_t a |> print_string |> flush_all);
  [%expect
    {|
    let f : 'a -> 'a = [typ 'a -> 'a](fun (a) -> ([typ 'a]a))
    ;let b : int = [typ int](([typ int -> int]f) ([typ int]10));let c : bool = [typ bool](([typ bool -> bool]f) ([typ bool]false)); |}]

let%test "wrong_application" =
  let parsed = Parser.of_string "let _ = 1 2" |> Parser.parse in
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  match
    Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty) parsed
  with
  | exception Type_checker.Errors.TypeError _ -> true
  | (exception _) | _ -> false

let%test "wrong_application2" =
  let parsed = Parser.of_string "let f a = a; let _ = 1 f" |> Parser.parse in
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  match
    Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty) parsed
  with
  | exception Type_checker.Errors.TypeError _ -> true
  | (exception _) | _ -> false

let%expect_test "runtime" =
  Parser.of_string "let _ = printint 15"
  |> Parser.parse
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  |> Type_checker.type_ast
       ~env:
         (Type_checker.Env.empty
            (Runtime.empty |> Runtime.add "printint" (Tarrow (Tint, Tint))))
  |> List.iter (fun a -> Ast.Typed.pp_t a |> print_string |> flush_all);
  [%expect
    {|
    let _ : int = [typ int](([typ int -> int]printint) ([typ int]15)); |}]
