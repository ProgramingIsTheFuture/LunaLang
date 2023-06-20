let%test "consts" =
  Parser.of_string "10" |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun a ->
         let open Ast.Typed in
         a.typ = Ast.Typed.Tint)
  |> List.fold_left (fun a b -> a && b) true

let%test "if_expr" =
  Parser.of_string "if true then 10 else 20"
  |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun a ->
         let open Ast.Typed in
         a.typ = Ast.Typed.Tint)
  |> List.fold_left (fun a b -> a && b) true

let%test "Tvar" =
  Parser.of_string "let a = 10 in a"
  |> Parser.parse
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.map (fun a ->
         let open Ast.Typed in
         a.typ = Tint)
  |> List.fold_left (fun a b -> a && b) true

let%expect_test "let" =
  Parser.of_string "let f a = a; let b = f 10; let c = f false;"
  |> Parser.parse
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  |> Type_checker.type_ast ~env:(Type_checker.Env.empty Runtime.empty)
  |> List.iter (fun a -> Ast.Typed.pp_t a |> print_string |> flush_all);
  [%expect
    {|
    [typ ()]let f = [typ 'a -> 'a](fun (a) -> ([typ 'a]a))
    ;
    [typ ()]let b = [typ int](([typ int -> int]f) ([typ int]10));
    [typ ()]let c = [typ bool](([typ bool -> bool]f) ([typ bool]false)); |}]

let%expect_test "runtime" =
  Parser.of_string "printint 15"
  |> Parser.parse
  (* |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all); *)
  |> Type_checker.type_ast
       ~env:
         (Type_checker.Env.empty
            (Runtime.empty |> Runtime.add "printint" (Tarrow (Tint, Tint))))
  |> List.iter (fun a -> Ast.Typed.pp_t a |> print_string |> flush_all);
  [%expect {|
    [typ int](([typ int -> int]printint) ([typ int]15)) |}]
