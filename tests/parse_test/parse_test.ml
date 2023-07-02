let%expect_test "Simple let" =
  Parser.of_string "let a = 10;"
  |> Parser.parse |> List.hd |> Ast.Parsing.pp_t |> print_string |> flush_all;
  [%expect {|
    let a = 10; |}]

let%expect_test "Simple fun" =
  Parser.of_string "let a = fun i -> i;"
  |> Parser.parse |> List.hd |> Ast.Parsing.pp_t |> print_string |> flush_all;
  [%expect {|
    let a = fun i -> i; |}]

let%expect_test "Syntatic Sugar" =
  Parser.of_string "let a i = i"
  |> Parser.parse |> List.hd |> Ast.Parsing.pp_t |> print_string |> flush_all;
  [%expect {|
    let a = fun i -> i; |}]

let%expect_test "Let unit expr" =
  Parser.of_string "let a = 10; let _ = a"
  |> Parser.parse
  |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all);
  [%expect {|
    let a = 10;let _ = a; |}]

let%expect_test "Variable" =
  Parser.of_string "let _ = a"
  |> Parser.parse
  |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all);
  [%expect {|
    let _ = a; |}]

let%expect_test "Let in" =
  Parser.of_string "let _ = let a = 10 in a"
  |> Parser.parse
  |> List.iter (fun a -> Ast.Parsing.pp_t a |> print_string |> flush_all);
  [%expect {| let _ = let a = 10 in a; |}]

let%expect_test "apply" =
  Parser.of_string "let _ = let f a = a in f true"
  |> Parser.parse |> List.hd |> Ast.Parsing.pp_t |> print_string |> flush_all;
  [%expect {|
  let _ = let f = fun a -> a in (f true); |}]
