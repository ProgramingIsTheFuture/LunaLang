open Ast

let default_pos : pos = { starts = 0; ends = 0; line = 0 }

(** (Code, Expected AST) list *)
let test_cases : (string * string * Ast.ut_code) list =
  [
    (* Example 1 *)
    ( "Triple variable code",
      (* Code *)
      "let a = 10; \n let b = \"Hello World\"; \n let c = false; \n",
      (* Expected AST *)
      [
        {
          desc =
            Let
              ( "a",
                TTyp None,
                { desc = Const (VInt (Int64.of_int 10)); pos = default_pos } );
          pos = default_pos;
        };
        {
          desc =
            Let
              ( "b",
                TTyp None,
                { desc = Const (VString "\"Hello World\""); pos = default_pos }
              );
          pos = default_pos;
        };
        {
          desc =
            Let
              ("c", TTyp None, { desc = Const (VBool false); pos = default_pos });
          pos = default_pos;
        };
      ] );
    (* Another example *)
    ( "Let as fun with syntatic sugar sugar",
      "let sum (a: int): int = a;",
      [
        {
          desc =
            Let
              ( "sum",
                TTyp (Some "int"),
                {
                  desc =
                    Fun
                      ( "a",
                        TTyp (Some "int"),
                        { desc = Var "a"; pos = default_pos } );
                  pos = default_pos;
                } );
          pos = default_pos;
        };
      ] );
    (* Arithmetic example *)
    (*
  "(5 + 5) / 2"
   *)
    ( "Basic arithmetic 5+5",
      "(5 + 5) / 2",
      [
        {
          desc =
            Op
              ( {
                  desc =
                    Op
                      ( {
                          desc = Const (VInt (Int64.of_int 5));
                          pos = default_pos;
                        },
                        Add,
                        {
                          desc = Const (VInt (Int64.of_int 5));
                          pos = default_pos;
                        } );
                  pos = default_pos;
                },
                Div,
                { desc = Const (VInt (Int64.of_int 2)); pos = default_pos } );
          pos = default_pos;
        };
      ] );
    (* Arithmetic with variables *)
    ( "Variable with arithmetic 'a + 10'",
      "let a = 10;\n a + 10",
      [
        {
          desc =
            Let
              ( "a",
                TTyp None,
                { desc = Const (VInt (Int64.of_int 10)); pos = default_pos } );
          pos = default_pos;
        };
        {
          desc =
            Op
              ( { desc = Var "a"; pos = default_pos },
                Add,
                { desc = Const (VInt (Int64.of_int 10)); pos = default_pos } );
          pos = default_pos;
        };
      ] );
    ( "Anonymous functions",
      "let a = (x: int) -> x + 1;",
      [
        {
          desc =
            Let
              ( "a",
                TTyp None,
                {
                  desc =
                    AnFun
                      ( "x",
                        TTyp (Some "int"),
                        {
                          desc =
                            Op
                              ( { desc = Var "x"; pos = default_pos },
                                Add,
                                {
                                  desc = Const (VInt (Int64.of_int 1));
                                  pos = default_pos;
                                } );
                          pos = default_pos;
                        } );
                  pos = default_pos;
                } );
          pos = default_pos;
        };
      ] );
    ( "Anonymous functions into a typed variable",
      "let add: int -> int -> int = (x: int) -> (y: int) -> x + y;",
      [
        {
          desc =
            Let
              ( "add",
                TTyp (Some "int -> int -> int"),
                {
                  desc =
                    AnFun
                      ( "x",
                        TTyp (Some "int"),
                        {
                          desc =
                            AnFun
                              ( "y",
                                TTyp (Some "int"),
                                {
                                  desc =
                                    Op
                                      ( { desc = Var "x"; pos = default_pos },
                                        Add,
                                        { desc = Var "y"; pos = default_pos } );
                                  pos = default_pos;
                                } );
                          pos = default_pos;
                        } );
                  pos = default_pos;
                } );
          pos = default_pos;
        };
      ] );
    ( "let Block",
      "let b = {\n      let a = 10;\n      add a\n    };",
      [
        {
          desc =
            Let
              ( "b",
                TTyp None,
                {
                  desc =
                    Block
                      [
                        {
                          desc =
                            Let
                              ( "a",
                                TTyp None,
                                {
                                  desc = Const (VInt (Int64.of_int 10));
                                  pos = default_pos;
                                } );
                          pos = default_pos;
                        };
                        {
                          desc =
                            Apply
                              ("add", [ { desc = Var "a"; pos = default_pos } ]);
                          pos = default_pos;
                        };
                      ];
                  pos = default_pos;
                } );
          pos = default_pos;
        };
      ] );
    ( "let fun Block",
      "let b a = {\n      add a\n    };",
      [
        {
          desc =
            Let
              ( "b",
                TTyp None,
                {
                  desc =
                    Fun
                      ( "a",
                        TTyp None,
                        {
                          desc =
                            Block
                              [
                                {
                                  desc =
                                    Apply
                                      ( "add",
                                        [
                                          { desc = Var "a"; pos = default_pos };
                                        ] );
                                  pos = default_pos;
                                };
                              ];
                          pos = default_pos;
                        } );
                  pos = default_pos;
                } );
          pos = default_pos;
        };
      ] );
  ]

let () =
  let t =
    test_cases
    |> List.map (fun (ss, ff, expt) -> (ss, Parse.parse ~code:ff (), expt))
  in
  ignore t
(* Ast_test.run_ast "Lex tests" t;; *)
