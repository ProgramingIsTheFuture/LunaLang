open Ast.TypedAst

let test_cases_stmt =
  [
    ( "let int with type",
      "let a:int = 10;",
      [
        {
          desc =
            Let
              ( ("a", TInt),
                { desc = Const (VInt (Int64.of_int 10)); typ = TInt } );
          typ = TInt;
        };
      ] );
    ( "let int + let string + let = str + var",
      "let a = 10; let b = \"Hello World\"; let c = a; c",
      [
        {
          desc =
            Let
              ( ("a", TInt),
                { desc = Const (VInt (Int64.of_int 10)); typ = TInt } );
          typ = TInt;
        };
        {
          desc =
            Let
              ( ("b", TString),
                { desc = Const (VString "\"Hello World\""); typ = TString } );
          typ = TString;
        };
        { desc = Let (("c", TInt), { desc = Var "a"; typ = TInt }); typ = TInt };
        { desc = Var "c"; typ = TInt };
      ] );
    ( "Fun with params",
      "let add (a:int): int = 10 + a; add 5",
      [
        {
          desc =
            Let
              ( ("add", TSeq (TInt, TInt)),
                {
                  desc =
                    Fun
                      ( ("a", TInt),
                        {
                          desc =
                            Op
                              ( {
                                  desc = Const (VInt (Int64.of_int 10));
                                  typ = TInt;
                                },
                                Add,
                                { desc = Var "a"; typ = TInt } );
                          typ = TInt;
                        } );
                  typ = TSeq (TInt, TInt);
                } );
          typ = TSeq (TInt, TInt);
        };
        {
          desc =
            Apply
              ("add", [ { desc = Const (VInt (Int64.of_int 5)); typ = TInt } ]);
          typ = TInt;
        };
      ] );
    ( "Fun with Multi params",
      "let add (a: int) (b: int): int = a + b; add 5 10",
      [
        {
          desc =
            Let
              ( ("add", TSeq (TInt, TSeq (TInt, TInt))),
                {
                  desc =
                    Fun
                      ( ("a", TInt),
                        {
                          desc =
                            Fun
                              ( ("b", TInt),
                                {
                                  desc =
                                    Op
                                      ( { desc = Var "a"; typ = TInt },
                                        Add,
                                        { desc = Var "b"; typ = TInt } );
                                  typ = TInt;
                                } );
                          typ = TSeq (TInt, TInt);
                        } );
                  typ = TSeq (TInt, TSeq (TInt, TInt));
                } );
          typ = TSeq (TInt, TSeq (TInt, TInt));
        };
        {
          desc =
            Apply
              ( "add",
                [
                  { desc = Const (VInt (Int64.of_int 5)); typ = TInt };
                  { desc = Const (VInt (Int64.of_int 10)); typ = TInt };
                ] );
          typ = TInt;
        };
      ] );
  ]

let () =
  test_cases_stmt
  |> List.map (fun (ss, ff, expt) ->
         let result = Lex.parse ~code:ff () |> Typing.check_types in
         (ss, expt, result))
  |> Ast_test.run_typedast "Typing tests" "Stmt"
