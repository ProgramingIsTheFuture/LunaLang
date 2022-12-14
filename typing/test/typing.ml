open Ast.TypedAst

let test_cases_stmt =
  [
    ( "let int with type",
      "let a = 10;",
      [
        {
          desc = Let ("a", { desc = Const (VInt (Int64.of_int 10)); typ = TInt });
          typ = TInt;
        };
      ] );
    ( "let int + let string + let = str + var",
      "let a = 10; let b = \"Hello World\"; let c = a; c",
      [
        {
          desc = Let ("a", { desc = Const (VInt (Int64.of_int 10)); typ = TInt });
          typ = TInt;
        };
        {
          desc =
            Let
              ("b", { desc = Const (VString "\"Hello World\""); typ = TString });
          typ = TString;
        };
        { desc = Let ("c", { desc = Var "a"; typ = TInt }); typ = TInt };
        { desc = Var "c"; typ = TInt };
      ] );
    ( "Identity",
      "let f x = x;",
      [
        {
          desc =
            Let
              ( "f",
                {
                  desc =
                    Fun
                      ( "x",
                        { desc = Var "x"; typ = TVar { def = None; id = 0 } } );
                  typ =
                    TSeq
                      (TVar { def = None; id = 0 }, TVar { def = None; id = 0 });
                } );
          typ = TSeq (TVar { def = None; id = 0 }, TVar { def = None; id = 0 });
        };
      ] );
    ( "Fun with params",
      "let add (a:int): int->int = 10 + a; add 5",
      [
        {
          desc =
            Let
              ( "add",
                {
                  desc =
                    Fun
                      ( "a",
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
              ( "add",
                {
                  desc =
                    Fun
                      ( "a",
                        {
                          desc =
                            Fun
                              ( "b",
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
    ( "Inference and polymorphism",
      "let add a = a; let b = add 10; add false",
      [
        {
          desc =
            Let
              ( "add",
                {
                  desc =
                    Fun
                      ( "a",
                        { desc = Var "a"; typ = TVar { def = None; id = 0 } } );
                  typ =
                    TSeq
                      (TVar { def = None; id = 0 }, TVar { def = None; id = 0 });
                } );
          typ = TSeq (TVar { def = None; id = 0 }, TVar { def = None; id = 0 });
        };
        {
          desc =
            Let
              ( "b",
                {
                  desc =
                    Apply
                      ( "add",
                        [
                          { desc = Const (VInt (Int64.of_int 10)); typ = TInt };
                        ] );
                  typ = TInt;
                } );
          typ = TInt;
        };
        {
          desc = Apply ("add", [ { desc = Const (VBool false); typ = TBool } ]);
          typ = TBool;
        };
      ] );
  ]

let () =
  test_cases_stmt
  |> List.map (fun (ss, ff, expt) ->
         let result = Lex.parse ~code:ff () |> Typing.check_types in
         (ss, expt, result))
  |> Ast_test.run_typedast "Typing tests" "Stmt"
