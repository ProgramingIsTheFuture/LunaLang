open Ast.TypedAst

let test_cases_desc = [
  (
    "Variable type",
    "let a = 10;",
    Let (("a", TSeq(TInt, None)), Const (VInt (Int64.of_int 10)))
    :: []
  );
  (
    "Variable int type with type",
    "let b: int = 10;",
    Let (("b", TSeq(TInt, None)), Const (VInt (Int64.of_int 10)))
    :: []
  );
  (
    "String let",
    "let d: string = \"Hello World\";",
    Let (("d", TSeq(TString, None)), Const (VString "\"Hello World\""))
    :: []
  );
  (
    "aritmetic",
    "let f: int = 10; f + 5",
    Let (("f", TSeq(TInt, None)), Const(VInt (Int64.of_int 10)))
    :: Op(Var("f"), Add, Const(VInt (Int64.of_int 5)))
    :: []
  )
];;

(**
  * "let b = \"Hello World\"; let c = a; c "
*
    :: { desc = Let (("c", TSeq(TInt, None)), Var("a")); typ = TSeq(TInt, None) }
    :: { desc = Var ("c"); typ = TSeq(TInt, None) }
 *)

let test_cases_stmt = [
  (
    "let int + let string + let = str + var",
    "let a = 10; let b = \"Hello World\"; let c = a; c",
    { desc = Let (
        ("a", TSeq(TInt, None)), 
        Const (
          VInt 
          (Int64.of_int 10)
        )
      ); 
      typ = TSeq(TInt, None) 
    }
   :: {
      desc = Let (
        ("b", TSeq(TString, None)),
        Const (VString "\"Hello World\"")
      );
      typ = TSeq(TString, None)
    }   
    :: { desc = Let (("c", TSeq(TInt, None)), Var("a")); typ = TSeq(TInt, None) } 
    :: { desc = Var ("c"); typ = TSeq(TInt, None)}
    :: []
  );
  (
    "Fun with params",
    "let add (a:int): int = 10 + a; add 5",
    { desc = Let (
        ("add", TSeq(TInt, Some (TSeq(TInt, None)))),
        Fun(("a", TSeq (TInt, None)),
          Op (
            Const (
              VInt 
              (Int64.of_int 10)
            )
            , Add, Var("a")
          )
        )
      );
      typ = TSeq (TInt, Some (TSeq(TInt, None))) 
    }
    :: {
      desc = Apply (
        "add",  
        Const (
          VInt 
          (Int64.of_int 5)
        )
        :: []
      );
      typ = TSeq (TInt, None)
    }
    :: []
  );
  (
    "Fun with Multi params",
    "let add (a:int) (b: int): int = b + a; add 5 10",
    { desc = Let (
        ("add", TSeq(TInt, Some (TSeq(TInt, Some (TSeq (TInt, None)))))),
        Fun(("a", TSeq (TInt, None)),
          Fun (("b", TSeq (TInt, None)), 
            Op (
              Var ("b"), Add, Var("a")
            )
          )
        )
      );
      typ = TSeq (TInt, Some (TSeq(TInt, Some (TSeq (TInt, None))))) 
    }
    :: {
      desc = Apply (
        "add",  
        Const (
          VInt 
          (Int64.of_int 5)
        ) 
        :: Const (
          VInt 
          (Int64.of_int 10)
        )
        :: []
      );
      typ = TSeq (TInt, None)
    }
    :: []
  )

];;

let () =
  let t1 = test_cases_desc 
  |> List.map 
    (fun (ss, ff, expt) -> 
      let result = Ast_test.desc_of_typedcode (Lex.parse ~code:(ff) () |> Typing.check_types) in
      (ss, expt, result)
    ) in
  let t2 = test_cases_stmt 
  |> List.map 
    (fun (ss, ff, expt) -> 
      let result = (Lex.parse ~code:(ff) () |> Typing.check_types) in
      (ss, expt, result)
    ) in
  Ast_test.run_typedast "Typing tests" "Desc" "Stmt" t1 t2;;
