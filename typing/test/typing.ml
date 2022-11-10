open Ast.TypedAst

let test_cases_desc = [
  (
    "Variable type",
    "let a = 10;",
    Let (("a", TInt), Const (VInt (Int64.of_int 10)))
    :: []
  );
  (
    "Variable int type with type",
    "let a: int = 10;",
    Let (("a", TInt), Const (VInt (Int64.of_int 10)))
    :: []
  );
  (
    "String let",
    "let a: string = \"Hello World\";",
    Let (("a", TString), Const (VString "\"Hello World\""))
    :: []
  );
];;

let test_cases_stmt = [
  (
    "let int + let string + let = str + var ",
    "let a: int = 10; let b = \"Hello World\"; let c = b; c",
    { desc = Let (
        ("a", TInt), 
        Const (
          VInt 
          (Int64.of_int 10)
        )
      ); 
      typ = TInt 
    }
    :: {
      desc = Let (
        ("b", TString),
        Const (VString "\"Hello World\"")
      );
      typ = TString
    }
    :: { desc = Let (("c", TString), Var("b")); typ = TString }
    :: { desc = Var ("c"); typ = TString }
    :: []
  );
  (* ( *)
  (*   "Fun with params", *)
  (*   "let add (a: int): int = 10 + a;", *)
  (*   { desc = Let ( *)
  (*       ("add", TInt), *)
  (*       Op (Var("a"), Add, Var("b")) *)
  (*     ); *)
  (*     typ = TSeq (TInt, Some TInt)  *)
  (*   } *)
  (*   :: { *)
  (*     desc = Apply ( *)
  (*       "add",   *)
  (*       Const ( *)
  (*         VInt  *)
  (*         (Int64.of_int 10) *)
  (*       ) *)
  (*       :: Const ( *)
  (*         VInt  *)
  (*         (Int64.of_int 15) *)
  (*       ) *)
  (*       :: [] *)
  (*     ); *)
  (*     typ = TInt *)
  (*   } *)
  (*   :: [] *)
  (* ) *)
];;

let () =
  let t1 = test_cases_desc 
  |> List.map 
    (fun (ss, ff, expt) -> 
      (ss, Ast_test.desc_of_typedcode (Lex.parse ~code:(ff) () |> Typing.check_types), expt)
    ) in
  let t2 = test_cases_stmt 
  |> List.map 
    (fun (ss, ff, expt) -> 
      (ss, (Lex.parse ~code:(ff) () |> Typing.check_types), expt)
    ) in
  Ast_test.run_typedast "Typing tests" "Desc" "Stmt" t1 t2;;
