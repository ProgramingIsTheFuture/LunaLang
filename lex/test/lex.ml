open Ast.Ast

(** (Code, Expected AST) list *)
let test_cases: (string * string * Ast.Ast.desc list) list = [
  (* Example 1 *)
  (
  "Triple variable code",
  (* Code *)
  "let a = 10; \n let b = \"Hello World\"; \n let c = false; \n",
  (* Expected AST *)
  Let (("a", TTyp None), 
    Const (VInt (Int64.of_int 10)))
  :: Let (("b", TTyp None),
      Const (VString "\"Hello World\""))
  :: Let (("c", TTyp None), 
      Const (VBool false))
  :: []
  );
  (* Another example *)
  (
    "Let as fun with syntatic sugar sugar",
    "let sum (a: int): int = a;",
    Let (("sum", TTyp (Some "int")), 
      Fun (("a", TTyp (Some "int")), 
        Var "a"))
    :: []
  );
  (* Arithmetic example *)
  (*
  "(5 + 5) / 2"
   *)
  (
    "Basic arithmetic 5+5",
    "(5 + 5) / 2",
    Op (
      Op (
        Const (VInt (Int64.of_int 5)), 
        Add, 
        Const(VInt (Int64.of_int 5))), 
      Div,
      Const(VInt (Int64.of_int 2)))
    :: []
  );
  (* Arithmetic with variables *)
  (
    "Variable with arithmetic 'a + 10'",
    "let a = 10;\n a + 10",
    Let (("a", TTyp None), 
        Const (VInt (Int64.of_int 10)))
    :: Op (
        Var "a", 
        Add, 
        Const (VInt (Int64.of_int 10)))
    :: []
  )
]

let () =
  let t = test_cases 
  |> List.map 
    (fun (ss, ff, expt) -> 
      (ss, Ast_test.desc_of_astcode (Lex.parse ~code:(ff) ()), expt)
    ) in
  Ast_test.run_ast "Lex tests" t;;
