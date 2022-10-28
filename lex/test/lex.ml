open Ast.Ast

(** (Code, Expected AST) list *)
let test_cases = [
  (* Example 1 *)
  (
  (* Code *)
  "let a = 10; \n let b = \"Hello World\"; \n let c = false; \n",
  (* Expected AST *)
  Let (("a", TTyp None), 
    Const (VInt (Int64.of_int 10)))
  :: Let (("b", TTyp None),
      Const (VString "\"Hello World\""))
  :: Let (("c", TTyp None), 
      Const (VBool false))
  :: [],
  "Triple variable code"
  );
  (* Another example *)
  (
    "let sum (a: int): int = a;",
    Let (("sum", TTyp (Some "int")), 
      Fun (("a", TTyp (Some "int")), 
        Var "a"))
    :: [],
    "Let as fun with syntatic sugar sugar"
  );
  (* Arithmetic example *)
  (*
  "(5 + 5) / 2"
   *)
  (
    "(5 + 5) / 2",
    Op (
      Op (
        Const (VInt (Int64.of_int 5)), 
        Add, 
        Const(VInt (Int64.of_int 5))), 
      Div,
      Const(VInt (Int64.of_int 2)))
    :: [],
    "Basic arithmetic 5+5"
  );
  (* Arithmetic with variables *)
  (
    "let a = 10;\n a + 10",
    Let (("a", TTyp None), 
        Const (VInt (Int64.of_int 10)))
    :: Op (
        Var "a", 
        Add, 
        Const (VInt (Int64.of_int 10)))
    :: [],
    "Variable with arithmetic 'a + 10'"
  )
]

exception Error of string

let () =
  test_cases
  |> List.map (fun (ff, ss, msg) -> 
      Format.printf "Testing %s\n\n" msg;
      let c = Lex.parse ~code:(ff) () in
      List.map2 
        (fun a b -> 
          if not (a.desc = b) then 
            raise (Error msg) 
          else Format.printf "Passed: %s\n" msg)
        c (ss)
     )
  |> ignore;;
