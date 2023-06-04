%{ 
  open Ast.Parsing

  let position (starts, ends) = 
    let open Ast.Common in
    {starts; ends }
%}

%token LET "let" 
%token FUN "fun" 
%token IF "if" 
%token THEN "then" 
%token ELSE "else" 
%token IN "in" 
%token EQUAL "=" 
%token ARROW "->" 
%token <Ast.Common.name> NAME
%token <Ast.Common.values> VALUE

%start <t>main

%%

main:
  | e = expr_t 
    { e }

expr_t:
  | e = expr
   { {expr = e; pos = position ($startofs(e), $endofs(e))} }

expr:
  | v = VALUE
    { Const v }
  | "let" name = NAME "=" e1 = expr_t "in" e2 = expr_t
    { LetIn (name, None, e1, e2) }
  | "fun" n = NAME "->" e1 = expr_t
    { Fun (n, None, e1) }
  | "if" e1 = expr_t "then" e2 = expr_t "else" e3 = expr_t
    { IfThen (e1, e2, e3) }
