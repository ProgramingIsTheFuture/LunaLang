%{
  open Ast.Ast
%}

%token <Ast.Ast.value> VALUE
%token <string> NAME
%token LET FOR IF ELSE RBRACE LBRACE RPARENT LPARENT BREAK
%token DOUBLEDOT QUESTION SEMICOLON EQUAL RARROW
%token EOF

%start code

%type <code> code

%%

code: c = list(stmt) EOF { c }

stmt:
  | LPARENT e = stmt RPARENT { e }
  | v = VALUE { Expr (Const v) }
  | n = NAME e = list(stmt)
    {
      match e with
      | [] -> Expr (Var n)
      | l ->
        Expr (Apply (n, l))
    }
  | v = variable SEMICOLON { v }
  | f = func SEMICOLON { f }

typ:
  | n1 = NAME?
    { match n1 with
      | Some v -> TTyp v
      | None -> TInference
    }

variable:
  | LET n = NAME EQUAL e = stmt
    { Let (n, TInference, e) }
  | LET n1 = NAME DOUBLEDOT n2 = typ EQUAL e = stmt
    { Let (n1, n2, e) }

params:
  | n = NAME
    { PName n }
  | LPARENT n1 = NAME DOUBLEDOT n2 = typ RPARENT
    { PTyp (n1, n2) }

func:
  | LET n = NAME p = list(params) EQUAL e = stmt
    { Fun (n, TInference, p, e) }
  | LET n1 = NAME p = list(params) DOUBLEDOT n2 = typ EQUAL e = stmt
    { Fun (n1, n2, p, e) }
