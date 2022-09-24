%{
  open Ast
%}

%token <Ast.value> VALUE

%token LET FOR IF ELSE RBRACE LBRACE RPARENT LPARENT BREAK
%token DOUBLEDOT SEMICOLON EQUAL ARROW
%token EOF

%start code

%type <Ast.code> code

%%

expr:
  | v = VALUE { Const v }

code: c = list(expr) EOF { c }
