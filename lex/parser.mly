%{
  open Ast
%}

%token <Ast.value> VALUE
%token EOF

%start code

%type <Ast.code> code

%%

expr:
  | v = VALUE { Const v }

code: c = list(expr) EOF { c }
