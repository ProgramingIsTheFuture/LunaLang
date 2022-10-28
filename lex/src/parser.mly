%{
  open Ast.Ast

  let loc_pos (loc: Lexing.position * Lexing.position): pos =
    {
      starts = (fst loc).pos_bol;
      line = (snd loc).pos_lnum;
      ends = (snd loc).pos_bol;
    };;

  let to_stmt (desc: desc) (loc: Lexing.position * Lexing.position): stmt =
    {desc = desc; pos = loc_pos loc}
%}

%token <Ast.Ast.value> VALUE
%token <string> NAME
%token LET FOR IF ELSE RBRACE LBRACE RPARENT LPARENT BREAK
%token ADD SUB DIV MUL MOD
%token DOUBLEDOT QUESTION SEMICOLON EQUAL RARROW
%token EOF

%start code

%type <code> code

%%

code: c = list(stmt) EOF { c }

stmt:
  | s = desc { to_stmt s $loc }

desc:
  | LPARENT e = desc RPARENT { print_string "PARENTS\n"; e }
  | e = expr { print_string "No Parenms\n"; e }
  | v = variable SEMICOLON { v }
  | f = fun_syntatic_sugar SEMICOLON { f }

expr:
  | o = op { print_string "Operator\n"; o }
  | v = VALUE { Const v }
  | n = NAME e = list(desc)
    {
      match e with
      | [] -> Var n
      | l ->
        Apply (n, l)
    }

opop:
  | ADD { Add }
  | SUB { Sub }
  | DIV { Div }
  | MUL { Mul }
  | MOD { Mod }

op:
  | LPARENT e1 = expr RPARENT oo = opop e2 = expr 
    { print_string "L e R o e\n"; Op (e1, oo, e2) }
  | LPARENT e1 = expr oo = opop e2 = expr RPARENT
    { print_string "L o R\n"; Op (e1, oo, e2) }
  | e1 = expr oo = opop e2 = expr
    { print_string "e o e\n"; Op (e1, oo, e2) }

typ:
  | DOUBLEDOT n1 = NAME { TTyp (Some n1) }
  | n1 = NAME {
    TTyp (Some n1)
  }

variable:
  | LET n = NAME EQUAL e = desc
    { Let ((n, TTyp None), e) }
  | LET n1 = NAME DOUBLEDOT n2 = typ EQUAL e = desc
    { Let ((n1, n2), e) }

funfun:
  | n = NAME
    { (n, None) }
  | LPARENT n = NAME DOUBLEDOT t = typ RPARENT
    {
      (n, Some t)
    }

fun_syntatic_sugar:
  | LET n = NAME f = list(funfun) EQUAL e = desc 
    { 
      let rec h = function
        | [] -> e
        | (nm, None) :: tl ->
          Fun ((nm, TTyp None), h tl)
        | (nm, Some typ) :: tl ->  
          Fun ((nm, typ), h tl)
      in 

      Let ((n, TTyp None), h f)
    }
  | LET n1 = NAME f = list(funfun) DOUBLEDOT n2 = typ EQUAL e = desc
    { 
      let rec h = function
        | [] -> e
        | (nm, None) :: tl ->
          Fun ((nm, TTyp None), h tl)
        | (nm, Some typ) :: tl ->  
          Fun ((nm, typ), h tl)
      in
      Let ((n1, n2), h f)
    }

