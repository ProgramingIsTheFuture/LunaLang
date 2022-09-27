%{
  open Ast
%}

%token <Ast.value> VALUE
%token <string> NAME
%token LET FOR IF ELSE RBRACE LBRACE RPARENT LPARENT BREAK
%token DOUBLEDOT QUESTION SEMICOLON EQUAL RARROW
%token EOF

%start code

%type <Ast.code> code

%%

code: c = list(expr) EOF { c }

expr:
  | LPARENT e = expr RPARENT { e }
  | v = VALUE { Const v }
  | n = NAME e = list(expr)
    {
      match e with
      | [] -> Var n
      | l ->
         Printf.printf "Apply\n";
        Apply (n, l)
    }
  | v = variable SEMICOLON { v }
  | f = func SEMICOLON { f }

vtyp:
  | n1 = NAME
    {
      match n1 with
      | "int" -> TInt
      | "string" -> TString
      | "bool" -> TBool
      | _ -> raise (Error.InvalidType (Format.sprintf "Invalid type: %s\n" n1))
    }

ftyp:
  | t1 = separated_list(RARROW,vtyp)
    {FTFun t1}

typ:
  | f = ftyp
    { TFTyp f }
  | n1 = vtyp?
    { match n1 with
      | Some v -> TTyp v
      | None -> TInference
    }

variable:
  | LET n = NAME EQUAL e = expr
    { Let (n, TInference, e) }
  | LET n1 = NAME DOUBLEDOT n2 = typ EQUAL e = expr
    { Let (n1, n2, e) }

params:
  | n = NAME
    { PName n }
  | LPARENT n1 = NAME DOUBLEDOT n2 = typ RPARENT
    { PTyp (n1, n2) }

func:
  | LET n = NAME p = list(params) EQUAL e = expr
    { Fun (n, TInference, p, e) }
  | LET n1 = NAME p = list(params) DOUBLEDOT n2 = typ EQUAL e = expr
    { Fun (n1, n2, p, e) }
