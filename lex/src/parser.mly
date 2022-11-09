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
%token LET FOR IF ELSE 
%token LBRACE "{"
%token RBRACE "}" 
%token LPARENT "(" 
%token RPARENT ")" 
%token BREAK
%token ADD SUB DIV MUL MOD
%token EQUAL "="
%token DOUBLEDOT ":" 
%token QUESTION "?"
%token SEMICOLON ";" 
%token RARROW "->"
%token EOF

%start code

%type <code> code

%%

code: c = list(stmt) EOF { c }

stmt:
  | s = desc { to_stmt s $loc }

desc:
  | "(" e = desc ")" { e }
  | e = expr { e }
  | b = block { b }
  | v = variable ";" { v }
  | f = fun_syntatic_sugar ";" { f }
  | af = anonymous_fun { af }


(*
  syntax:
    expr:
      | operators
      | values as constants
      | calling functions (print a b c)
 *)
expr:
  | o = op { o }
  | v = VALUE { Const v }
  | n = NAME e = list(desc)
    {
      match e with
      | [] -> Var n
      | l ->
        Apply (n, l)
    }

block:
  | "{" l = list(desc) "}"
    {Block (l)}

opop:
  | ADD { Add }
  | SUB { Sub }
  | DIV { Div }
  | MUL { Mul }
  | MOD { Mod }

(* 
   syntax:

     op:
      | +
      | -
      | /
      ...

     (.. op ..)
     (..) op ..
     .. op ..
 *)
op:
  | "(" e1 = expr ")" oo = opop e2 = expr 
    { Op (e1, oo, e2) }
  | "(" e1 = expr oo = opop e2 = expr ")"
    { Op (e1, oo, e2) }
  | e1 = expr oo = opop e2 = expr
    { Op (e1, oo, e2) }

arrow_typ:
  | "->" n1 = NAME {
    n1
  }

typ:
  | ":" n1 = NAME { TTyp (Some n1) }
  | n1 = NAME l = list(arrow_typ)?
    {
      match l with
      | Some ll ->
        let v = List.fold_left (fun s n -> 
          s ^ " -> " ^ n
        ) n1 ll in
        TTyp (Some v) 
      | None -> 
        TTyp (Some n1) 
    }

(*
  Syntax:
  let n = ...
  * and
  let n: typ = ... 
 *)
variable:
  | LET n = NAME "=" e = desc
    { Let ((n, TTyp None), e) }
  | LET n1 = NAME ":" n2 = typ "=" e = desc
    { Let ((n1, n2), e) }

(* Helper *)
funfun:
  | n = NAME
    { (n, None) }
  | "(" n = NAME ":" t = typ ")"
    {
      (n, Some t)
    }

(* 
  sytnax:

  One param or multiple
  Typed or not
  funfun:
    | a 
    | (a: typ)

  let n funfun =
    ...

  * and 

  let n funfun : typ =
    ...
 *)
fun_syntatic_sugar:
  | LET n = NAME f = list(funfun) "=" e = desc 
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
  | LET n1 = NAME f = list(funfun) ":" n2 = typ "=" e = desc
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

anonymous_fun:
  | "(" n = NAME ":" t = typ ")" "->" e = desc 
    { AnFun (n, t, e) }
