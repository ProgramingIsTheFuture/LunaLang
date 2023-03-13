%{
  open Ast

  let loc_pos (loc: Lexing.position * Lexing.position): pos =
    {
      starts = (fst loc).pos_bol;
      line = (snd loc).pos_lnum;
      ends = (snd loc).pos_bol;
    };;

  let to_stmt (desc: ut_desc) (loc: Lexing.position * Lexing.position): ut_stmt =
    {desc = desc; pos = loc_pos loc}
%}

%token <Ast.value> VALUE
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

%type <ut_code> code

%%

code: c = list(desc) EOF { c }

desc:
  | "(" e = desc ")" { e }
  | e = expr { to_stmt e $loc }
  | b = block { to_stmt b $loc }
  | v = variable ";" { to_stmt v $loc }
  | f = fun_syntatic_sugar ";" { to_stmt f $loc }
  | af = anonymous_fun { to_stmt af $loc }


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
    { 
      let e1 = to_stmt e1 $loc in
      let e2 = to_stmt e2 $loc in
      Op (e1, oo, e2) 
    }
  | "(" e1 = expr oo = opop e2 = expr ")"
    { 
      let e1 = to_stmt e1 $loc in
      let e2 = to_stmt e2 $loc in
      Op (e1, oo, e2) 
    }
  | e1 = expr oo = opop e2 = expr
    {
      let e1 = to_stmt e1 $loc in
      let e2 = to_stmt e2 $loc in
      let ee:  ut_desc = Op (e1, oo, e2)  in ee
    }

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
    { Let (n, TTyp None, e) }
  | LET n1 = NAME ":" n2 = typ "=" e = desc
    { 
      let ee: Ast.ut_desc = Let (n1, n2, e) in ee
    }

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
          let e: ut_desc = Fun (nm, TTyp None, h tl) in
          to_stmt e $loc
        | (nm, Some typ) :: tl ->  
          let e: ut_desc = Fun (nm, typ, h tl) in
          to_stmt e $loc
      in 

      let ee: ut_desc = Let (n, TTyp None, h f) in ee
    }
  | LET n1 = NAME f = list(funfun) ":" n2 = typ "=" e = desc
    { 
      let rec h = function
        | [] -> e
        | (nm, None) :: tl ->
          let e: ut_desc = Fun (nm, TTyp None, h tl) in
          to_stmt e $loc
        | (nm, Some typ) :: tl ->  
          let e: ut_desc = Fun (nm, typ, h tl) in
          to_stmt e $loc
      in
      let ee: ut_desc = Let (n1, n2, h f) in ee
    }

anonymous_fun:
  | "(" n = NAME ":" t = typ ")" "->" e = desc 
    { AnFun (n, t, e) }
