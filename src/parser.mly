/* Ocamlyacc parser for HeBGB (using MicroC template) */

%{
open Ast
%}

%token LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY IF DO ELSE COMMA QUOTE
%token PLUS MINUS TIMES DIVIDE MOD LT GT EQ LEQ GEQ NEQ
%token OR AND INT STRING CONT LIST
%token LARROW IN LSQUIGGLY RARROW CONTINUE WITH THEN
%token <int> LITINT
%token <string> LITSTR
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%left IN THEN ELSE
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%right RARROW
%left  LIST
%%

program:
  expr EOF { $1 }

expr:
    ID                                                  { Id($1) }
  | literal                                             { $1 }
  | infix                                               { $1 }
  | LPAREN expr RPAREN                                  { $2 }
  | typ ID LARROW expr IN expr                          { Bind($1, $2, $4, $6) }
  | ID LSQUIGGLY expr IN expr                           { Assign($1, $3, $5) }
  | typ ID LPAREN params_opt RPAREN RARROW expr IN expr { Fun($1, $2, $4, $7, $9) }
  | ID LPAREN args_opt RPAREN                           { App(Id($1), $3) }
  | LPAREN expr RPAREN LPAREN args_opt RPAREN           { App($2, $5) }
  | typ ID LARROW CONTINUE WITH ID IN expr THEN expr    { Cont($1, $2, $6, $8, $10) }
  | IF expr DO expr ELSE expr                           { If($2, $4, $6) }

infix:
    expr PLUS   expr { Infix($1, Add,     $3) }
  | expr MINUS  expr { Infix($1, Sub,     $3) }
  | expr TIMES  expr { Infix($1, Mul,     $3) }
  | expr DIVIDE expr { Infix($1, Div,     $3) }
  | expr MOD    expr { Infix($1, Mod,     $3) }
  | expr EQ     expr { Infix($1, Eq,      $3) }
  | expr NEQ    expr { Infix($1, Neq,     $3) }
  | expr LT     expr { Infix($1, Less,    $3) }
  | expr LEQ    expr { Infix($1, Leq,     $3) }
  | expr GT     expr { Infix($1, Greater, $3) }
  | expr GEQ    expr { Infix($1, Geq,     $3) }
  | expr AND    expr { Infix($1, And,     $3) }
  | expr OR     expr { Infix($1, Or,      $3) }

typ:
    typ_atom { $1 }
  | typ_atom RARROW arrow_list { let r = List.rev ($1::$3) in
			         TyFun (List.rev (List.tl r), List.hd r) }
  | typ_atom TIMES times_list { TyTup ($1::$3) }

arrow_list:
    typ_atom { [$1] }
  | arrow_list RARROW typ_atom { $3::$1 }

times_list:
    typ_atom { [$1] }
  | times_list TIMES typ_atom { $3::$1 }

typ_atom:
    LPAREN typ RPAREN { $2         }
  | INT               { TyInt      }
  | STRING            { TyStr      }
  | CONT              { TyCont     }
  | typ_atom LIST     { TyList($1) }

literal:
    LITINT                 { LitInt($1)  }
  | LITSTR                 { LitStr($1)  }
  | LBRACK args_opt RBRACK { LitList($2) }
  | LCURLY args     RCURLY { LitTup($2)  }

args_opt:
         { [] }
  | args { $1 }

args:
    expr              { [$1] }
  | expr COMMA args   { $1 :: $3 }

params_opt:
           { [] }
  | params { $1 }

params:
    typ ID               { [($1, $2)] }
  | typ ID COMMA params  { ($1, $2) :: $4 }
