%{
(*
 * File: parser.mly
 * Date: 2017-03-11
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 *)
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA
%token PLUS MINUS TIMES DIVIDE ASSIGN NOT
%token EQ NEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token APPLY MATAPP TRANS EMULT EDIV
%token RETURN VOID IF ELSE ELSEIF WHILE INT BOOL FLOAT
%token NULL NEW FUNC
%token CENTER NORTH SOUTH WEST EAST NWEST NEAST SWEST SEAST
%token IMG MAT FMAT
%token <Ast.num> NUMLIT
%token <string> ID
%token <string> STRLIT
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left APPLY MATAPP EMULT EDIV
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%
program:
    decls EOF { $1 }

decls:
    /* nothing */      { []}
  /*| decls gdecl        { ($2 :: fst $1), snd $1 }*/
  | decls fdecl        { $2 :: $1 }

fdecl:
  typ ID LBRACE vdecl_list stmt_list RBRACE
    { { typ = $1; fname = $2;
      locals = List.rev $4; body = List.rev $5 } }

typ:
  primitives { Typ($1) }

primitives:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | VOID { Void }
  | MAT LT primitives GT LBRACKET NUMLIT RBRACKET LBRACKET NUMLIT RBRACKET { Mat($3, $6, $9) }
  | FMAT LT primitives GT LBRACKET NUMLIT RBRACKET LBRACKET NUMLIT RBRACKET { FMat($3, $6, $9) }
  | IMG { Img }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  typ ID SEMI { Local($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr:
    NUMLIT           { NumLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | NULL             { Null }
  | STRLIT           { StrLit($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | expr APPLY  expr { Binop($1, Apply, $3) }
  | expr MATAPP expr { Binop($1, Matapp, $3) }
  | expr EMULT  expr { Binop($1, Emult,  $3) }
  | expr EDIV   expr { Binop($1, Ediv,   $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }
  | LBRACKET mat_lit RBRACKET                 { MatrixLit($2) }
  | ID LBRACKET expr COMMA expr RBRACKET      { MatrixAccess($1, $3, $5) }

/*expr_opt:
    nothing                   { Noexpr }
  | expr                            { $1 } */

actuals_opt:
    /* nothing */                   { [] }
  | actuals_list                    { List.rev $1 }
  
actuals_list:
    expr                            { [$1] }
  | actuals_list COMMA expr         { $3 :: $1 }

lit:
    NUMLIT                          { $1 }

mat_lit:
    lit_list                        { [$1] }
    | mat_lit SEMI lit_list         { $3 :: $1 }

lit_list:
    lit                             { [$1] }
    | lit_list COMMA lit            { $3 :: $1 }