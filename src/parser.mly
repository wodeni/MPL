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
%token RETURN VOID IF ELSE ELSEIF WHILE INT BOOL FLOAT STRING
%token NULL NEW FUNC
%token CENTER NORTH SOUTH WEST EAST NWEST NEAST SWEST SEAST
%token IMG MAT FMAT
%token <int> INTLIT
%token <float> FLOATLIT
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
  | decls fdecl        { $2 :: $1 }

fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
    { { typ = $1; fname = $2; formals = $4;
      locals = List.rev $7; body = List.rev $8 } }
      
formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | FLOAT { Float }
  | STRING { String }
  | VOID { Void }
  | MAT LT typ GT LBRACKET INTLIT RBRACKET LBRACKET INTLIT RBRACKET { Mat($3, $6, $9) }
  | FMAT LT typ GT LBRACKET INTLIT RBRACKET LBRACKET INTLIT RBRACKET { FMat($3, $6, $9) }
  | IMG { Img }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  typ ID SEMI { ($1, $2) }

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
    INTLIT           { IntLit($1) }
  | FLOATLIT         { FloatLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | NULL             { Null }
  | STRLIT           { StrLit($1) }
  | CENTER           { Id("#C") }
  | EAST             { Id("#E") }
  | WEST             { Id("#W") }
  | NORTH            { Id("#N") }
  | SOUTH            { Id("#S") }
  | NWEST            { Id("#NW") }
  | NEAST            { Id("#NE") }
  | SWEST            { Id("#SW") }
  | SEAST            { Id("#SE") }
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
  | LBRACKET mat_lit RBRACKET                 { MatrixLit(List.rev $2) }
  | ID LBRACKET expr RBRACKET LBRACKET expr RBRACKET      { MatrixAccess($1, $3, $6) }

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
    INTLIT                          { IntLit($1) }
    | FLOATLIT                      { FloatLit($1) }
    | ID			    { Id($1) }

mat_lit:
    lit_list                        { [$1] }
    | mat_lit SEMI lit_list         { $3 :: $1 }

lit_list:
    lit                             { [$1] }
    | lit_list COMMA lit            { $1 @ [$3] }
