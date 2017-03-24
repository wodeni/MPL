type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | TRUE
  | FALSE
  | AND
  | OR
  | APPLY
  | MATAPP
  | TRANS
  | EMULT
  | EDIV
  | RETURN
  | VOID
  | IF
  | ELSE
  | ELSEIF
  | WHILE
  | INT
  | BOOL
  | FLOAT
  | NULL
  | NEW
  | FUNC
  | CENTER
  | NORTH
  | SOUTH
  | WEST
  | EAST
  | NWEST
  | NEAST
  | SWEST
  | SEAST
  | IMG
  | MAT
  | FMAT
  | NUMLIT of (Ast.num)
  | ID of (string)
  | STRLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
