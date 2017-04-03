open Parser

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"

  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | APPLY -> "APPLY"   | MATAPP -> "MATAPP"

  (* Relational Operators *)
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LEQ -> "LEQ"  | GEQ -> "GEQ"

  (* Logical Operators & Keywords *)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"

  (* Assignment Operator *)
  | ASSIGN -> "ASSIGN"

  (* Conditional Operators *)
  | IF -> "IF"
  | ELSE -> "ELSE"
  (*| ELSEIF -> "ELSEIF"*)

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"

  (* Literals *)
  | INTLIT(int) -> "INTLIT"
  | FLOATLIT(float) -> "FLOATLIT"
  | STRLIT(string) -> "STRLIT"
  | SEMI -> "SEMI" | LBRACKET -> "LBRACKET" | RBRACKET -> "RBRACKET"
  | LT -> "LT" | GT -> "GT" | WHILE -> "WHILE"
  | RETURN -> "RETURN"
  | TRUE -> "TRUE" | FALSE -> "FALSE"
  | INT -> "INT" | BOOL -> "BOOL" | VOID -> "VOID" | FLOAT -> "FLOAT"
  | NULL -> "NULL" | MAT -> "MAT" | FMAT -> "FMAT"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
