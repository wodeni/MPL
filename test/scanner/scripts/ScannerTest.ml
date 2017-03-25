open Parser

type num =
  | Int_lit of int
  | Float_lit of float

let stringify = function
  (* Punctuation *)
  | LPAREN -> "LPAREN"      | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"      | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"        | SEMI -> "SEMI"
  | LBRACKET -> "LBRACKET"  | RBRACKET -> "RBRACKET"


  (* Arithmetic Operators *)
  | PLUS -> "PLUS"     | MINUS -> "MINUS"
  | TIMES -> "TIMES"   | DIVIDE -> "DIVIDE"
  | EQ -> "EQ"    | NEQ -> "NEQ"
  | LEQ -> "LEQ"  | GEQ -> "GEQ"
  | LT -> "LT" | GT -> "GT"

  (* Matrix Operators *)

  | APPLY -> "APPLY" | MATAPP -> "MATAPP"
  | TRANS -> "TRANS" | EMULT -> "EMULT"
  | EDIV -> "EDIV"

  (* Logical Operators & Keywords *)
  | AND -> "AND"   | OR -> "OR"
  | NOT -> "NOT"

  (* Assignment Operator *)
  | ASSIGN -> "ASSIGN"

  (* Conditional Operators *)
  | IF -> "IF"
  | ELSE -> "ELSE"
  | ELSEIF -> "ELSEIF"

  (* Loop ID *)
  | WHILE -> "WHILE"

  (* End-of-File *)
  | EOF -> "EOF"

  (* Identifiers *)
  | ID(string) -> "ID"
  (* | ROWS -> "ROWS" | COLS -> "COLS" | LEN -> "LEN" | TRANSPOSE -> "TRANSPOSE"
  | BAR -> "BAR"
  *)
  (* Literals *)
  | INT -> "INT" | FLOAT -> "FLOAT"
  | BOOL -> "BOOL"
  | TRUE -> "TRUE" | FALSE -> "FALSE"
  | NUMLIT(num) -> "NUMLIT"
  | FUNC -> "FUNC"
  | NULL -> "NULL"
  | NEW -> "NEW"
  | CENTER -> "CENTER"
  | NORTH -> "NORTH"
  | SOUTH -> "SOUTH"
  | WEST -> "WEST"
  | EAST -> "EAST"
  | NWEST -> "NWEST"
  | NEAST -> "NEAST"
  | SWEST -> "SWEST"
  | SEAST -> "SEAST"
  | IMG -> "IMG"
  | MAT -> "MAT"
  | FMAT -> "FMAT"
  | STRLIT(string) -> "STRLIT"
  (* | INC -> "INC" | DEC -> "DEC"
  | COLON -> "COLON"
  | FOR -> "FOR" *)
  | RETURN -> "RETURN"

  | VOID -> "VOID"

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let rec print_tokens = function
    | EOF -> " "
    | token ->
      print_endline (stringify token);
      print_tokens (Scanner.token lexbuf) in
  print_tokens (Scanner.token lexbuf)
