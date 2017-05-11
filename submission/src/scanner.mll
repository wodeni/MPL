(*
 * File: scanner.mll
 * Date: 2017-03-11
 *
 * PLT Spring 2017
 * MPL Project
 * Wode "Nimo" Ni    <wn2155@columbia.edu>
 * David Rincon-Cruz <dr2884@columbia.edu>
 * Chi Zhang         <cz2440@columbia.edu>
 * Jiangfeng Wang    <jw3107@columbia.edu>
 *)

{ open Parser

exception LexError of string

(* string parsing from OCaml compiler code :-) *)
let string_buff = Buffer.create 256
let reset_string_buffer () = Buffer.clear string_buff
let store_string_char c = Buffer.add_char string_buff c
let store_string_snip str = Buffer.add_string string_buff str
let get_stored_string () = Buffer.contents string_buff

let char_for_backslash = function
    'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c   -> c

let decimal_code c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let char_for_hexadecimal_code d u =
  let d1 = Char.code d in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code u in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let lex_warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "MPL warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }
;;
}

let newline = '\n' | "\r\n"
let whitespace = [' ' '\t']
let consecutive_strings = ['"'] whitespace* ['"']
let backslash_escapes = ['\\' '"' '\'' 'n' 't' 'b' 'r']
let digit      = ['0'-'9']
let integer    = digit+
let exp = ('e' | 'E') ('+' | '-')? digit+
let float_re = '.' digit+ exp? | digit+ ('.' digit* exp? | exp)

rule token = parse
  newline            { Lexing.new_line lexbuf; token lexbuf }
| whitespace         { token lexbuf }
| "/*"               { comment 0 lexbuf }           (* Comments *)
| '"'              { reset_string_buffer ();    (* String literals *)
      parse_string lexbuf;
      (*handle_lexical_error string lexbuf;*)
      STRLIT(get_stored_string ()) }
(* Punctuation *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LBRACKET }
| ']'      { RBRACKET }
| ';'      { SEMI }
| ','      { COMMA }

(* Operators *)
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"    { AND }
| "||"     { OR }
| "!"    { NOT }
(* | "neg"     { NEG } *)
| "@"      { APPLY }
| ".@"     { MATAPP }
| "^"      { TRANS }
| ".*"     { EMULT }
| "./"     { EDIV }

(* Keywords *)
| "if"     { IF }
| "else"   { ELSE }
| "elseif" { ELSEIF }
(* | "for"    { FOR } *)
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "boolean"{ BOOL }
| "string" { STRING }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "func"   { FUNC }
| "null"   { NULL }
| "new"    { NEW }
| "#C"     { CENTER }
| "#N"     { NORTH }
| "#S"     { SOUTH }
| "#W"     { WEST }
| "#E"     { EAST }
| "#NW"    { NWEST }
| "#NE"    { NEAST }
| "#SW"    { SWEST }
| "#SE"    { SEAST }


(* Built-in Types *)
| "Img"    { IMG }
| "Mat"    { MAT }
| "fMat"   { FMAT }

(* Integer literals, identifiers, and others *)
| integer as lxm { INTLIT(int_of_string lxm) }
| float_re as lxm { FLOATLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

(*
and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
*)

(* Block comment parsing: nested comments are allowed *)
and comment level = parse
    "*/"  { if level = 0 then token lexbuf
            else comment (level-1) lexbuf }
  | newline { Lexing.new_line lexbuf; comment level lexbuf }
  | "/*"    { comment (level+1) lexbuf }
  | eof     { raise (LexError("unterminated comment!")) }
  | _       { comment level lexbuf }

(* fancy string parsing from OCaml compiler code :-) *)
and parse_string = parse
    consecutive_strings  { parse_string lexbuf }
  | '"'     { () }
  | newline { Lexing.new_line lexbuf; parse_string lexbuf }
  | '\\' ("\010" | "\013" | "\013\010") ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      parse_string lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char(char_for_backslash c);
      parse_string lexbuf }
  | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u) ;
      parse_string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if v > 255 then
       lex_warning lexbuf
        (Printf.sprintf
          "illegal backslash escape in string: `\\%c%c%c'" c d u) ;
      store_string_char (Char.chr v);
      parse_string lexbuf }
  | '\\' (_ as c)
    { lex_warning lexbuf
        (Printf.sprintf "illegal backslash escape in string: `\\%c'" c) ;
      store_string_char '\\' ;
      store_string_char c ;
      parse_string lexbuf }
  | '\010'
    { store_string_char '\010';
      incr_loc lexbuf 0;
      parse_string lexbuf }
  | eof { raise(LexError("unterminated string")) }
  | _ as c
    { store_string_char c;
      parse_string lexbuf }
