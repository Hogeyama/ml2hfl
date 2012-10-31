{
open Syntax
open Spec_parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let symbol = ['(' ')' '*' '?' '|' '+' ',' '!' ';' '.' ':' '#']

rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '{' { LCURLY }
| '}' { RCURLY }
| '[' { LSQUAR }
| ']' { RSQUAR }
| '=' { EQUAL }
| '<' { LTHAN }
| '>' { GTHAN }
| "<=" { LEQ }
| ">=" { GEQ }
| "||" { OR }
| "&&" { AND }
| "not" { NOT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '|' { BAR }
| "inline" { INLINE }
| "inlinef" { INLINEF }
| "type" { TYPE }
| "unit" { TUNIT }
| "bool" { TBOOL }
| "int" { TINT }
| "list" { LIST }
| "true" { TRUE }
| "false" { FALSE }
| "fail" { FAIL }
| "***" { UNKNOWN }
| "begin" { BEGIN }
| "end" { END }
| "fun" { FUN }
| "->" { ARROW }
| "let" { LET }
| "rec" { REC }
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "in" { IN }
| ';' { SEMI }
| ':' { COLON }
| ',' { COMMA }
| '.' { PERIOD }
| '\"' { STRING (string lexbuf) }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
(*| lower (digit|lower|upper|'_')* *)
(*    { IDENT(Lexing.lexeme lexbuf) } *)
| lower (digit|lower|upper)*
    { IDENT(Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { (*Format.eprintf "unknown token %s near characters %d-%d@."
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf);*)
      failwith "lex error" }

and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { failwith "unterminated comment" }
| _
    { comment lexbuf }

and string = parse
| '\"' { "" }
| (digit|lower|upper|space|symbol)+
  { let s = Lexing.lexeme lexbuf in
      s ^ (string lexbuf) }
| "\\n" { "\n" ^ (string lexbuf) }
| _
    { Format.eprintf "unknown token %s near characters %d-%d@."
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf);
      failwith "lex error" }
