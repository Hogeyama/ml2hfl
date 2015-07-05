{
open Syntax
open Spec_parser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_' '\'']
let upper = ['A'-'Z']
let symbol = ['(' ')' '*' '?' '|' '+' ',' '!' ';' '.' ':' '#']

rule token = parse
| space+
    { token lexbuf }
| '\n'
    { Lexing.new_line lexbuf;
      token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LSQUAR }
| ']' { RSQUAR }
| '{' { LBRACE }
| '}' { RBRACE }
| '=' { EQUAL }
| '<' { LTHAN }
| '>' { GTHAN }
| "<=" { LEQ }
| ">=" { GEQ }
| "<>" { NEQ }
| "||" { OR }
| "&&" { AND }
| "not" { NOT }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| "inline" { INLINE }
| "inlinef" { INLINEF }
| "unit" { TUNIT }
| "X" { TRESULT }
| "bool" { TBOOL }
| "int" { TINT }
| "list" { LIST }
| "->" { ARROW }
| ';' { SEMI }
| ':' { COLON }
| '|' { BAR }
| ',' { COMMA }
| "type" { TYPE }
| "val" { VAL }
| "valcps" { VALCPS }
| "valcegar" { VALCEGAR }
| "external" { EXTERNAL }
| "fairness" { FAIRNESS }
| "true" { TRUE }
| "false" { FALSE }
| digit+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
| lower(digit|lower|upper)* { IDENT(Lexing.lexeme lexbuf) }
| (lower|upper)(digit|lower|upper)* { EVENT(Lexing.lexeme lexbuf) }
| eof { EOF }
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
| '\n'
    { Lexing.new_line lexbuf;
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
