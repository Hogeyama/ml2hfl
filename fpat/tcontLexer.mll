{
open Lexing
open TcontParser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| '\n'
    { let ln = lexbuf.lex_curr_p.pos_lnum
      and off = lexbuf.lex_curr_p.pos_cnum in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                             pos_lnum = ln + 1; pos_bol = off };
      token lexbuf }
| "/*"      { comment lexbuf; token lexbuf}
| "where"   { WHERE }
| "not"     { NOT }
| '!'       { NEG }
| "int"     { TINT }
| "bool"    { TBOOL }
| "any"     { TANY }
| "call"    { CALL }
| "ret"     { RET }
| "->"      { ARROW }
| "..."     { WILDS }
| (lower|'_'(digit|lower|upper|'_')) (digit|lower|upper|'_')* (''')*
    { LIDENT(Lexing.lexeme lexbuf) }
| upper (digit|lower|upper|'_')* (''')*
    { UIDENT(Lexing.lexeme lexbuf) }
| digit+
  { let str = Lexing.lexeme lexbuf in
    INT (int_of_string str) }
| '=' { EQUAL }
| '*' { ASTERISK }
| ',' { COMMA }
| '.' { PERIOD }
| ':' { COLON }
| ';' { SEMICOLON }
| '_' { WILD }
| '(' { LPAREN }
| ')' { RPAREN }
| '|' { OR }
| eof { EOF }
| _
    { raise (Failure ("unknown token: " ^ Lexing.lexeme lexbuf)) }
and comment = parse
| '\n'
    { let ln = lexbuf.lex_curr_p.pos_lnum
      and off = lexbuf.lex_curr_p.pos_cnum in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                               pos_lnum = ln + 1; pos_bol = off };
      comment lexbuf }
| "*/"  { () }
| "/*"  { comment lexbuf; comment lexbuf }
| eof
    { raise (Failure "unterminated comment") }
| _
    { comment lexbuf }
