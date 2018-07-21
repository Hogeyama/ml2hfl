{
open Lexing
open Parser

(** Simple ML expression lexer *)

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
| "(*"
    { comment lexbuf; token lexbuf}
| "fun"
    { FUN }
| "fix"
    { FIX }
| "let"
    { LET }
| "in"
    { IN }
| "match"
    { MATCH }
| "with"
    { WITH }
| "fail"
    { FAIL }
| "as"
    { AS }

| "rec"
    { REC }
| "and"
    { AND }
| "if"
    { IF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "type"
    { TYPE }
| "of"
    { OF }
| "where"
    { WHERE }
| "int"
    { TINT }
| "unit"
    { TUNIT }
| "tt"
    { TT }
| "ff"
    { FF }
| "/\\"
    { SLASH_BSLASH }
| "\\/"
    { BSLASH_SLASH }

| digit+
    { INTEGER(int_of_string(Lexing.lexeme lexbuf)) }
| (lower|'_'(digit|lower|upper|'_')) (digit|lower|upper|'_')* (''')*
    { LIDENT(Lexing.lexeme lexbuf) }
| upper (digit|lower|upper|'_')* (''')*
    { UIDENT(Lexing.lexeme lexbuf) }
| '_'
    { UNDERSCORE }
| '='
    { EQUAL }
| ','
    { COMMA }
| '.'
    { PERIOD }
| ':'
    { COLON }
| ';'
    { SEMICOLON }
| "|"
    { BAR }
| "+"
    { PLUS }
| "-"
    { MINUS }
| "*"
    { AST }
| "/"
    { SLASH }
| "mod"
    { MOD }
| "<"
    { LANGLE }
| ">"
    { RANGLE }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "<="
    { LANGLE_EQUAL }
| ">="
    { RANGLE_EQUAL }
| "<>"
    { LANGLE_RANGLE }
| "->"
    { MINUS_RANGLE }
| "=>"
    { EQUAL_RANGLE }
| "::"
    { COLON_COLON }
| eof
    { EOF }
| _
    { raise (Global.Syntax_error ("unknown token: " ^ Lexing.lexeme lexbuf)) }
and comment = parse
| '\n'
    { let ln = lexbuf.lex_curr_p.pos_lnum
      and off = lexbuf.lex_curr_p.pos_cnum in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                               pos_lnum = ln + 1; pos_bol = off };
      comment lexbuf }
| "*)"
    { () }
| "(*"
    { comment lexbuf; comment lexbuf }
| eof
    { raise (Global.Syntax_error "unterminated comment") }
| _
    { comment lexbuf }
