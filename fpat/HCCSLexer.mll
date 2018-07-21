{
  open HCCSParser

  (** HCCS lexer *)
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lalpha = ['A'-'Z']
let salpha = ['a'-'z']
let lsnum = digit | lalpha | salpha | '\'' | '_'

rule token = parse
| [' ' '\t' '\r'] { token lexbuf }
| '\n' {
      Lexing.new_line lexbuf;
      token lexbuf
    }

| digit+
  { let str = Lexing.lexeme lexbuf in
    INT (int_of_string str) }
| digit+ '.' digit+ (['e''E'] ['+''-']? ['0'-'9']+)?
  { let str = Lexing.lexeme lexbuf in
    FLOAT (float_of_string str) }

| ":-" { DECLARE }
| "?-" { QUERY }
| '=' { EQ }
| "\\=" { NOTEQ }
| '!' { NOT }
| "not" { NOT }
| '>' { GT }
| '<' { LT }
| ">=" { GEQ }
| "<=" { LEQ }
| '.' { DOT }
| ',' { COMMA }
| "bot" { BOT }
| "top" { TOP }
| "false" { BOT }
| "true" { TOP }

| '+' { PLUS }
| "+." { FPLUS }
| '-' { MINUS }
| "-." { FMINUS }
| '*' { TIMES }
| "*." { FTIMES }
| '/' { DIV }
| "/." { FDIV }

| "/\\" { AND }
| "&&" { AND }
| "\\/" { OR }
| "||" { OR }

| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRA }
| ']' { RBRA }

| 'c' ['0'-'9']+ as str { COEFF(str) }
| ['_']* salpha lsnum* as str { VAR(str) }
| salpha lsnum* ('[' digit+ ':' digit+ ']')* as str { VAR_T (str) }
| lalpha lsnum* as str { CON(str) }

| eof { EOF }

| "(*" { comment lexbuf; token lexbuf }

| _ {
      Printer.print_lexer_error_information lexbuf;
      raise (Global.Syntax_error "Lexical error")
    }
and comment = parse
  | '\n'
      { Lexing.new_line lexbuf;
        comment lexbuf }
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | eof
      { raise (Global.Syntax_error "unterminated comment") }
  | _
      { comment lexbuf }
