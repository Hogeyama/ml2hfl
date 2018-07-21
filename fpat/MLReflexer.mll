{
  open MLRefparser
}

let space = [' ' '\t' '\r']
let digit = ['0'-'9']
let lalpha = ['A'-'Z']
let salpha = ['a'-'z']
let lsnum = digit | lalpha | salpha | '\'' | '_'


rule token = parse
  | "let" { LET }
  | "in" { IN }
  | '|' { MID }
  | ";;" { SEQ }
  | "forall" { FORALL }
  | "exists" { EXISTS }
  | "mu" { MU }
  | "nu" { NU }
  | '.' { DOT }
  | "&&" { AND }
  | "||" { OR }
  | "=>" { IMP }
  | "not" { NOT }
  | '<' { LT }
  | '>' { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }

  | "^" { SCOMP }
  | "(=" { SUBSET }
  | "++" { SUNION }
  | "**" { SINTERSECT }
  | "--" { SDIFF } 
  | "{}" { SEMPTY }

  | '+' { ADD }
  | '-' { SUB }
  | '*' { AST }
  | '/' { DIV }
  | "+." { FADD }
  | "-." { FSUB }
  | "*." { FMUL }
  | "/." { FDIV }
  | "==" { EQEQ }

  | '=' { EQUAL }
  | "<>" { NOTEQUAL }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '|' { MID }
  | '{' { LBRA }
  | '}' { RBRA }

  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '?' { QUESTION }

  | "<-" { LARROW }
  | "->" { ARROW }
  | ':' { COLON }
  | "::" { COLCOL }
  | "//" { SLASLA }
  | ":-" { COLMIN }
  | "?-" { QMIN }
  | ';' { SEMICOLON }
  | ";;" { SEQ }
  | ";;;" { TSEQ }

  | "[|" { LABRA }
  | "|]" { RABRA }
  | "_set" { ASET }
 

  | "_is_" lalpha lsnum* { 
    let str = Lexing.lexeme lexbuf in
    RECOGNIZER (String.sub str 4 (String.length str - 4))
  }

  | "_get_" lalpha lsnum*  "_" digit+ {
    let str = Lexing.lexeme lexbuf in
    let sn = String.length str in
    let mn = String.rindex str '_' in
    let id = String.sub str 5 (mn - 5) in
    let n = String.sub str (mn + 1) (sn - mn -1) in
    ACCESSOR (id,int_of_string n)
  }

  | "_size_" lsnum+ "_" lsnum* {
    let str = Lexing.lexeme lexbuf in
    let sn = String.length str in
    let mn = String.rindex str '_' in
    let size_id = String.sub str 6 (mn - 6) in
        let adt_id = String.sub str (mn + 1) (sn - mn -1) in
    SIZE (size_id, adt_id)
  }

  | "size_" lsnum* {
    let str = Lexing.lexeme lexbuf in
    let adt_id = String.sub str 5 (String.length str - 5) in
    SIZE ("size", adt_id)
  }

  | "proj"   { PROJ }

  | "forall" { FORALL }
  | "exists" { EXISTS }

  | "type" { DATA }
  | "of" {OF}

  | "true" { BOOL (true) }
  | "false" { BOOL (false) }
    

  | "assert" { ASSERT }

  | "maximize" { MAXIMIZE }
  | "minimize" { MINIMIZE }
  | "prioritize" { PRIORITIZE }
  | "template" { TEMPLATE }

  | "Ip" { IPTAG }
  | "Im" { IMTAG }
  | "O"  { OTAG }

  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }

  | digit+ '.' digit* (['e''E'] ['+''-']? ['0'-'9']+)?
    { let str = Lexing.lexeme lexbuf in
      FLOAT (float_of_string str) }

  | "'c" lsnum*   { VAR (Lexing.lexeme lexbuf) }
  | salpha lsnum* { VAR (Lexing.lexeme lexbuf) }
  | salpha lsnum* ('[' digit+ ':' digit+ ']')* { VAR_T (Lexing.lexeme lexbuf) }
  | lalpha lsnum* { CONST (Lexing.lexeme lexbuf) }

  | '#' salpha lsnum*
    { let str = Lexing.lexeme lexbuf in
      LIDENT (Idnt.make str) }
  | '#' lalpha lsnum*
    { let str = Lexing.lexeme lexbuf in
      UIDENT (Idnt.make str) }

  | eof { EOF }

  | space+ { token lexbuf }
  | '\n' {
        Lexing.new_line lexbuf;
        token lexbuf
      }

  | "(*"
      { comment lexbuf; token lexbuf}

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
