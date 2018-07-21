{
  open SexpParser

  (** S-expression lexer *)
}
rule token = parse
| [' ' '\t' '\r'] { token lexbuf }
| '\n' {
      Lexing.new_line lexbuf;
      token lexbuf
    }
| '(' { LPAREN }
| ')' { RPAREN }
(*| ['A'-'Z' 'a'-'z' '0'-'9']+ as str { SYMBOL(str) }*)
| [^' ' '\t' '\n' '\r' '(' ')']+ as str { SYMBOL(str) }
| eof { EOF }

| _ {
      Printer.print_lexer_error_information lexbuf;
      raise (Global.Syntax_error "Lexical error")
    }
