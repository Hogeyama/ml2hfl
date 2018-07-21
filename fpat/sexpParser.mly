%{
  open Util
  open Combinator

  (** S-expression parser *)
%}

%token <string> SYMBOL
%token LPAREN RPAREN
%token EOF

%start parser_main sexps
%type <Sexp.t> parser_main
%type <Sexp.t list> sexps
%%

parser_main:
 | sexp EOF { $1 }
 | error {
       Printer.print_parser_error_information ();
       raise (Global.Syntax_error "Parse error")
     }
;

sexp:
 | SYMBOL { Sexp.S($1) }
 | LPAREN sexps RPAREN { Sexp.L($2) }

sexps:
 | sexp sexps { $1 :: $2 }
 |  { [] }
;
