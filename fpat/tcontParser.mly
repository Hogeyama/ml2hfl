%{
  open Util
  open Combinator

  let print_error_information () =
    let st = Parsing.symbol_start_pos () in
    let en = Parsing.symbol_end_pos () in
    print_string ("File \"" ^ st.Lexing.pos_fname);
    Format.printf "\", line %d" st.Lexing.pos_lnum;
    Format.printf ", characters %d-%d:\n"
                  (st.Lexing.pos_cnum - st.Lexing.pos_bol)
                  (en.Lexing.pos_cnum - en.Lexing.pos_bol)

%}

%token WHERE
%token NOT
%token NEG
%token TINT
%token TBOOL
%token TANY
%token CALL
%token RET
%token ARROW
%token WILDS

%token <string> LIDENT
%token <string> UIDENT
%token <int> INT
%token EQUAL
%token ASTERISK
%token COMMA
%token PERIOD
%token COLON
%token SEMICOLON
%token WILD
%token LPAREN
%token RPAREN
%token OR
%token EOF

%type <Ttype.hotcontract> hotcontract
%start hotcontract

%%

hotcontract:
| UIDENT EQUAL scontracts WHERE tcontract
    { ($3, $5) }
| error
    { print_error_information ();
      raise (Failure "Syntax error") }

scontracts:
| scontract
    { [$1] }
| scontract SEMICOLON scontracts
    { $1 :: $3 }

scontract:
| LPAREN scontract RPAREN
    { $2 }
| TINT
    { SFlat FInt }
| TBOOL
    { SFlat FBool }
| TANY
    { SFlat FAny }
| LIDENT COLON scontract ARROW scontract
    { SFun1 ($1, $3, $5) }
| scontract ARROW scontract
    { SFun2 ($1, $3) }

tcontract:
| atom
    { Atom $1 }
| NEG atom
    { Neg $2 }
| tcontract ASTERISK
    { Kleene $1 }
| tcontract tcontract
    { Concat ($1, $2) }
| NOT tcontract
    { Not $2 }
| tcontract OR tcontract
    { Or ($1, $3) }
| WILDS
    { Wilds }

atom:
| CALL LPAREN LIDENT COMMA vpattern RPAREN
    { Call ($3, $5) }
| RET LPAREN LIDENT COMMA vpattern RPAREN
    { Ret ($3, $5) }

vpattern:
| WILD
    { Any }
| LIDENT
    { Var $1 }
