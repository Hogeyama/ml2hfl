%{
open Type
open Syntax

let print_error_information () =
  let st = Parsing.symbol_start_pos () in
  let en = Parsing.symbol_end_pos () in
  print_string ("File \"" ^ st.Lexing.pos_fname);
  Format.printf "\", line %d" st.Lexing.pos_lnum;
  Format.printf ", characters %d-%d:\n"
    (st.Lexing.pos_cnum - st.Lexing.pos_bol)
    (en.Lexing.pos_cnum - en.Lexing.pos_bol)

let parse_error _ = print_error_information ()

let new_var s = Id.make 0 s (TInt[])
%}

%token EOF
%token <string> IDENT
%token <int> INT
%token <string> STRING
%token LPAREN
%token RPAREN
%token LCURLY
%token RCURLY
%token LSQUAR
%token RSQUAR
%token DOT
%token BEGIN
%token END
%token FUN
%token REC
%token ARROW
%token LET
%token FIX
%token IN
%token LBRACKET
%token RBRACKET
%token SEMI
%token COLON
%token COMMA
%token PERIOD
%token IF
%token THEN
%token ELSE
%token TYPE
%token TUNIT
%token TBOOL
%token TINT
%token LIST
%token TRUE
%token FALSE
%token EQUAL
%token LTHAN
%token GTHAN
%token LEQ
%token GEQ
%token OR
%token AND
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token CHOICE
%token FAIL
%token ASSERT
%token QUESTION
%token UNKNOWN

/* priority : low -> high */
%left prec_if
%left prec_app
%nonassoc FUN
%right ARROW
%left prec_fun
%nonassoc prec_let
%left SEMI
%left OR
%left AND
%nonassoc EQUAL LTHAN GTHAN LEQ GEQ
%left PLUS MINUS
%left prec_add
%nonassoc IDENT INT TRUE FALSE UNKNOWN QUESTION LPAREN RPAREN BEGIN END LET IN FAIL ASSERT

%start typedefs
%type <(Syntax.id * Syntax.typ) list> typedefs

%%

exp:
  id
  { make_var $1 }
| LPAREN exp RPAREN
  { $2 }
| INT
  { make_int $1 }
| MINUS exp
  { make_sub (make_int 0) $2 }
| exp EQUAL exp
  { make_eq $1 $3 }
| exp LTHAN exp
  { make_lt $1 $3 }
| exp GTHAN exp
  { make_gt $1 $3 }
| exp LEQ exp
  { make_leq $1 $3 }
| exp GEQ exp
  { make_geq $1 $3 }
| exp AND exp
  { make_and $1 $3 }
| exp OR exp
  { make_or $1 $3 }
| exp PLUS exp
  { make_add $1 $3 }
| exp MINUS exp
  { make_sub $1 $3 }
| NOT exp
  { make_not $2 }

id:
| IDENT { new_var $1 }

typedefs:
  { [] }
| id COLON typ typedefs
  { ($1, snd $3)::$4 }

typ_base:
| TUNIT
  { TUnit }
| TBOOL
  { TBool }
| TINT
  { TInt([]) }
| TINT LSQUAR pred_list RSQUAR
  { TInt($3) }

typ_base_id:
| id COLON typ_base
  { $1, $3 }
| typ_base
  { new_var "", $1 }

typ:
| LPAREN typ RPAREN
  { $2 }
| typ_base_id
  { $1 }
| typ TIMES typ
  { new_var "", TPair(snd $1, snd $3) }
| typ LIST
  { new_var "", TList(snd $1, []) }
| typ LIST LSQUAR pred_list RSQUAR
  { new_var "", TList(snd $1, $4) }
| typ ARROW typ
  {
    let x1,typ1 = $1 in
    let x2,typ2 = $3 in
    let x = {x1 with Id.typ=subst_type x1 (make_var abst_var) typ1} in
    let typ = subst_type x2 (make_var abst_var) typ2 in
      new_var "", TFun(x, typ)
  }

pred_list:
  { [] }
| exp
  { [$1] }
| exp SEMI pred_list
  { $1::$3 }
