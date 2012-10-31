%{
open Syntax

let print_error_information () =
  let st = Parsing.symbol_start_pos () in
  let en = Parsing.symbol_end_pos () in
  print_string ("File \"" ^ st.Lexing.pos_fname);
  Format.printf "\", line %d" st.Lexing.pos_lnum;
  Format.printf ", characters %d-%d:\n"
    (st.Lexing.pos_cnum - st.Lexing.pos_bol)
    (en.Lexing.pos_cnum - en.Lexing.pos_bol)
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

%type <(Syntax.ident * Syntax.typ) list * Syntax.t> file
%start file

%%

file:
  typedefs exp EOF
  { $1, $2 }
| error
    { print_error_information ();
      raise (Failure "Syntax error") }

simple_exp:
| LPAREN exp RPAREN
  { $2 }
| TRUE
  { True }
| FALSE
  { False }
| UNKNOWN
  { Unknown }
| INT
  { Int $1 }
| id
  { Var $1 }
| LPAREN RPAREN
  { Unit }
| FAIL
  { Fail }
/*
| ASSERT
  {
    let f = new_var "assert" in
    let x = new_var "b" in
      Let(f, [x], If(Var x, Unit, App(Fail, [Unit]), Unit), Var f)
  }
| QUESTION
  {
    let x = new_var "x" in
      App(Fun(x, Var x), [NInt (new_var "n")])
  }
*/
| LCURLY IDENT RCURLY
  { Event $2 }



exp:
| simple_exp
  { $1 }
| BEGIN exp END
  { $2 }
| FUN id ARROW exp %prec prec_fun
  { Fun($2, $4) }
/*
  { let f = new_var "f" in Let(Nonrecursive, f, $2, $4, Var f) }
*/
| MINUS exp
  { BinOp(Sub, Int 0, $2) }
| exp exp_list %prec prec_app
  { App($1,$2) }
| IF exp THEN exp ELSE exp %prec prec_if
  { If($2, $4, $6, Unit) }
| LET id_list EQUAL exp IN exp %prec prec_let
  {
    let f = List.hd $2 in
    let xs = List.tl $2 in
      Let(f, xs, $4, $6)
  }
| LET REC id_list EQUAL exp IN exp %prec prec_let
  {
    let f = List.hd $3 in
    let xs = List.tl $3 in
      Letrec(f, xs, $5, $7)
  }
| exp EQUAL exp
  { BinOp(Eq, $1, $3) }
| exp LTHAN exp
  { BinOp(Lt, $1, $3) }
| exp GTHAN exp
  { BinOp(Gt, $1, $3) }
| exp LEQ exp
  { BinOp(Leq, $1, $3) }
| exp GEQ exp
  { BinOp(Geq, $1, $3) }
| exp AND exp
  { BinOp(And, $1, $3) }
| exp OR exp
  { BinOp(Or, $1, $3) }
| exp PLUS exp %prec prec_add
  { BinOp(Add, $1, $3) }
| exp MINUS exp
  { BinOp(Sub, $1, $3) }
| exp TIMES exp
  { BinOp(Mult, $1, $3) }
| exp SEMI exp
  { Let(new_var "u", [], $1, $3) }
| NOT exp
  { Not $2 }

exp_list:
  simple_exp %prec prec_app
  { [$1] }
| simple_exp exp_list %prec prec_app
  { $1::$2 }

id:
| IDENT { new_var $1 }

id_list:
| id { [$1] }
| id id_list { $1::$2 }

typedefs:
  { [] }
| TYPE id COLON typ SEMI typedefs
  { ($2, $4)::$6 }
| TYPE LPAREN id COMMA INT RPAREN COLON typ SEMI typedefs
  { ({$3 with id = $5}, $8)::$10 }

typ:
  LPAREN typ RPAREN
  { $2 }
| TUNIT
  { TUnit }
| TBOOL
  { TBool }
| TINT
  { TInt([]) }
| LCURLY id CHOICE exp RCURLY
  { TRInt(subst_orig $2 (Var abst_var) $4) }
| TINT LSQUAR pred_list RSQUAR
  { TInt($3) }
| id COLON typ ARROW typ
  { TFun(($1, $3), $5) }
| typ ARROW typ
  { TFun((new_var "v", $1), $3) }

pred_list:
  { [] }
| id PERIOD exp
  { [subst_orig $1 (Var abst_var) $3] }
| id PERIOD exp COMMA pred_list_aux
  { (subst_orig $1 (Var abst_var) $3)::$5 }

pred_list_aux:
  id PERIOD exp
  { [subst_orig $1 (Var abst_var) $3] }
| id PERIOD exp COMMA pred_list_aux
  { (subst_orig $1 (Var abst_var) $3)::$5 }

