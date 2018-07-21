// Hylo導出用 Parser

%{
open Util
open Combinator
open Syntax
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// 演算子

//%token PLUS     // '+'
//%token MINUS    // '-'
//%token ASTERISK // '*'
//%token SLASH    // '/'
//%token LESS     // '<'
//%token GREATER  // '>'
//%token COLCOL   // "::"

%token PLUS     // "plus"
%token MINUS
%token MUL
%token DIV
%token MAX
%token MIN
%token LESS
%token GREATER
%token ERROR

// 括弧類
%token LPAREN   // '('
%token RPAREN   // ')'
%token LBRA     // '['
%token RBRA     // ']'

// 区切り記号
%token EQUAL    // '='
%token ARROW    // "->"
%token SEMICOL  // ';'
%token COM      // ','
%token DOT      // '.'
//%token SCSC     // ";;"

// キーワード
%token TRUE     // "true"
%token FALSE    // "false"
%token LAMBDA   // "lambda"
%token NIL      // "Nil"
%token CONS     // "Cons"
%token IF       // "if"
%token THEN     // "then"
%token ELSE     // "else"
%token CASE     // "case"
%token OF       // "of"

// 制御記号
%token EOF 

%start main
%type <Syntax.decl> main

%%

// 開始記号
main:
  | decl EOF
      { $1 }
;

// 変数
variable:
  | VAR
      { Var $1 }

  | INT
      { Intlit $1 }

  | PLUS
      { Plus }
  | MINUS
      { Minus }
  | MUL
      { Mul }
  | DIV
      { Div }
  | MAX
      { Max }
  | MIN
      { Min }
  | LESS
      { Less }
  | GREATER
      { Greater }
  | ERROR
      { Error }
;

// コンストラクタ
c:
  | NIL
      { Nil }
  | CONS
      { Cons }
  | TRUE
      { True }
  | FALSE
      { False }
;

// パターン
p:
  | c
      { Pc ($1, Pt[])}
  | c p
      { Pc ($1, $2) }
  | LPAREN RPAREN
      { Pt [] }
  | LPAREN plist RPAREN
      { Pt (List.rev $2) }
  | variable
      { Pv $1 }
;

plist:
  | p
      { [$1] }
  | plist COM p
      { $3 :: $1 }
;

// 項
term:
  | variable
      {Tv $1}
  | LPAREN RPAREN
      {Tt []}
  | LPAREN tlist RPAREN
      {Tt (List.rev $2)}
  | variable term
      {Ta ($1, $2)}
  | c
      {Tc ($1, Tt[])}
  | c term
      {Tc ($1, $2)}
;

tlist:
  | term
      { [$1] }
  | tlist COM term
      { $3 :: $1}
;

// alternatives
r:
  | cases_rev
      { Al (List.rev $1) }
;

cases_rev:
  | p ARROW term
      { [($1, $3)] }
  | cases_rev SEMICOL p ARROW term
      { ($3, $5) :: $1 }
;

// 引数
vs:
  | variable
      { V $1 }
  | LPAREN RPAREN
      { Vs []}
  | LPAREN vlist RPAREN
      { Vs (List.rev $2)}
;

vlist:
  | variable
      { [$1] }
  | vlist COM variable
      { $3 :: $1}
;

// ボディ
b:
  | LAMBDA vs CASE term OF r
      { B ($2, $4, $6) }
;

// 宣言
decl:
  | VAR EQUAL b
      { Def (Var $1, $3) }
  | error
      { 
	let message =
          Printf.sprintf 
            "parse error near characters %d-%d"
            (Parsing.symbol_start ())
	    (Parsing.symbol_end ())
	in
        failwith message
      }
;
