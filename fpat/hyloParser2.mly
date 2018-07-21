// 仕様用 Parser

%{
open Util
open Combinator
open Syntax
%}

// リテラル
%token <string> VAR  // x, y, abc, ...
%token <int> INT     // 0, 1, 2, ...

// 括弧類
%token LPAREN   // '('
%token RPAREN   // ')'

// 区切り記号
%token EQUAL 
%token GREATER
%token LESS
%token GEQUAL
%token LEQUAL
%token NOTEQUAL
%token DOT

// キーワード
%token TRUE     // "true"
%token FALSE    // "false"

// 制御記号
%token EOF 

%start main
%type <Syntax.spec> main

%%

// 開始記号
main:
  | spec EOF
      { $1 }
;

// 等号・不等号
eq:
  | GREATER
      { Gt }
  | LESS
      { Lt }
  | EQUAL
      { Eq }
  | GEQUAL
      { Geq }
  | LEQUAL
      { Leq }
  | NOTEQUAL
      { Neq }
;

// 関数
fs:
  | VAR
      { Fun (Var $1) }
  | VAR VAR
      { Fun (Var ($1 ^ "_" ^ $2)) }
  | VAR DOT fs
      { Circ (Var $1, $3) }
  | VAR VAR DOT fs
      { Circ (Var ($1 ^ "_" ^ $2), $4) }
;

// 仕様
spec:
  | LPAREN fs RPAREN eq VAR
      { ($2, $4, Var $5) }
  | LPAREN fs RPAREN eq INT
      { ($2, $4, Intlit $5) }
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
