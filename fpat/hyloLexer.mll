(* Hylo導出用 *)

{
open Util
open Combinator
open HyloParser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let alnum = digit | alpha | '\''

rule token = parse
  (* 整数定数 *)
  | digit+
    { let str = Lexing.lexeme lexbuf in
      INT (int_of_string str) }
  
  (* 演算子 *)
(*
  | '+'       { PLUS }
  | '-'       { MINUS }
  | '*'       { ASTERISK }
  | '/'       { SLASH }
*)
(*
  | '<'       { LESS }
  | '>'       { GREATER }
  | "::"      { COLCOL }
*)

  (* 括弧類 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '['       { LBRA }
  | ']'       { RBRA }
  
  (* 区切り記号 *)
  | '='       { EQUAL }
  | "->"      { ARROW }
  | ';'       { SEMICOL }
  | ','       { COM }
  | '.'       { DOT }
(*  | ";;"      { SCSC } *)

  (* キーワード *)
  | "true"    { TRUE }
  | "false"   { FALSE }
  | "lambda"  { LAMBDA }
  | "Nil"     { NIL }
  | "Cons"    { CONS }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "case"    { CASE }
  | "of"      { OF }

  | "plus"    { PLUS }
  | "minus"   { MINUS }
  | "mul"     { MUL }
  | "div"     { DIV }
  | "max"     { MAX }
  | "min"     { MIN }
  | "<"       { LESS }
  | ">"       { GREATER }
  | "error"   { ERROR }

  (* 変数 *)
  | alpha alnum*
    { VAR (Lexing.lexeme lexbuf) }
  
  (* 制御記号 *)
  | eof       { EOF }

  (* スペースを読み飛ばす *)
  | space+    { token lexbuf }

  | _
    {
      let message = Printf.sprintf
        "unknown token %s near characters %d-%d"
        (Lexing.lexeme lexbuf)
        (Lexing.lexeme_start lexbuf)
        (Lexing.lexeme_end lexbuf)
      in
      failwith message
    }
