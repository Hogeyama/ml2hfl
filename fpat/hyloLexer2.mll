(* 仕様用 *)

{
open Util
open Combinator
open HyloParser2
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
  
  (* 括弧類 *)
  | '('       { LPAREN }
  | ')'       { RPAREN }
  
  (* 区切り記号 *)
  | '='       { EQUAL }
  | ">"       { GREATER }
  | "<"       { LESS }
  | ">="      { GEQUAL }
  | "<="      { LEQUAL }
  | "!="      { NOTEQUAL }
  | '.'       { DOT }

  (* キーワード *)
  | "true"    { TRUE }
  | "false"   { FALSE }

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
