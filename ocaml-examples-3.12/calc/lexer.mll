(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexer.mll,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

{
open Parser;;        (* The type token is defined in parser.mli *)
exception Eof;;
}
rule token = parse
  | [' ' '\t']     { token lexbuf } (* Skip blanks *)
  | ['\n' ]        { EOL }
  | ['0'-'9']+     { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }
