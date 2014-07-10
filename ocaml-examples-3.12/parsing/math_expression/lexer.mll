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

(* $Id: lexer.mll,v 1.2 2011-08-08 18:21:34 weis Exp $ *)

{

(* Prelude part: this is pure Caml. *)

open Lexing;;
open Parser;;

(** {6 Lexing errors} *)

type error =
   | Illegal_character of char
(** The various errors when lexing. *)
;;

exception Error of error * Lexing.position * Lexing.position;;

(** {6 Explaining lexing errors} *)

let report_error ppf = function
  | Illegal_character c ->
      Format.fprintf ppf "Illegal character (%C)" c
;;

(** {6 The keyword table} *)

let keyword_table = Hashtbl.create 42;;

List.iter
 (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) [
  "else", ELSE;
  "fi", FI;
  "if", IF;
  "then", THEN;
]
;;

let token_of_lowercase_ident s =
  assert (String.length s > 0);
  try Hashtbl.find keyword_table s with
  | Not_found -> IDENT s
;;

let token_of_lowercase_prefix_symbol s =
  let length_s = String.length s in
  assert (length_s > 0);
  match s.[0] with
  | '!' ->
    begin match length_s with
    | 1 -> BANG_OP s
    | _ ->
      begin match s.[1] with
      | '=' -> BANG_EQ_OP s
      | _ -> BANG_OP s
      end
    end
  | _ -> assert false
;;

let token_of_lowercase_infix_symbol s =
  let length_s = String.length s in
  assert (length_s > 0);
  let c = s.[0] in
  match c with
  | '.' ->
    begin match length_s with
    | 1 -> DOT_OP s
    | _ ->
      begin match s.[1] with
      | '\'' -> DOT_QUOTE_OP s
      | '^' -> DOT_HAT_OP s
      | '*' -> DOT_STAR_OP s
      | '/' -> DOT_SLASH_OP s
      | '\\' -> DOT_BACKSLASH_OP s
      | _ -> DOT_OP s
      end
    end
  | '^' -> HAT_OP s
  | '+' -> PLUS_OP s
  | '-' -> DASH_OP s
  | ':' -> COLON_OP s
  | '*' ->
    begin match length_s with
    | 1 -> STAR_OP s
    | _ when s.[1] <> '*' -> STAR_OP s
    | _ -> STAR_STAR_OP s
    end
  | '/' -> SLASH_OP s
  | '\\' -> BACKSLASH_OP s
  | '%' -> PERCENT_OP s
  | '&' ->
    begin match length_s with
    | 1 -> AMPER_OP s
    | _ when s.[1] = '&' -> AMPER_AMPER_OP s
    | _ -> AMPER_OP s
    end
  | '|' ->
    begin match length_s with
    | 1 -> BAR_OP s
    | _ when s.[1] = '|' -> BAR_BAR_OP s
    | _ -> BAR_OP s
    end
  | '<' -> LT_OP s
  | '=' -> EQ_OP s
  | '>' -> GT_OP s
  | '!' ->
    begin match length_s with
    | 1 -> BANG_OP s
    | _ when s.[1] = '=' -> BANG_EQ_OP s
    | _ -> BANG_OP s
    end
  | _ -> assert false
;;

(** {6 Keeping the internal buffer locations up to date} *)

let update_loc lexbuf fname line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_fname =
    match fname with
    | None -> pos.pos_fname
    | Some s -> s in
  lexbuf.lex_curr_p <- {
    pos with
    pos_fname = new_fname;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  }
;;

(** Add one to the current line counter of the file being lexed. *)
let incr_line_num lexbuf =
  update_loc lexbuf None 1 false 0
;;

}

(** The lexer: this is the pure ocamllex part. *)

let newline = ( '\n' | '\r' | "\r\n" )
let blank = [ ' ' '\t' '\012' ]

(** {6 Floating point numbers} *)

let decimal_digit = [ '0'-'9' ]
let unsigned_decimal_literal = decimal_digit ( decimal_digit | '_' )*

let sign = [ '+' '-' ]

let decimal_literal = sign? unsigned_decimal_literal

let scientific_notation = [ 'e' 'E' 'd' 'D' ]

let unsigned_decimal_float_literal =
  unsigned_decimal_literal
  ('.' unsigned_decimal_literal* )?
  (scientific_notation decimal_literal)?

let float_literal = unsigned_decimal_float_literal

(** {3 Identifiers} *)
let lowercase_alphabetic = [ 'a'-'z' ]
let uppercase_alphabetic = [ 'A'-'Z' ]

let inside_ident =
    lowercase_alphabetic
  | uppercase_alphabetic
  | decimal_digit
  | '_'

let lowercase_ident =
    lowercase_alphabetic inside_ident*
  | uppercase_alphabetic inside_ident*

let symbolic =
  [ '/' '%' '&' '|' '<' '=' '>' '^' ]

(** Characters that can start a lowercase infix symbol. *)
let start_lowercase_infix_symbolic =
    '*' | '.' | '^' | ':' | '!'
  | sign
  | symbolic

let start_lowercase_prefix_symbolic = [ '~' '\'']

(* Symbolic identifiers. *)
let lowercase_infix_symbol = start_lowercase_infix_symbolic+
let lowercase_prefix_symbol = start_lowercase_prefix_symbolic symbolic*

(** {3 The main lexer. *)

rule token = parse
  | newline
    { incr_line_num lexbuf;
      token lexbuf }
  | blank +
    { token lexbuf }

  (* Numbers *)
  | float_literal
    { FLOAT (Lexing.lexeme lexbuf) }

  (* Identifiers *)
  | lowercase_ident
    { token_of_lowercase_ident (Lexing.lexeme lexbuf) }

  (* Symbols (or symbolic idents) *)
  | lowercase_prefix_symbol
    { token_of_lowercase_prefix_symbol (Lexing.lexeme lexbuf) }
  | lowercase_infix_symbol
    { token_of_lowercase_infix_symbol (Lexing.lexeme lexbuf) }

  (* Usual simple tokens *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character
              (Lexing.lexeme_char lexbuf 0),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p)) }
