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

(* $Id: lexer.mll,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

{

(* Prelude part: this is pure Caml. *)

open Lexing;;
open Parser;;

(** {6 Lexing errors} *)

type error =
   | Illegal_character of char
   | Illegal_escape of string
   | Unterminated_string
(** The various errors when lexing. *)
;;

exception Error of error * Lexing.position * Lexing.position;;

(** {6 Explaining lexing errors} *)

let report_error ppf = function
  | Illegal_character c ->
      Format.fprintf ppf "Illegal character (%C)" c
  | Illegal_escape s ->
      Format.fprintf ppf "Illegal escape (%S)" s
  | Unterminated_string ->
      Format.fprintf ppf "Unterminated string"
;;

(** {6 The keyword table} *)

let dot_keyword_table = Hashtbl.create 42;;

List.iter
 (fun (kwd, tok) -> Hashtbl.add dot_keyword_table kwd tok) [
  ".if", DOT_IF;
  ".then", DOT_THEN;
  ".else", DOT_ELSE;
  ".end", DOT_END;
  ".switch", DOT_SWITCH;
  ".case", DOT_CASE;
  ".otherwise", DOT_OTHERWISE;
  ".end", DOT_END;
]
;;

let token_of_dot_lowercase_ident s =
  assert (String.length s > 0);
  try Hashtbl.find dot_keyword_table s with
  | Not_found -> DOT_IDENT (String.sub s 1 (String.length s - 1))
;;

let keyword_table = Hashtbl.create 42;;

List.iter
 (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok) [
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "end", END;
]
;;

let token_of_lowercase_ident s =
  assert (String.length s > 0);
  try Hashtbl.find keyword_table s with
  | Not_found -> IDENT s
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

(** {6 Lexing the string tokens} *)
let initial_string_buffer = String.create 256;;

let string_buff = ref initial_string_buffer
and string_index = ref 0
;;

let string_start_pos = ref None;;

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
;;

let store_string_char c =
  if !string_index >= String.length !string_buff then begin
    let new_buff = String.create (String.length !string_buff * 2) in
      String.blit !string_buff 0
                  new_buff 0 (String.length !string_buff);
      string_buff := new_buff
  end;
  String.unsafe_set !string_buff !string_index c;
  incr string_index
;;

let get_stored_string () =
  let s = String.sub !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s
;;

let char_for_character = function
  | '\\' -> '\\'
  | '\"' -> '\"'
  | 'n' -> '\n'
  | 't' -> '\t'
  | 'b' -> '\b'
  | 'r' -> '\r'
  | c ->
    failwith
      (Printf.sprintf "polish: lexer error, unknown escaped character %C" c)
;;

}

(** The lexer: this is the pure ocamllex part. *)

let newline = ( '\n' | '\r' | "\r\n" )
let blank = [ ' ' '\t' '\012' ]
let comment_char = [^ '\n' '\r' ]
let comment_chars = comment_char*

(** {6 Floating point numbers} *)

let decimal_digit = [ '0'-'9' ]
let unsigned_decimal_literal = decimal_digit ( decimal_digit | '_' )*

let sign = [ '+' '-' ]

let decimal_literal = sign? unsigned_decimal_literal

let scientific_notation = [ 'e' 'E' 'd' 'D' ]

let unsigned_decimal_float_literal =
  unsigned_decimal_literal?
  ('.' unsigned_decimal_literal* )?
  (scientific_notation decimal_literal)?

let float_literal = unsigned_decimal_float_literal

(** {6 Strings} *)
let escaped_character =
    "\\" [ '\\' '\"' 'n' 't' 'b' 'r' ]

(** {3 Identifiers} *)
let lowercase_alphabetic = [ 'a' - 'z' ]
let uppercase_alphabetic = [ 'A' - 'Z' ]

let inside_ident =
    lowercase_alphabetic
  | uppercase_alphabetic
  | decimal_digit
  | '_'

let lowercase_ident =
    lowercase_alphabetic inside_ident*
  | uppercase_alphabetic inside_ident*

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

  (* Strings *)
  | '\"'
    {
      reset_string_buffer ();
      string_start_pos :=
        Some (lexbuf.lex_start_p, lexbuf.lex_curr_p);
      string lexbuf;
      STRING (get_stored_string ())
    }

  (* Parameters *)
  | 'u' ([ '1' - '9' ] [ '0' - '9' ]*) ?
    { PARAM (Lexing.lexeme lexbuf) }

  (* Special keywords starting with a dot *)
  | '.' lowercase_ident
    { token_of_dot_lowercase_ident (Lexing.lexeme lexbuf) }

  (* Identifiers *)
  | lowercase_ident
    { token_of_lowercase_ident (Lexing.lexeme lexbuf) }

  | "+" { PLUS_OP "+" }
  | "-" { DASH_OP "-" }
  | "*" { STAR_OP "*" }
  | "/" { SLASH_OP "/" }
  | "\\" { BACKSLASH_OP "\\" }
  | "^" { HAT_OP "^" }

  | ".\'" { DOT_QUOTE_OP ".\'" }
  | ".*" { DOT_STAR_OP ".*" }
  | "./" { DOT_SLASH_OP "./" }
  | ".\\" { DOT_BACKSLASH_OP ".\\" }
  | ".^" { DOT_HAT_OP ".^" }

  | "|" "||" { BAR_BAR_OP (Lexing.lexeme lexbuf) }
  | "&" "&&" { AMPER_AMPER_OP (Lexing.lexeme lexbuf) }

  | "<" | ">" | "<=" | ">=" | "=" | "==" | "~=" | "!=" | "<>"
    { COMPARE_OP (Lexing.lexeme lexbuf) }

  | '~' | '!' { NOT_OP (Lexing.lexeme lexbuf) }

  | ":=" { COLON_EQUAL }

  (* Usual simple tokens *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | '.' { DOT }
  | ';' { SEMI }
  | ':' { COLON }
  | '$' { DOLLAR }
  | '\'' { QUOTE }
  | "%" comment_chars newline
    { token lexbuf }
  | "//" comment_chars newline
    { token lexbuf }

  | eof { EOF }
  | _
    { raise
        (Error
           (Illegal_character
              (Lexing.lexeme_char lexbuf 0),
              lexbuf.lex_start_p,
              lexbuf.lex_curr_p)) }

and string = parse
  | '\"'
    { () }
  | escaped_character
    { store_string_char (char_for_character (Lexing.lexeme_char lexbuf 1));
      string lexbuf }
  | '\\' _
    { raise
        (Error
           (Illegal_escape (Lexing.lexeme lexbuf),
           lexbuf.lex_start_p,
           lexbuf.lex_curr_p)) }
  | eof
    { match !string_start_pos with
      | Some (start_pos, end_pos) ->
        raise (Error (Unterminated_string, start_pos, end_pos))
      | _ -> assert false }
  | _
    { store_string_char (Lexing.lexeme_char lexbuf 0);
      string lexbuf }
