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
(* $Id: token.mli,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

type token_type =
   | IDENT of string | INT of int | OP of string
   | BSLASH | DOT | ELSE | EQUAL | FI | IF | LET | LPAREN | RPAREN | SEMICOL
   | THEN
;;

val next_token : char Stream.t -> token_type;;
val reset_lexer : char Stream.t -> unit;;
val token_name : token_type -> string;;
