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

(* $Id: treat.ml,v 1.5 2011-08-08 19:11:31 weis Exp $ *)

let parse lexbuf =
  Parser.file Lexer.token lexbuf
;;

let parse_file fname =
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let ast = parse lexbuf in
  close_in ic;
  ast
;;

let parse_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf
;;

let treat_ast ast =
  Scoping.scope ast;
  let ppf = Format.std_formatter in
  Format.fprintf ppf "%a@." Print_ast.print ast
;;

let treat_file fname =
  treat_ast (parse_file fname)
;;

let treat_string s =
  treat_ast (parse_string s)
;;
