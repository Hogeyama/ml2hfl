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

(* $Id: treat_polish.ml,v 1.2 2011-08-11 15:51:27 weis Exp $ *)

open Format;;

type operator = string;;

type config = {
  print_ast : bool;
  print_scope_ast : bool;
  print_transl_ast : bool;
  print_splice_ast : bool;
  print_polish : bool;
}
;;

let polish_config = {
  print_ast = false;
  print_scope_ast = false;
  print_transl_ast = false;
  print_splice_ast = false;
  print_polish = true;
}
;;

let debug_config = {
  print_ast = true;
  print_scope_ast = true;
  print_transl_ast = true;
  print_splice_ast = true;
  print_polish = true;
}
;;

let config = ref polish_config;;
let set_debug () = config := debug_config;;

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

let print_globals ppf glbs =
  let pr_glbs ppf glbs =
    for i = 0 to Array.length glbs - 1 do
      Format.fprintf ppf "@ %d : %a; @ " (i + 1)
        Splice_ast_pprint.pprint glbs.(i)
    done in
  Format.fprintf ppf "[@[<v 2>%a@]@.]" pr_glbs glbs
;;

let treat_ast config ast =
  if config.print_ast then begin
    Ast_print.print_expression ast;
    print_newline ();
  end;

  if config.print_scope_ast then Format.printf "@.Scoping:@.";
  let ast = Scoping.scope ast in
  if config.print_scope_ast then begin
    Ast_print.print_expression ast;
    print_newline ();
  end;

  if config.print_transl_ast then Format.printf "@.Translating:@.";
  let ast = Transl.translate ast in
  if config.print_transl_ast then begin
    Deep_ast_print.print_expression ast;
    print_newline ();
  end;

  if config.print_splice_ast then Format.printf "@.Splicing:@.";
  let ast = Splicing.splice ast in
  if config.print_splice_ast then begin
    Splice_ast_print.print_expression ast;
    print_newline ();
  end;

  let (_accu_args, _accu_ops as ast) = Polish.polish ast in
  if config.print_polish then begin
    Format.printf "@.Polishing:@.";
    Format.printf "%a@." Print_polish.print ast;
  end;

  ast
;;

let treat_file_gen config fname =
  if config.print_ast then Format.printf "@.Parsing:@.";
  let ast = parse_file fname in
  treat_ast config ast
;;

let treat_string_gen config s =
  if config.print_ast then Format.printf "@.Parsing:@.";
  let ast = parse_string s in
  treat_ast config ast
;;

let treat_file f = treat_file_gen !config f
;;

let treat_string s = treat_string_gen !config s
;;
