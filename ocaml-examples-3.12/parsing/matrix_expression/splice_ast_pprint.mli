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

(* $Id: splice_ast_pprint.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

val pprint: Format.formatter -> Splice_ast.expression -> unit;;
val pstring_of_spliced_expression: Splice_ast.expression -> string;;
