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

(* $Id: name.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

type name = string;;

let print_name id = Lib_pp.print_quoted_string (String.escaped id)
;;

type path = string;;

let print_path id = Lib_pp.print_quoted_string (String.escaped id)
;;
