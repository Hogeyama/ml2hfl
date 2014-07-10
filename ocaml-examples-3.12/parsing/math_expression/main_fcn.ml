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

(* $Id: main_fcn.ml,v 1.2 2011-08-08 18:21:34 weis Exp $ *)

module Treat = Treat_fcn;;

let usage () =
  prerr_endline "polish [ -s <\"string\"> | -c <filename> ]";
  exit 2
;;

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  if argc = 3 then
   match argv.(1) with
   | "-s" -> Treat.treat_string argv.(2)
   | "-c" -> Treat.treat_file argv.(2)
   | opt -> prerr_endline (Printf.sprintf "Unknown option %s" opt); usage () else
  if argc = 2 then Treat.treat_string argv.(1) else usage ()
;;

main ()
;;
