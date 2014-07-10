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

(* $Id: main.ml,v 1.4 2011-08-08 19:11:31 weis Exp $ *)

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
