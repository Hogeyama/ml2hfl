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

(* $Id: main_polish.ml,v 1.2 2011-08-11 15:51:44 weis Exp $ *)

module Treat = Treat_polish;;

let usage () =
  prerr_endline "polish [-d] [ -s <\"string\"> | -c <filename> ]";
  exit 2;
;;

let treat_args argv i =
  match argv.(i) with
  | "-s" -> Treat.treat_string argv.(i + 1)
  | "-c" -> Treat.treat_file argv.(i + 1)
  | opt ->
    prerr_endline (Printf.sprintf "Unknown option %s" opt);
    usage ()
;;

let main () =
  let argv = Sys.argv in
  let argc = Array.length argv in
  match argc with
  | 3 -> treat_args argv 1
  | 4 ->
    if argv.(1) <> "-d" then usage () else begin
      Treat_polish.set_debug ();
      treat_args argv 2;
    end
  | _ -> usage ()
;;

main ()
;;
