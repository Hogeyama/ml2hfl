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

(* $Id: connect.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* The biconnect program that connects two programs via stdin/stdout. *)

let usage () =
  prerr_endline "Usage: connect prog1 prog2";
  exit 2
;;

let main () =
  let args = Sys.argv in
  if Array.length args <> 3 then usage () else
  Pipe.launch_connected_processes args.(1) args.(2)
;;

main ()
;;
