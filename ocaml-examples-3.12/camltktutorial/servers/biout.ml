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

(* $Id: biout.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(* The biout module, to launch two programs connected as in a biout. *)

(* father_stdin -- father_stdout
   p1_in -- p1_out
   p2_in -- p2_out

Fork:
- Needs two forks:
  one to launch a child that would launch the two forked processes.
- child (0) connects its stdin (hence stdin of process1)
  to stdout of pipe p1, then launches process1
- father (cpid) connects its stdin (hence stdin of process2)
  to stdout of pipe p2, then launches process2

Join is the same:
- Needs two forks:
*)

(* The Caml version, using module Unix. *)
let connect_stdin proc (fdin, fdout) =
  Unix.close fdout;
  Unix.dup2 fdin Unix.stdin;
  Unix.close fdin;
  proc ()
;;

let connect_stdout proc (fdin, fdout) =
  Unix.close fdin;
  Unix.dup2 fdout Unix.stdout;
  Unix.close fdout;
  proc ()
;;

let pipe_connect proc1 proc2 =
  let p1 = Unix.pipe () in
  let p2 = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdin proc2 p1
  | cpid -> connect_stdout proc1 p2
;;

let launch prog () = Unix.execv prog [| prog |];;

let launch_connected_processes prog1 prog2 =
  pipe_connect (launch prog1) (launch prog2)
;;
