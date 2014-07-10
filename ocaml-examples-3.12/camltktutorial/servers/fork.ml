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

(* $Id: fork.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* The pipe module, to launch two programs connected as in a pipe. *)

(* The shell version:

#!/bin/sh

exec ($1 | $2)

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
  let p = Unix.pipe () in
  match Unix.fork () with
  | 0 -> connect_stdin proc2 p
  | _ -> connect_stdout proc1 p
;;

let launch prog () = Unix.execv prog [| prog |];;

let launch_connected_processes prog1 prog2 =
  pipe_connect (launch prog1) (launch prog2)
;;
