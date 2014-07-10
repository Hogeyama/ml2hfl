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

(* $Id: fib.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

(** The Fibonacci function, once more. *)

(**
- Global or toplevel definitions have the syntax

  [let ident = expression;;]

- If the definition is recursive, you must write

  [let rec] instead of [let].
*)

let rec fib n =
  if n < 2 then 1 else fib (n - 1) + fib (n - 2)
;;

(**
- In the branches of conditionals, sequences must be enclosed into
  [begin ... end].

  Hence, use

  [if cond then begin e1; e2 end else begin e3; e4 end]

- Some functions may fail to operate properly, in case of bad input.
  In those situations, the function normally stops the computation, by
  <EM>raising some exception</EM>. For instance, [int_of_string] raises
  ``[Failure "int_of_string"]'' when called with a string that cannot be
  interpreted as a number.

  You can check this situation, and continue the computation
  appropriately, using the

  [try ... with ...]

  construct.

  For instance, if you expect the expression [expr] can fail raising exception
  [Exc], use

    [try expr with Exc -> e]

  to prevent the computation to stop if [expr] cannot operate properly.

  The construct [try expr with Exc -> e],

  - is equivalent to [expr], when the expression [expr] evaluates normally,
  - is equivalent to [e],    when the expression [expr] fails to evaluate and
    raises exception [Exc].
*)

let main () =
  if Array.length Sys.argv <> 2 then begin
    print_string "Usage: fib <number>";
    print_newline ()
  end else begin
    try
      print_int (fib (int_of_string Sys.argv.(1)));
      print_newline ()
    with Failure "int_of_string" ->
      print_string "Bad integer constant";
      print_newline ()
  end
;;

(**
- [!Sys.interactive] tests if the program is called from within the interactive
   system or not.

  If the program is indeed called from the interactive system, we just do
  nothing and let the user to call the [fib] function directly.

  Otherwise, we call the main procedure.
*)
if !Sys.interactive then () else main ()
;;
