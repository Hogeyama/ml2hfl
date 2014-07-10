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

(* $Id: fib.ml,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

(** Exporting Caml functions to the C world.

 To export a Caml function to the C world, we must register this function as
 a {e call back}, that is a function that can be retrieved from C (hence
 called from C), using the C function [callback] (the function [callback] is
 provided to C by the Caml runtime library).

 To register a Caml function [f] as a {e call back} we must call the
 predefined Caml function [Callback.register] that registers a name and a
 functional value for [f]; the registered name is the name that will be
 available to the C world to call the function [callback] and retrieve and
 call the Caml functional value [f]. So, to properly register [f] for C, we
 evaluate:
 [Callback.register "f" f;;]

 In fact, since we have:
 [
  # Callback.register;;
  - : string -> 'a -> unit = <fun>
 ]
 the registered name can be any string and the functional value can be any
 Caml value; of course, to prevent extreme confusion and errors, the
 registered name and the associated values should be tightly correlated to
 the function defined in Caml: always register the functional value [f]
 with the name ["f"]!

*)

(* File fib.ml: define some regular Caml functions. *)

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2);;

let format_result n = Printf.sprintf "Result is: %d" n;;

(** Export the fib and format_result Caml functions to the C world. *)
Callback.register "fib" fib;;
Callback.register "format_result" format_result;;
