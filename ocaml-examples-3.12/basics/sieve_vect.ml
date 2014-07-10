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

(* $Id: sieve_vect.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** Erathostene sieve, imperative version.

   A vector is initialized with consecutive integers,
   then the vector is sieved to remove non prime integers.

   The sieve proceeds by removing all the multiples of the next still
   not removed integer, starting from 2. *)

let fixed_bound = Sys.max_array_length
;;

let sieve max =

 let v = Array.init max (fun i -> i + 1) in

 let prime_count = ref 0 in

 v.(0) <- 0;
 prime_count := 0;

 for i = 1 to max - 1 do
  if v.(i) <> 0 then begin
   prime_count := !prime_count + 1;
   let prime = i + 1 in
   let rec sieve j =
    if j < max then begin v.(j) <- 0; sieve (j + prime) end in
   sieve (i + prime)
  end
 done;
 Printf.printf
  "There are %d primes less than or equal to %d.\n" !prime_count max
;;

let main () =
 let max =
   try int_of_string (Sys.argv.(1)) with
   | _ -> fixed_bound in
 sieve max
;;

main ()
;;
