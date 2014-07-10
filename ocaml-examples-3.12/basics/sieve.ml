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

(* $Id: sieve.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(** Erathostene sieve, functional version.

  We create a list of consecutive integers, then this list is sieved to
  remove non prime integers in it.

  The sieve proceeds by removing all the multiples of the next still
  not removed integer, starting from 2. *)

(** List are predefined in Caml.

    Lists are defined as follows:

    - [] is the empty list,
    - :: is the infix operator that builds list cells.

    Hence [1 :: (2 :: \[\])] is the list that contains 1 and 2 in this order.

    - due to the right associativity rule for the constructor [::],
      - [1 :: (2 :: \[\])] is equivalent to [1 :: 2 :: \[\]].

    - to cite a list, you can use the corresponding special syntax: just
      write the list (!) of the elements between brackets.
      - [\[0; 1; 2; 3; 4; 5; 6; 7; 8; 9; \]] is the list of successive
      integers from [0] to [9].

    The manipulation of lists is easy and ubiquitous in functional
    programming.
*)

(** [interval min max] returns the list of integers between [min] and [max]:
    [interval min max = \[min; min + 1; ...; max - 1; max\]]. *)
let rec interval min max =
  if min > max then [] else min :: interval (succ min) max
;;

(**

  Case analysis on list [l] is written:

  [match l with
  | \[\] -> ``nil case''
  | x :: tail -> ``non nil case,
                 with x (the head of l) and tail (the tail of l) available''
  ]

  All functions can perform direct case analysis on their (implicit) argument,
  if introduced by the function keyword.

  Hence,

    [let f (x) =
      match x with
      | \[\] -> ...
      | ...]

  can be abbreviated as:

    [let f = function
      | \[\] -> ...
      | ...]

*)

(** [filter p l] returns the list of all the elements in list [l]
   that satisfy the predicate [p]. *)
let rec filter p = function
  | []  -> []
  | a :: r -> if p a then a :: filter p r else filter p r
;;

(** Application: removing all the numbers that are multiple of some integer
    [n] in a list of integers.

    We simply ``filter'' the list, using the predicate that tests
    if a number [m] is not a multiple of [n] (i.e. [m mod n <> 0]). *)
let remove_multiples_of n =
  filter (fun m -> m mod n <> 0)
;;

(** The sieve itself:
    to find the list of prime numbers lesser of equal to [max] we repeatedly
    filter the list of integers from [2] to [max] to remove the multiple of
    prime numbers in it (hence starting from [2]).

    We stop the sieve when the sieved list only contains prime numbers (this
    is granted when the square of the smaller prime of the list is greater
    than [max]). *)

let sieve max =
  let rec filter_again = function
    | [] -> []
    | n :: r as l ->
      if n * n > max then l else n :: filter_again (remove_multiples_of n r) in
  filter_again (interval 2 max)
;;

let usage() =
  print_string "Usage: sieve <n>\n";
  exit 2
;;

(* The entry point *)

let main () =
  if Array.length Sys.argv <> 2 then usage () else
  begin
    try
      let n = int_of_string Sys.argv.(1) in
      List.iter (fun n -> print_int n; print_string " ") (sieve n);
      print_newline ()
    with Failure "int_of_string" ->
      print_string "Bad integer constant";
      print_newline ()
  end
;;

if !Sys.interactive then () else main ()
;;
