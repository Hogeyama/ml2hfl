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

(* $Id: types.mli,v 1.2 2011-08-08 19:31:17 weis Exp $ *)

(** {3 The data structures to solve the Sudoku game} *)

(** A (solved) sudoku board is an array of digits.
    An (unsolved) sudoku board is an array containing either nothing
    (represented as a white space in problems), or a decimal digit between
    one (1) to nine (9) and between ten (A) to  fifteen (15) (inclusive).
*)

(** {6 The digits modelisation} *)

(**
    We could represent digits as string values (["1", "2", ..., "15"]); the
    problem with this data representation is that any character string
    could be a digit, which is clearly wrong.
    Among all those {\em admissible} representations for digits,
    we must carefully prohibit all the values not in the list
    ["1", "2", ..., "15"] which are the only {\em valid} string
    representations for digits.
    Prohibiting the admissible but invalid representations for the values
    that your program manipulates is a complex and error prone process
    (since this is a non-local problem: you must carefully check the creation
    {\em and} the manipulation of the values in {\em all the routines}
    of the program).

    To circumvent this problem, we could choose to represent digits
    as true integers; this representation is more reasonable, since it
    restricts the range of admissible representations.
    Unfortunately, this is not yet a faithful data representation,
    since any representable integer in the range [\[min_int .. max_int\]]
    would be an admissible representation of a digit (even if the
    integer is indeed negative!). Once again, we clearly have the
    {\em unfaithful data representation} problem to face with.

    Representing digits as characters (i.e. Caml [char] values) is again
    better, since we restrict the admissible representations of digits
    to 255 values; the unfaithful data representation problem is still
    there since we must prohibit all the characters not in the range
    ['1'..'9''a'..'f'].

    To obtain a faithful representation we use a simple enumerated type
    definition:
    {[
    type digit =
       | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
       | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen
    ;;
    ]}
*)
(** An obvious and simple way to model the digits written on the Sudoku board
    could just be:

{[type digit = string;;]}

Now, the strings ["1"], ["2"], ["3"], ["4"], ["5"], ["6"], ["7"], ["8"], and
 ["9"] correctly represent digits.

However, this encoding is not adequate at all, since any string value can be
considered as a digit, including the string ["Hello world!"], which is
clearly wrong. Since we will get no help from the type-checker to prevent
wrong usage of erroneous string values as digits, we consider this encoding
as dangerous and not acceptable.

We could choose a more restricted type for digits

{[type digit = char;;]}

The choice is a bit better, since ["Hello world!"] is no more considered a
digit. Unfortunately, ['a'] and ['z'] still are, and we must also abandon
this idea.

Considering that digits are in fact integer values, we can choose:

{[type digit = int;;]}

Once more, digits 1 to 9 are correctly represented and ["Hello world"],
['a'], and ['z'] are rejected. Unfortunately, we still have too much values
considered as digits, such as [0], [10], or [-1]. You could argue that [0] is
effectively a digit in the decimal representation of numbers; well that's
true for the notation, but [0] is still not an acceptable digit for the Sudoku
game!  Admittedly, [10] can also be considered a digit, if you use an
hexadecimal notation in your Sudoku variant; but in any case, [-1] cannot be a
digit in any basis.

So we must explicitely enumerate acceptable digits in a type of our own, a
simple enumeration of constant constructors in a {e sum} concrete data type:
*)

type digit =
   | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
   | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen
;;

(** {6 Representation of the Sudoku board} *)

(** Now that we have a faithful data representation for digits, we must choose
    a (faithful) representation for the contents of the board elements.
*)

(** {7 Contents of a board place} *)

(**
    The contents of a board element is either a digit, or a space on the
    paper.

    We represent the contents of a board element as a [digit option].

    We could have chosen to had a fake digit that would represent the empty
    or space case for a board element; once again, this would be ugly, since a
    space is not a valid digit.
*)

(** {7 The solved Sudoku board} *)

(**
    The dimension of the board is a square matrix of n (n being between 4 to
    sixteen) columns by n rows.
    We associate to each item of the matrix the list of possible digits
    to this place of the board.

    The traditional Sudoku board is a square with 9x9 digits.  We represent
    the board as a matrix of 9 columns by 9 rows, i.e. an array of 9 rows
    with each row an array of 9 elements.

    In summary, a board is an array of arrays that content an optional digit:
*)
type out_board = {
  out_h : int;
  out_w : int;
  out_height : int;
  out_width : int;
  out_board : (digit option) array array;
};;
(**
  If h is the height of an elementary rectangle,
     w is the width of an elementary rectangle,
  then
    the board has
     height = h * w,
     width = w * h.

   E.g. h = 2, w = 3
        height = h * w = 6
        width =  w * h = 6
*)

(** {7 The unsolved Sudoku board} *)

(** We must associate to each place the list of surely not present digits
   to this place of the board.
*)

(** Element with explicit numbering with number and letters, as in the chess
board.

Loosy representation:

(** The board to program the game. *)

A more explicit representation for unsolved Sudoku board could explicit
letters, places, sets of digits, and place contents.

type place = digit * letter
;;

For sets, we stil need to implement the following functions:

{[
   val add : 'a * 'a set -> 'a set;;
   val complement : 'a set -> 'a set;;
   val remove : 'a * 'a set -> 'a set;;
   val difference : 'a set * 'a set -> 'a set;;
]}

{[
type place_contents =
   | Not_here of digit set
   | May_be of digit set
;;

type in_board = (place, (place_contents * place_contents)) Hashtbl.t
;;
]}

*)

type in_board = {
  in_h : int;
  in_w : int;
  in_height : int;
  in_width : int;
  in_board : (digit list) array array;
};;
(** The board to program the game. *)

type chosen_place =
   | Place of digit * int * int
   | No_place of in_board
   | Backtrack

type solution =
   | No_solution
   | Unique_solution of in_board
   | Multiple_solutions of in_board * (in_board list)
;;
