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

(* $Id: main.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)


open Format;;


(*** Types ***)
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

(* $Id: types.mli,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(** {3 The data structures to solve the Sudoku game} *)

(** A (solved) sudoku board is an array of digits.
    An (unsolved) sudoku board is an array containing either nothing
    (represented as a white space in Sudoku problems on papers), or a decimal digit between
    one (1) to nine (9) (inclusive).

    We must represent solved Sudoky and unsolved Sudoku boards.
*)

(** {6 The digits modelisation} *)

(**
    We could represent digits as string values (["1", "2", ..., "9"]); the
    problem with this data representation is that any character string
    could be a digit, which is clearly wrong.
    Among all those {\em admissible} representations for digits,
    we must carefully prohibit all the values not in the list
    ["1", "2", ..., "9"] which are the only {\em valid} string
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
    ['1'..'9'].

    To obtain a faithful representation we use a simple enumerated type
    definition:
    {[
    type digit = | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
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

(** The traditional Sudoku board is a square with 9x9 digits.  We represent
    the board as a matrix of 9 columns by 9 rows, i.e. an array of 9 rows
    with each row an array of 9 elements.

    In summary, a board is an array of arrays that content an optional digit:
*)
type out_board = (digit option) array array
;;

(** {7 The unsolved Sudoku board} *)

(** We must associate to each place the list of surely not present digits
   to this place of the board.
*)

(** Element with explicit numbering with number and letters, as in the chess
board.

Loosy representation:

type board = (int * char) * string Hashtbl.t
;;

A more explicit representation for unsolved Sudoku board could explicit
letters, places, sets of digits, and place contents.
*)

type letter =
   | A | B | C | D | E | F | G | H | I
;;

(**
type place = digit * letter
;;

type 'a set =
   | Empty
   | Cons of 'a * 'a set
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

type in_board = (digit list) array array;;
(** The board to program the game. *)

exception Solution;;
(** The exception raised when a solution has been found. *)
exception Backtrack;;
(** The exception raised when no solution has been found: we need to explore
    another path in the board. *)


(*** Read ***)
external _Read_board_of_string : string -> digit option array array = "%board_of_string"
(*
let digit_of_char = function
  | '1' -> One
  | '2' -> Two
  | '3' -> Three
  | '4' -> Four
  | '5' -> Five
  | '6' -> Six
  | '7' -> Seven
  | '8' -> Eight
  | '9' -> Nine
  | c -> assert false
;;

let current_position ib =
  Scanf.bscanf ib "%l%n" (fun ln cn -> ln, cn)
;;

let skip_delimitors ib = Scanf.bscanf ib " %[-] " (fun s ->
 (* prerr_endline (Printf.sprintf "skip delimitors `%s'" s); *)
 s)
;;

let rec read_one_digit ib =
  Scanf.bscanf ib "%c" (function
  | '1'..'9' as c -> Some (digit_of_char c)
  | ' ' -> None
  | '|' | '-' -> read_one_digit ib
  | '\n' -> Scanf.bscanf ib " %r" read_one_digit (fun d -> d)
  | c ->
    let line_number, character_number = current_position ib in
    failwith
      (Printf.sprintf
         "%s: line %i, character %i, invalid character `%c'"
         (Scanf.Scanning.name_of_input ib) line_number character_number c))
;;

let read_row len ib = Array.init len (fun i ->
(* prerr_endline (Printf.sprintf "one digit %i" i);*)
 read_one_digit ib)
;;

let read_board_gen len ib =
     let _ = skip_delimitors ib in ();
  Array.init len
    (fun i ->
(* prerr_endline (Printf.sprintf "one row %i" i); *)
     let _ = skip_delimitors ib in ();
     let row = read_row len ib in
     row)
;;

let read_board ib = read_board_gen 9 ib
;;

let input_board () = read_board Scanf.Scanning.stdib
;;

let _Read_board_of_string s = read_board (Scanf.Scanning.from_string s)
;;

let board_of_file fname = read_board (Scanf.Scanning.from_file fname)
 *)



(*** Utils ***)
let _Utils_debug_flag = ref false;;

let _Utils_int_of_digit = function
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
;;

let print_digit ppf d = fprintf ppf "%i" (_Utils_int_of_digit d);;

let print_digit_option ppf = function
  | None -> fprintf ppf "%s" "*"
  | Some d -> print_digit ppf d
;;

let print_upper_edge ppf =
  fprintf ppf "%s" "\
  \   123 456 789\
 \n   -----------"
;;

let print_lower_edge ppf =
  fprintf ppf "%s" "\
  \   -----------"
;;

let print_horizontal_edge ppf =
  fprintf ppf "%s" "\n"
;;

let char_of_digit = function
  | A -> 'A'
  | B -> 'B'
  | C -> 'C'
  | D -> 'D'
  | E -> 'E'
  | F -> 'F'
  | G -> 'G'
  | H -> 'H'
  | I -> 'I'
;;

let print_letter ppf l = fprintf ppf "%c" (char_of_digit l);;

let letters = [| A; B; C; D; E; F; G; H; I; |];;

let _Utils_digits = [ One; Two; Three; Four; Five; Six; Seven; Eight; Nine; ];;

let print_out_board ppf board =
  print_upper_edge ppf;
  for i = 0 to 8 do
    fprintf ppf "\n";
    fprintf ppf "%a " print_letter letters.(i);
    for j = 0 to 8 do
      if j mod 3 = 0 then fprintf ppf "%c" ' ';
      let d = board.(i).(j) in
      print_digit_option ppf d
    done;
    if i mod 3 = 2 then print_horizontal_edge ppf;
  done;
  print_lower_edge ppf;
;;

(**
   123 456 789
   -----------
A  123 123 123
B  456 456 456
C  789 789 789

D  123 123 123
E  123 123 123
F  123 123 123

G  123 123 123
H  123 123 123
I  123 123 123
   -----------
*)

let _Utils_in_of_out_board out_board =
  let in_board = Array.init 9 (fun i -> Array.init 9 (fun j -> [])) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let d_option = out_board.(i).(j) in
      match d_option with
      | None ->
        in_board.(i).(j) <-
          [ One; Two; Three; Four; Five; Six; Seven; Eight; Nine; ]
      | Some d ->
        in_board.(i).(j) <- [d]
    done;
  done;
  in_board
;;

let out_of_in_board in_board =
  let out_board = Array.init 9 (fun i -> Array.init 9 (fun j -> None)) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let d_list = in_board.(i).(j) in
      match d_list with
      | [d] -> out_board.(i).(j) <- Some d
      | _ -> out_board.(i).(j) <- None
    done;
  done;
  out_board
;;

let iter_on_square f (board, i0, j0) =
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      f i j board.(i).(j)
    done
  done
;;

let iter_on_squares f_square board =
  for i = 0 to 2 do
    for j = 0 to 2 do
      f_square (board, (3 * i), (3 * j))
    done
  done
;;

let print_digits ppf l =
  let print_digit_semi ppf d =
    Format.fprintf ppf "%a;@ " print_digit d in
  List.iter (print_digit_semi ppf) l
;;

let print_digit_list ppf = function
  | [d] -> print_digit ppf d
  | l -> Format.fprintf ppf "[%a]" print_digits l
;;

let print_squares ppf board =
  let f ppf i j c =
    Format.fprintf ppf "@[(%i, %i):@ @[%a@]@,@]@," i j print_digit_list c in
  let f_square (board, i0, j0) =
    Format.fprintf ppf "@[<hv 1>{%i %i =@ @[<v>%a@]}@]@," i0 j0
      (fun ppf -> iter_on_square (f ppf)) (board, i0, j0) in
  Format.fprintf ppf "@[<v>%a@]"
   (fun ppf -> iter_on_squares f_square) board
;;

let print_in_board ppf board =
  print_squares ppf board
;;

(** To trace the board. *)
let trace_board board =
  print_out_board Format.std_formatter (out_of_in_board board);
  Format.fprintf Format.std_formatter
    "@.Print the lists of possible digits ?@?";
  let ans = read_line () in
  if ans = "y" then
    print_in_board Format.std_formatter board;
;;

let _Utils_print_result s board = ()
(*
  Format.fprintf Format.std_formatter "@.%s@.%a@."
    s print_out_board (out_of_in_board board)*)
;;

let _Utils_debug s board =
  if !_Utils_debug_flag then begin
    Format.fprintf Format.std_formatter "@.%s@." s;
    trace_board board;
  end
;;



(*** Solve ***)
let rec remove_from_list d = function
  | [] -> []
  | d' :: l ->
    if d = d' then l else d' :: remove_from_list d l
;;

let _Solve_remove_digit d board i j =
  let l = board.(i).(j) in
  let rl = remove_from_list d l in
  if rl = [] then raise Backtrack else
  rl <> l && (board.(i).(j) <- rl; true)
;;

let suppress_digit_in_square d id jd (board, i0, j0) =
(*prerr_endline (Printf.sprintf "suppress_digit: %i %i" i0 j0);*)
  let go_on = ref false in
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      (* Don't ask for this very place: we have not to remove it anyway! *)
      if i = id && j = jd then () else
      go_on := _Solve_remove_digit d board i j || !go_on
    done
  done;
  !go_on
;;

(** {6 Saturation functions for various parts of the board} *)

let sature_square (board, i0, j0) =
(*  Format.printf "sature_square: %i %i@." i0 j0; *)
  let go_on = ref false in
  for i = i0 to i0 + 2 do
    for j = j0 to j0 + 2 do
      let c = board.(i).(j) in
      match c with
      | [d] ->
        go_on := suppress_digit_in_square d i j (board, i0, j0) || !go_on
      | _ -> ()
    done
  done;
  !go_on
;;

let sature_squares board =
(*  Format.printf "sature_squares:@." i0 j0; *)
  let go_on = ref false in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let square = (board, (3 * i), (3 * j)) in
      go_on := sature_square square || !go_on
    done;
  done;
  !go_on
;;

let suppress_on_column d jd (board, i0) =
  let go_on = ref false in
  for j = 0 to 8 do
    if j = jd then () else
    go_on := _Solve_remove_digit d board i0 j || !go_on
  done;
  !go_on
;;

let suppress_on_row d id (board, j0) =
  let go_on = ref false in
  for i = 0 to 8 do
    if i = id then () else
    go_on := _Solve_remove_digit d board i j0 || !go_on
  done;
  !go_on
;;

let rec sature_board board =
  let go_on = ref false in
  for i = 0 to 8 do
    for j = 0 to 8 do
(*prerr_endline (Printf.sprintf "sature_board %i %i" i j);*)
      match board.(i).(j) with
      | [d] ->
(*prerr_endline (Printf.sprintf "suppress_on_line %i" i);*)
        go_on := suppress_on_column d j (board, i) || !go_on;
(*prerr_endline (Printf.sprintf "suppress_on_row %i" j);*)
        go_on := suppress_on_row d i (board, j) || !go_on
      | _ -> ()
    done;
  done;
  go_on := sature_squares board || !go_on;
  if !go_on then sature_board board
;;

let deduce board with_lists =
  let rec except (i0, j0) = function
    | [] -> []
    | (l, i, j as list) :: lists ->
      if i = i0 && j = j0 then lists else
      list :: except (i0, j0) lists in

  let digit_not_in_other_lists i j d =
    List.for_all
      (fun (l, _, _) -> not (List.mem d l))
      (except (i, j) with_lists) &&
    (board.(i).(j) <- [d];
     true) in

  (** Find if we can reduce a list of possible digits at place i, j. *)
  let reducible (l, i, j) =
    List.exists (fun d -> digit_not_in_other_lists i j d) l in

  List.exists reducible with_lists
;;

let deduce_square board i0 j0 =
  let with_lists =
    let rec loop accu i j =
      if j > j0 + 2 then loop accu (i + 1) j0 else
      if i > i0 + 2 then accu else
      match board.(i).(j) with
      | [d] -> loop accu i (j + 1)
      | l -> loop ((l, i, j) :: accu) i (j + 1) in
    loop [] i0 j0 in

  deduce board with_lists
;;

let deduce_row board i0 =
  let with_lists =
    let rec loop accu j =
      if j > 8 then accu else
      match board.(i0).(j) with
      | [d] -> loop accu (j + 1)
      | l -> loop ((l, i0, j) :: accu) (j + 1) in
    loop [] 0 in

  deduce board with_lists
;;

let deduce_column board j0 =
  let with_lists =
    let rec loop accu i =
      if i > 8 then accu else
      match board.(i).(j0) with
      | [d] -> loop accu (i + 1)
      | l -> loop ((l, i, j0) :: accu) (i + 1) in
    loop [] 0 in

  deduce board with_lists
;;

(** {6 Saturation of the entire board} *)

let all_squares = [
  (0, 0); (0, 3); (0, 6);
  (3, 0); (3, 3); (3, 6);
  (6, 0); (6, 3); (6, 6);
]
;;

let all_rows = [
  0; 1; 2; 3; 4; 5; 6; 7; 8;
]
;;

let all_columns = all_rows
;;

let deduce_squares board =
  List.exists (fun (i, j) -> deduce_square board i j) all_squares
;;

let deduce_rows board =
  List.exists (fun i -> deduce_row board i) all_rows
;;

let deduce_columns board =
  List.exists (fun j -> deduce_column board j) all_columns
;;

let rec _Solve_deduce_board board =
  sature_board board;
  (* For each square *)
  let found_something = deduce_squares board in
  if found_something then _Solve_deduce_board board else
  (* For each row *)
  let found_something = deduce_rows board in
  if found_something then _Solve_deduce_board board else
  (* For each column *)
  let found_something = deduce_columns board in
  if found_something then _Solve_deduce_board board
;;



(*** Resolve ***)
let worst_choice = _Utils_digits
;;

let choose_place board =
  (* Find the first non assigned place with smallest number of choices and
     return it. *)
  let rec loop (lenb, best, ib, jb as accu) i j =
    if j > 8 then loop accu (i + 1) 0 else
    if i > 8 then (best, ib, jb) else
    match board.(i).(j) with
    | [d] -> loop accu i (j + 1)
    | [d1; d2] as l -> (l, i, j)
    | l ->
      let lenl = List.length l in
      if lenl < lenb
      then loop (lenl, l, i, j) i (j + 1)
      else loop accu i (j + 1) in

  let (best, ib, jb) =
    loop (List.length worst_choice, worst_choice, 0, 0) 0 0 in
  if List.length best = 9 then raise (Solution) else
  (List.hd best, ib, jb)
;;

let copy_board board =
  let v =
  Array.init (Array.length board)
    (fun i -> Array.init (Array.length board.(i))
      (fun j -> board.(i).(j))) in
  assert (v = board);
  v
;;



let rec resolve nr boards board =
  _Utils_debug (Printf.sprintf "Resolve %i 0 (before deduce)." nr) board;
  _Solve_deduce_board board;
  _Utils_debug (Printf.sprintf "Resolve %i 1 (after deduce)." nr) board;
  (* Choose one possible assignment of a digit to a still unknown place. *)
  let (d, i, j) = choose_place board in
  (* Copy the board to the list of backtrack points
     (omitting the digit from the list of the corresponding place). *)
  _Utils_debug
    (Printf.sprintf "Resolve %i 2 (after choosing %i, %i, %i)."
       nr (_Utils_int_of_digit d) i j)
    board;
  let board_copy = copy_board board in
  let b = _Solve_remove_digit d board_copy i j in
  _Utils_debug
    (Printf.sprintf "Resolve %i 3 (after choosing %i)."
       nr (_Utils_int_of_digit d))
    board_copy;
  if not b then assert false;

  (* Assign it to the current [board] and go on. *)
  board.(i).(j) <- [d];
  _Utils_debug
    (Printf.sprintf "Resolve %i 4 (after saturation)." nr)
    board_copy;
  try resolve (nr + 1) (board_copy :: boards) board with
  | Backtrack -> resolve (nr + 1) boards board_copy
;;

let _Resolve_resolve_board board =
 try resolve 0 [] board with
 | Backtrack -> failwith "Cannot solve this sudoku!"
 | Solution -> ()
;;



(*** Main ***)
_Utils_debug_flag := false;;

let read_out_board () = [|
  [| None; None; None;
     None; Some Four; Some Seven;
     None; Some Eight; None; |];
  [| None; Some Six; None;
     None; None; None;
     None; None; None; |];
  [| None; None; Some Two;
     Some Eight; None; None;
     Some One; None; Some Nine; |];
  [| None; None; None;
     None; None; None;
     None; None; None; |];
  [| Some One; None; None;
     Some Six; None; None;
     Some Five; None; None; |];
  [| None; Some Three; None;
     None; None; Some Four;
     None; Some Nine; None; |];
  [| Some Eight; Some Seven; Some Nine;
     None; None; None;
     None; Some Three; None; |];
  [| None; None; None;
     None; Some Three; None;
     Some Four; None; None; |];
  [| None; None; Some Four;
     None; Some Seven; None;
     None; None; None; |];
|]
;;

let out_board = read_out_board ()
;;

let expert_board =
 [|
  [| None; None; None;
     None; Some Four; Some Seven;
     None; Some Eight; None; |];
  [| None; Some Six; None;
     None; None; None;
     None; None; None; |];
  [| None; None; Some Two;
     Some Eight; None; None;
     Some One; None; Some Nine; |];
  [| None; None; None;
     None; None; None;
     None; None; None; |];
  [| Some One; None; None;
     Some Six; None; None;
     Some Five; None; None; |];
  [| None; Some Three; None;
     None; None; Some Four;
     None; Some Nine; None; |];
  [| Some Eight; Some Seven; Some Nine;
     None; None; None;
     None; Some Three; None; |];
  [| None; None; None;
     None; Some Three; None;
     Some Four; None; None; |];
  [| None; None; Some Four;
     None; Some Seven; None;
     None; None; None; |];
|]
;;

let easy_board =
 [|
  [| Some One; None; None;
     None; Some Two; Some Nine;
     None; None; None; |];
  [| None; Some Seven; Some Eight;
     None; None; None;
     None; None; None; |];
  [| None; None; None;
     None; None; None;
     Some Six; Some Four; Some Three; |];
  [| None; Some Five; None;
     None; None; None;
     None; None; None; |];
  [| Some Four; None; None;
     Some Eight; Some One; None;
     None; None; None; |];
  [| Some Three; Some One; None;
     None; None; Some Two;
     None; Some Five; Some Seven; |];
  [| None; Some Four; Some Nine;
     None; None; None;
     None; None; None; |];
  [| None; None; None;
     None; Some Seven; None;
     Some One; None; None; |];
  [| Some Two; None; None;
     None; Some Nine; None;
     Some Four; None; None; |];
|]
;;

let read_board = function
  | 0 ->
    (* Expert *)
    expert_board
  | 1 ->
    (* Facile *)
    easy_board
  | 528 | 2 ->
    (* Très facile *)
    _Read_board_of_string  "
            -------------
            |1  | 29|   |
            | 78|   |   |
            |   |   |643|
            -------------
            | 5 |   |   |
            |4  |81 |   |
            |31 |  2| 57|
            -------------
            | 49|   |   |
            |   | 7 |1  |
            |2  | 9 |4  |
            ------------- "
  | 529 | 3 ->
    (* Très difficile *)
    _Read_board_of_string  "
            -------------
            |2  | 87|   |
            | 5 |   |  7|
            |  8|9  |21 |
            -------------
            |   |   |   |
            |1  |4  |3 9|
            |94 |  2|  1|
            -------------
            |835|   |   |
            |   | 5 |6 2|
            |  6| 41|   |
            ------------- "
  | 530 | 4 ->
    (* Virtuose *)
    _Read_board_of_string  "
            -------------
            |   | 1 |  5|
            |   |4  |7 2|
            |   | 8 |6  |
            -------------
            |  8|   |4  |
            | 6 |  5|   |
            |1  |9  | 3 |
            -------------
            |34 | 6 | 1 |
            |   |2  |9  |
            | 5 |  7|   |
            ------------- "
  | _ -> failwith "read_board: unknown sudoku problem."
;;

let out_board = read_board 530;;

let in_board = _Utils_in_of_out_board out_board;;

(************************** Comment out, if you want to follow the resolution.

Utils.debug "@.***** Before saturation ****@." in_board
;;

Solve.sature_board in_board
;;

Utils.debug "***** After saturation ****" in_board
;;

Solve.deduce_board in_board
;;

Utils.deduce_board "***** After deduction ****" in_board
;;
****************************)

let result_in_board = _Resolve_resolve_board in_board;;

(****************************
let result_out_board = Utils.out_of_in_board result_in_board;;

Utils.debug "***** After resolution ****" result_out_board
;;
****************************)

_Utils_print_result "***** Final result ****" result_in_board
;;
