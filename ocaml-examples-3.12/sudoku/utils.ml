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

(* $Id: utils.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

(** {3 Utilities to print and debug} *)

open Types;;
open Format;;

let debug_flag = ref false;;

let int_of_digit = function
  | Types.One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
;;

let print_digit ppf d = fprintf ppf "%i" (int_of_digit d);;

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

let digits = [ One; Two; Three; Four; Five; Six; Seven; Eight; Nine; ];;

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

let in_of_out_board out_board =
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

let print_result s board =
  Format.fprintf Format.std_formatter "@.%s@.%a@."
    s print_out_board (out_of_in_board board)
;;

let debug s board =
  if !debug_flag then begin
    Format.fprintf Format.std_formatter "@.%s@." s;
    trace_board board;
  end
;;
