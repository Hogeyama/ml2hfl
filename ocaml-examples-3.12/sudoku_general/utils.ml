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

let get_debug, set_debug =
 let debug_flag = ref false in
  (fun () -> !debug_flag),
  (fun () -> debug_flag := true)
;;

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
  | Ten -> 10
  | Eleven -> 11
  | Twelve -> 12
  | Thirteen -> 13
  | Fourteen -> 14
  | Fifteen -> 15
;;

let print_digit ppf d = fprintf ppf "%i" (int_of_digit d);;

let print_digit_option ppf = function
  | None -> fprintf ppf "%s" "*"
  | Some d -> print_digit ppf d
;;

let full_upper_edge, full_lower_edge =
  " 123 456 789 abc def",
  " -------------------"
;;

let print_upper_edge width ppf =
  let width = width + (width / 3) in
  fprintf ppf "%s@.%s"
    (String.sub full_upper_edge 0 width)
    (String.sub full_lower_edge 0 width)
;;

let print_lower_edge width ppf =
  let width = width + (width / 3) in
  fprintf ppf "%s"
    (String.sub full_lower_edge 0 width)
;;

let print_horizontal_edge ppf =
  fprintf ppf "@."
;;

let all_digits = [
  One; Two; Three; Four; Five; Six; Seven; Eight; Nine;
  Ten; Eleven; Twelve; Thirteen; Fourteen; Fifteen;
]
;;

let digits n =
  if n <= 1 then
    failwith (Printf.sprintf "digits: too few digits (%i) asked for" n) else
  if n >= List.length all_digits then
    failwith (Printf.sprintf "digits: too many digits (%i) asked for" n) else
  let rec loop accu digits = function
    | 0 -> accu
    | n ->
      match digits with
      | d :: digits -> loop (d :: accu) digits (n - 1)
      | _ -> assert false in
  List.rev (loop [] all_digits n)
;;

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
let in_of_out_board out_board =
  let height = out_board.out_height in
  let width = out_board.out_width in

  let all_digits = digits width in

  let iboard = Array.init height (fun i -> Array.init width (fun j -> [])) in
  let oboard = out_board.out_board in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let d_option = oboard.(i).(j) in
      match d_option with
      | None ->
        iboard.(i).(j) <- all_digits
      | Some d ->
        iboard.(i).(j) <- [d]
    done;
  done;
  { in_h = out_board.out_h;
    in_w = out_board.out_w;
    in_height = height;
    in_width = width;
    in_board = iboard; }
;;

let out_of_in_board in_board =
  let height = in_board.in_height in
  let width = in_board.in_width in

  let iboard = in_board.in_board in
  let oboard = Array.init height (fun i -> Array.init width (fun j -> None)) in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let d_list = iboard.(i).(j) in
      match d_list with
      | [d] -> oboard.(i).(j) <- Some d
      | _ -> oboard.(i).(j) <- None
    done;
  done;
  { out_h = in_board.in_h;
    out_w = in_board.in_w;
    out_height = height;
    out_width = width;
    out_board = oboard; }
;;

let iter_on_rectangle f (board, i0, j0) =
  let h = board.in_h in
  let w = board.in_w in
  let board = board.in_board in
  for i = i0 to i0 + (h - 1) do
    for j = j0 to j0 + (w - 1) do
     f i j board.(i).(j)
    done
  done
;;

let iter_on_rectangles f_rectangle board =
  let h = board.in_h in
  let w = board.in_w in
  for i = 0 to h - 1 do
    for j = 0 to w - 1 do
      f_rectangle (board, (h * i), (w * j))
    done
  done
;;

let print_out_board ppf board =
  let height = board.out_height in
  let width = board.out_width in
  let h = board.out_h in
  let w = board.out_w in
  let oboard = board.out_board in
  print_upper_edge width ppf;
  for i = 0 to height - 1 do
    if i mod h = 0 then print_horizontal_edge ppf;
(*    fprintf ppf "%a " print_letter letters.(i);*)
    for j = 0 to width - 1 do
      if j mod w = 0 then fprintf ppf "%c" ' ';
      let d = oboard.(i).(j) in
      print_digit_option ppf d
    done;
    fprintf ppf "@.";
  done;
  print_lower_edge width ppf;
  fprintf ppf "@.";
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

let print_digits ppf l =
  let print_digit_semi ppf d =
    Format.fprintf ppf "%a;@ " print_digit d in
  List.iter (print_digit_semi ppf) l
;;

let print_digit_list ppf = function
  | [d] -> print_digit ppf d
  | l -> Format.fprintf ppf "[%a]" print_digits l
;;

let print_rectangles ppf board =
  let f ppf i j c =
    Format.fprintf ppf "@[(%i, %i):@ @[%a@]@,@]@," i j print_digit_list c in
  let f_rectangle (board, i0, j0) =
    Format.fprintf ppf "@[<hv 1>{%i %i =@ @[<v>%a@]}@]@," i0 j0
      (fun ppf -> iter_on_rectangle (f ppf)) (board, i0, j0) in
  Format.fprintf ppf "@[<v>%a@]"
   (fun ppf -> iter_on_rectangles f_rectangle) board
;;

let print_in_board ppf board =
  print_rectangles ppf board
;;

let iter_i i0 f l =
  let rec loop i = function
  | [] -> ()
  | [x] -> f i x
  | x :: xs -> f i x; loop (i + 1) xs in
  loop i0 l
;;

(** To trace the board. *)
let trace_board board =
  print_out_board Format.err_formatter (out_of_in_board board);
  Format.fprintf Format.err_formatter
    "@.Print the lists of possible digits ?@?";
  let ans = read_line () in
  if ans = "y" then
    print_in_board Format.err_formatter board;
;;

let print_result board =
  print_out_board Format.std_formatter (out_of_in_board board)
;;

let debug s board =
  if get_debug () then begin
    Format.fprintf Format.err_formatter "@.%s@." s;
    trace_board board;
  end
;;

let debug_message s =
  if get_debug () then begin
    Format.fprintf Format.err_formatter "@.%s@." s;
  end
;;
