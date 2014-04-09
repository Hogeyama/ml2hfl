open Util

type color =
    Blue
  | Red
  | Green
  | Cyan

let color_table =
  [Blue, 34;
   Red, 31;
   Green, 32;
   Cyan, 36]

let rec zeros = 0::zeros
let history = ref zeros

let init () =
  if Unix.isatty Unix.stdout
  then ()
  else Flag.color := false

let print ppf s =
  if !Flag.color || !Flag.color_always
  then Format.fprintf ppf "\027[%dm" s

let set ppf c =
  try
    let c' = List.assoc c color_table in
    history := c'::!history;
    print ppf c'
  with Not_found -> raise (Invalid_argument "Color.set")
let reset ppf =
  history := List.tl !history;
  print ppf @@ List.hd !history

let fprintf ppf c =
  set ppf c;
  Format.kfprintf (fun _ -> reset ppf) ppf
let printf c = fprintf Format.std_formatter c

let blue ppf s = fprintf ppf Blue "%s" s
let red ppf s = fprintf ppf Red "%s" s
let green ppf s = fprintf ppf Green "%s" s
let cyan ppf s = fprintf ppf Cyan "%s" s
