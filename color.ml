open Util

let color =
  ["Blue", 34;
   "Red", 31;
   "Green", 32;
   "Cyan", 36]

let rec zeros = 0::zeros
let history = ref zeros

let init () =
  if Unix.isatty Unix.stdout
  then ()
  else Flag.color := false

let print fm s =
  if !Flag.color || !Flag.color_always
  then Format.fprintf fm "\027[%dm" s

let set fm c =
  try
    let c' = List.assoc c color in
    history := c'::!history;
    print fm c'
  with Not_found -> raise (Invalid_argument "Color.set")
let reset fm =
  history := List.tl !history;
  print fm @@ List.hd !history

let wrap c pr =
  set Format.std_formatter c;
  pr ();
  reset Format.std_formatter
