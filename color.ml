
type color =
    Default
  | Bright
  | Dim
  | Underscore
  | Blink
  | Reverse
  | Hidden
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | BG_Black
  | BG_Red
  | BG_Green
  | BG_Yellow
  | BG_Blue
  | BG_Magenta
  | BG_Cyan
  | BG_White

let color_table =
  [Default, 0;
   Bright, 1;
   Dim, 2;
   Underscore, 4;
   Blink, 5;
   Reverse, 7;
   Hidden, 8;
   Black, 30;
   Red, 31;
   Green, 32;
   Yellow, 33;
   Blue, 34;
   Magenta, 35;
   Cyan, 36;
   White, 37;
   BG_Black, 40;
   BG_Red, 41;
   BG_Green, 42;
   BG_Yellow, 43;
   BG_Blue, 44;
   BG_Magenta, 45;
   BG_Cyan, 46;
   BG_White, 47]

let rec init_colors = (List.assoc Default color_table)::init_colors
let history = ref init_colors

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
