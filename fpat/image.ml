open Util
open Combinator
open Color

(** Bitmap images *)

let rec ignore_byte ic n =
  if n > 0 then
    let _ = input_byte ic in
    ignore_byte ic (n - 1)

(* little endian *)
let input2 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  (b2 lsl 8) lor b1

(* little endian *)
let input3 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  (b3 lsl 16) lor (b2 lsl 8) lor b1

(* little endian *)
let input4 ic =
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  let b4 = input_byte ic in
  (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

(* little endian *)
let output2 oc n =
  output_byte oc (n land 0xFF);
  output_byte oc ((n lsr 8) land 0xFF)

(* little endian *)
let output4 oc n =
  output_byte oc (n land 0xFF);
  output_byte oc ((n lsr 8) land 0xFF);
  output_byte oc ((n lsr 16) land 0xFF);
  output_byte oc ((n lsr 24) land 0xFF)


type t = { pixels: RGB888.t array; width: int; height: int }
let make width height = { pixels = Array.make (width * height) RGB888.black; width = width; height = height }
let get x y img = img.pixels.(img.width * y + x)
let set x y c img = img.pixels.(img.width * y + x) <- c

let caml_image img =
  let init_matrix dimx dimy f =
    let v = f 0 0 in
    let ar = Array.make dimx (Array.make 0 v) in
    for x = 0 to dimx - 1 do
      ar.(x) <- Array.make dimy v;
      for y = 0 to dimy - 1 do
         ar.(x).(y) <- f x y
      done
    done;
    ar in
  Graphics.make_image (init_matrix img.height img.width (fun y x -> get x y img))

let load filename =
  let ic = open_in_bin filename in
  let h1 = input_byte ic in
  let h2 = input_byte ic in
  assert ((h1 = int_of_char 'B') && (h2 = int_of_char 'M'));
  ignore_byte ic 12;
  (* BITMAPINFO/BITMAPCOREINFO *)
  (* BITMAPINFOHEADER/BITMAPCOREHEADER *)
  let width = ref 0 in (*long*)
  let height = ref 0 in (*long*)
  let planes = ref 0 in (*short*)
  let bitcount = ref 0 in (*short*)
  let colors = Array.make 256 RGB888.black in
  (* Windows形式かOS/2形式化の判別*)
  let size = input4 ic in
 (if size = 40 then
   ((* Windows形式 *)
    width := input4 ic;
    height := input4 ic;
    planes := input2 ic;
    bitcount := input2 ic;
    ignore_byte ic 24;
   (if !bitcount = 1 || !bitcount = 4 || !bitcount = 8 then
      (* カラーパレットの読み込み *)
      for i = 0 to (1 lsl !bitcount) - 1 do
        colors.(i) <- input4 ic
      done))
  else if size = 12 then
   ((* OS/2形式 *)
    width := input2 ic;
    height := input2 ic;
    planes := input2 ic;
    bitcount := input2 ic;
   (if !bitcount = 1 || !bitcount = 4 || !bitcount = 8 then
      (* カラーパレットの読み込み *)
      for i = 0 to (1 lsl !bitcount) - 1 do
        colors.(i) <- input3 ic
      done))
  else
    failwith "load");
  (* Data *)
 (if !planes <> 1 then
    failwith ""
  else
    let img = make !width !height in
    let wow = 8 / !bitcount - 1 in
    let value = ref 0 in
   (for y = !height - 1 downto 0 do
     (for x = 0 to !width - 1 do
       (match !bitcount with
          1 | 4 | 8 ->
           (if (x land wow) = 0 then
              value := (input_byte ic) land 0xFF);
            let mask = 0xFF lsr (8 - !bitcount) in
            let idx = (!value lsr ((wow - (x land wow)) * !bitcount)) land mask in
            set x y colors.(idx) img
        | 24 ->
          set x y (input3 ic) img
        | _ ->
          failwith "")
      done);
      ignore_byte ic (((32 - ((!width * !bitcount) land 31)) land 31) lsr 3)
    done);
    close_in ic;
    Printf.printf "loaded file:%s width:%d height:%d bpp:%d\n" filename !width !height !bitcount;
    img)

let save img filename =
  let oc = open_out_bin filename in
  (* BitmapFileHeader *)
  output_byte oc (int_of_char 'B');
  output_byte oc (int_of_char 'M');
  output4 oc (54 + (img.width * 3 + (img.width land 3)) * img.height);
  output4 oc 0;
  output4 oc 54;
  (* BitmapInfoHeader*)
  output4 oc 40;
  output4 oc img.width;
  output4 oc img.height;
  output2 oc 1;
  output2 oc 24;
  output4 oc 0;
  output4 oc 0;
  output4 oc 0;
  output4 oc 0;
  output4 oc 0;
  output4 oc 0;
  (* Data *)
  for y = img.height - 1 downto 0 do
    for x = 0 to img.width - 1 do
      output_byte oc (RGB888.get_blue (get x y img));
      output_byte oc (RGB888.get_green (get x y img));
      output_byte oc (RGB888.get_red (get x y img));
    done;
    for i = 0 to (img.width land 3) - 1 do
      output_byte oc 0
    done
  done;
  close_out oc;
  Printf.printf "saved file:%s width:%d height:%d bpp:%d\n" filename img.width img.height 24;
