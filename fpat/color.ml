open Util
open Combinator

(** Colors *)

module RGB888 =
  struct
    type t = int

    let make r g b = ((r land 0xFF) lsl 16) lor ((g land 0xFF) lsl 8) lor (b land 0xFF)

    let get_red c = (c lsr 16) land 0xFF
    let get_green c = (c lsr 8) land 0xFF
    let get_blue c = c land 0xFF

    let black = make 0 0 0
    let white = make 0xFF 0xFF 0xFF
    let red = make 0xFF 0 0
    let green = make 0 0xFF 0
    let blue  = make 0 0 0xFF
    let yellow = make 0xFF 0xFF 0
    let cyan = make 0 0xFF 0xFF
    let magenta = make 0xFF 0 0xFF
  end

module RGBFFF =
  struct
    type t = { red: float; green: float; blue: float }

    let make r g b = { red = r; green = g; blue = b }

    let scale s c = make (s *. c.red) (s *. c.green) (s *. c.blue)

    let add c1 c2 = make (c1.red +. c2.red) (c1.green +. c2.green) (c1.blue +. c2.blue)

    let sub c1 c2 = make (c1.red -. c2.red) (c1.green -. c2.green) (c1.blue -. c2.blue)

    let mul c1 c2 = make (c1.red *. c2.red) (c1.green *. c2.green) (c1.blue *. c2.blue)

    let white = make 1.0 1.0 1.0
    let black = make 0.0 0.0 0.0
    let red = make 1.0 0.0 0.0
    let orange = make 1.0 0.78125 0.0
    let yellow = make 1.0 1.0 0.0
    let green = make 0.0 1.0 0.0
    let cyan = make 0.0 1.0 1.0
    let blue  = make 0.0 0.0 1.0
    let magenta = make 1.0 0.0 1.0
    let gray = make 0.5 0.5 0.5
    let dark_gray = make 0.25 0.25 0.25
    let light_gray = make 0.75 0.75 0.75
    let pink = make 1.0 0.68359375 0.68359375

    let rgb888 c =
      let r = min 255 (max 0 (int_of_float (256.0 *. c.red))) in
      let g = min 255 (max 0 (int_of_float (256.0 *. c.green))) in
      let b = min 255 (max 0 (int_of_float (256.0 *. c.blue))) in
      RGB888.make r g b

    let luminance c = (c.red +. c.green +. c.blue) /. 3.0
  end
