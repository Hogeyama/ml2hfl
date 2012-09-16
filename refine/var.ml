open ExtList
open ExtString

(** Variables *)

type t = V of Idnt.t | C of Idnt.t | T of t * int * int

(** {6 Printers} *)

let rec pr ppf x =
  match x with
    V(id) ->
      Format.fprintf ppf "%a" Idnt.pr id
  | C(id) ->
      Format.fprintf ppf "%a" Idnt.pr id
  | T(x, uid, arg) ->
      Format.fprintf ppf "<%a@@%d:%d>" pr x uid arg

let pr_list ppf xs =
  Format.fprintf ppf "%a" (Util.pr_list pr ",") xs

(** {6 Constructors} *)

let make id = V(id)
let make_coeff id = C(id)

let new_var () = V(Idnt.new_id ())

(** {6 Functions on variables} *)

let string_of x =
  match x with
    V(id) | C(id) ->
      Idnt.string_of id
  | T(_, _, _) -> assert false

let equiv x y = x = y

let rec cong x1 x2 =
  match x1, x2 with
    V(id1), V(id2) ->
      id1 = id2
  | T(x1, _, arg1), T(x2, _, arg2) ->
      arg1 = arg2 && cong x1 x2
  | _ -> false

let rec lt x1 x2 =
  match x1, x2 with
    V(id1), V(id2) ->
      id1 = id2
  | T(x1, uid1, arg1), T(x2, uid2, arg2) ->
      arg1 = arg2 && cong x1 x2 && uid1 < uid2
  | _ -> assert false

let rec rename_base f x =
  match x with
    V(id) ->
      V(f id)
  | C(id) ->
      C(f id)
  | T(x, uid, arg) ->
      T(rename_base f x, uid, arg)

let rec base x =
  match x with
    V(id) | C(id) ->
      id
  | T(x, _, _) ->
      base x

let is_top x =
  match x with
    V(_) -> true
  | C(_) -> assert false
  | T(_, _, _) -> false

let rec is_pos x =
  match x with
    V(_) -> true
  | C(_) -> assert false
  | T(x', _, _) -> is_neg x'
and is_neg x =
  match x with
    V(_) -> false
  | C(_) -> assert false
  | T(x', _, _) -> is_pos x'

let is_coeff x =
  match x with
    C(_) -> true
  | V(_) | T(_, _, _) -> false


(** {6 Encoding and decoding variables as strings} *)

let vheader = "v"
let cheader = "c"
let separator = "_sep_" (*???*)

let rec print x =
  match x with
    V(id) ->
      (try
        let _ = String.find (Idnt.string_of id) separator in
        assert false
      with Invalid_string ->
        vheader ^ separator ^ Idnt.string_of id)
  | C(id) ->
      (try
        let _ = String.find (Idnt.string_of id) separator in
        assert false
      with Invalid_string ->
        cheader ^ separator ^ Idnt.string_of id)
  | T(x, uid, arg) ->
      print x ^ separator ^ String.of_int uid ^ separator ^ String.of_int arg

let parse s =
  let rec f x ss =
    match ss with
      [] -> x
    | s1::s2::ss ->
        (try f (T(x, int_of_string s1, int_of_string s2)) ss with _ -> Format.printf "%s,%s@," s1 s2; assert false)
    | _ -> assert false
  in
  if String.starts_with s vheader then
    match String.nsplit s separator with
      _::s::ss -> f (V(Idnt.make s)) ss
    | _ -> assert false
  else if String.starts_with s cheader then
    match String.nsplit s separator with
      _::s::ss -> f (C(Idnt.make s)) ss
    | _ -> assert false
  else
    assert false(*V(Idnt.make s)*)
