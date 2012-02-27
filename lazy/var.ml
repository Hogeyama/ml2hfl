open ExtList
open ExtString

(** Variables *)

type t = V of Idnt.t | C of Idnt.t | T of t * int * int

let rec pr ppf x =
  match x with
    V(id)
  | C(id) ->
      Format.fprintf ppf "%a" Idnt.pr id
  | T(x, uid, arg) ->
      Format.fprintf ppf "<%a@@%d:%d>" pr x uid arg

let rec pr_x_uid ppf (x, uid) = Format.fprintf ppf "<%a@@%d>" pr x uid

let make id = V(id)
let make_coeff id = C(id)

let new_var () = V(Idnt.new_id ())

let vheader = "v"
let cheader = "c"
let separator = "_sep_" (*???*)

let rec string_of x =
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
		    string_of x ^ separator ^ String.of_int uid ^ separator ^ String.of_int arg

let string_of2 x =
  match x with
    V(id) | C(id) ->
      Idnt.string_of id
  | T(_, _, _) -> assert false

let parse s =
  let rec f x ss =
    match ss with
      [] -> x
    | s1::s2::ss ->
        (try f (T(x, int_of_string s1, int_of_string s2)) ss with _ -> Format.printf "%s,%s@." s1 s2; assert false)
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

let equiv x y = x = y

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

(** @deprecated *)
let is_avail x =
  match x with
    V(_) -> true
  | C(_) -> assert false
  | T(_, _, i) -> i <> -1

let is_coeff x =
  match x with
    C(_) -> true
  | V(_) | T(_, _, _) -> false

(** @return the call id of top level function call *)
let rec tlfc_of x =
  match x with
    V(_) -> raise Not_found
  | C(_) -> assert false
  | T(x', uid, _) -> try tlfc_of x' with Not_found -> x', uid

(** @return the call id of parent function call *)
let rec fc_ref_of x =
  match x with
    V(_) -> assert false
  | C(_) -> assert false
  | T(x', uid, _) -> x', uid

let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    V(_) -> false
  | C(_) -> assert false
  | T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

