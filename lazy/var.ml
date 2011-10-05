open ExtList
open ExtString

type t = V of Idnt.t | T of t * int * int

let rec pr ppf x =
  match x with
    V(id) ->
      Format.fprintf ppf "%a" Idnt.pr id
  | T(x, uid, arg) ->
      Format.fprintf ppf "<%a%s%s>" pr x
        (":" ^ String.of_int uid)
        (":" ^ String.of_int arg)

let make id = V(id)

let new_var () = V(Idnt.new_id ())

let header = "a"
let separator = "_" (*???*)(*"-"*)

let string_of x =
  let rec f x =
    match x with
      V(id) ->
        (try
          let _ = String.find id separator in
          assert false
        with Invalid_string -> id)
    | T(x, uid, arg) ->
        f x ^ separator ^ String.of_int uid ^ separator ^ String.of_int arg
  in
  header ^ separator ^ f x

let parse s =
  let rec f x ss =
    match ss with
      [] -> x
    | s1::s2::ss ->
        f (T(x, int_of_string s1, int_of_string s2)) ss
    | _ -> assert false
  in
  if String.starts_with s header then
    match String.nsplit s separator with
      _::s::ss -> f (V(s)) ss
    | _ -> assert false
  else
    V(s)

let equiv x y = x = y

let is_top x =
  match x with
    V(_) -> true
  | T(_, _, _) -> false

let rec is_pos x =
  match x with
    V(_) -> true
  | T(x', _, _) -> is_neg x'
and is_neg x =
  match x with
    V(_) -> false
  | T(x', _, _) -> is_pos x'

let rec fc_of x =
  match x with
    V(_) -> raise Not_found
  | T(x', uid, _) -> try fc_of x' with Not_found -> x', uid
