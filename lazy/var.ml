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

let rec string_of x =
  match x with
    V(id) -> id
  | T(x, uid, arg) ->
      string_of x ^ "_" ^ String.of_int uid ^ "_" ^ String.of_int arg

let parse s =
  let rec f x ss =
    match ss with
      [] -> x
    | s1::s2::ss ->
        f (T(x, int_of_string s1, int_of_string s2)) ss
    | _ -> assert false
  in
  let s::ss = String.nsplit s "_" in
  f (V(s)) ss

let equiv x y = x = y
