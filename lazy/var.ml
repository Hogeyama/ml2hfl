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

let header = "var"
let separator = "-"

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
		  let _::s::ss = String.nsplit s separator in
		  f (V(s)) ss
  else
    V(s)

let equiv x y = x = y
