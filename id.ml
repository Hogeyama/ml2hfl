open Util

type 'a t = {id:int; name:string; typ:'a}

let init_counter = 0
let counter = ref init_counter
let tmp_counter = ref init_counter

let new_int () = incr counter; !counter
let get_counter () = !counter
let set_counter n = counter := n
let save_counter () = tmp_counter := !counter
let reset_counter () = counter := !tmp_counter
let clear_counter () = counter := init_counter

let make id name typ = {id; name; typ}
let new_var ?(name="x") typ = make (new_int()) name typ
let new_var_id x = {x with id=new_int()}

let id x = x.id
let name x = x.name
let typ x = x.typ

let to_string x =
  let s =
    let n = id x in
    if n <= 0 then
      name x
    else
      name x ^ "_" ^ string_of_int n
  in
  if s<>"" && s.[0] = '@' then
    "$" ^ String.sub s 1 (String.length s - 1) ^ "$"
  else
    s

let from_string s typ =
  try
    let s1,s2 = String.rsplit s "_" in
    {id=int_of_string s2; name=s1; typ=typ}
  with _ -> {id=0; name=s; typ=typ}

let compare x y = compare (to_string x) (to_string y)
let same x y = compare x y = 0
let eq = same

let set_name x name = {x with name=name}
let set_typ x typ = {x with typ=typ}

let add_name_before x str = set_name x (str ^ name x)
let add_name_after x str = set_name x (name x ^ str)

let rec mem x = function
  | [] -> false
  | a::l -> same a x || mem x l

let rec assoc x = function
  | [] -> raise Not_found
  | (a,b)::l -> if same x a then b else assoc x l

let rec mem_assoc x = function
  | [] -> false
  | (a, b) :: l -> same a x || mem_assoc x l

let print fm x =
  let s =
    if !Flag.web then
      name x
    else
      to_string x
  in
  assert (s <> "");
  Format.pp_print_string fm s

let map_typ f {id; name; typ} =
  {id; name; typ=f typ}
