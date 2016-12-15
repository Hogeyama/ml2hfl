open Util

type 'a t = {id:int; name:string; typ:'a; attr:attr list}

and attr =
  | External
  | Coefficient

let init_counter = 0
let counter = ref init_counter
let tmp_counter = ref init_counter

let new_int () = incr counter; !counter
let get_counter () = !counter
let set_counter n = counter := n
let save_counter () = tmp_counter := !counter
let reset_counter () = counter := !tmp_counter
let clear_counter () = counter := init_counter

let make id name attr typ = {id; name; attr; typ}
let new_var ?(name="x") ?(attr=[]) typ = make (new_int()) name attr typ
let new_var_id x = {x with id=new_int()}

let id x = x.id
let name x = x.name
let typ x = x.typ
let attr x = x.attr

let to_string x =
  let s =
    let n = id x in
    if n <= 0 then
      name x
    else
      name x ^ "_" ^ string_of_int n
  in
  if s <> "" && s.[0] = '@' then
    "$" ^ String.sub s 1 (String.length s - 1) ^ "$"
  else
    s

let from_string name typ =
  try
    let s1,s2 = String.rsplit name "_" in
    {id=int_of_string s2; name=s1; typ=typ; attr=[]}
  with _ -> {id=0; name; typ; attr=[]}

let compare x y = Compare.on to_string x y
let eq x y = compare x y = 0
let same = eq

let set_id x id = {x with id}
let set_name x name = {x with name}
let set_typ x typ = {x with typ}

let add_name_before str x = set_name x (str ^ name x)
let add_name_after str x = set_name x (name x ^ str)

let add_attr attr x = {x with attr=attr::x.attr}

let mem x xs = List.mem ~eq x xs
let assoc x xs = List.assoc ~eq x xs
let mem_assoc x xs = List.mem_assoc ~eq x xs

let print fm x =
  let s =
    if !Flag.web then
      name x
    else
      to_string x
  in
  assert (s <> "");
  let s' =
    if !Flag.print_as_ocaml then
      String.sign_to_letters @@ String.uncapitalize s
    else
      s
  in
  Format.fprintf fm "@[%s@]" s'

let map_name f x = {x with name = f x.name}
let map_typ f x = {x with typ = f x.typ}

let is_external x = List.mem External x.attr
