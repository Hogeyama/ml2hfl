type 'a t = {id:int; name:string; typ:'a}

let counter = ref 1

let new_int () = incr counter; !counter
let get_counter () = !counter
let set_counter n = counter := n

let make id name typ = {id=id; name=name; typ=typ}
let new_var s typ = {id=new_int(); name=s; typ=typ}
let new_var_id x = {x with id=new_int()}

let id x = x.id
let name x = x.name
let typ x = x.typ

let same x y = name x = name y && id x = id y
let compare x y = compare (name x,id x) (name y,id y)

let set_name x name = {x with name=name}
let set_typ x typ = {x with typ=typ}

let add_name x str = set_name x (name x ^ str)

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if same x a then b else assoc x l

let print fm x = Format.fprintf fm "%s_%n" (name x) (id x)

let to_string x = name x ^ "_" ^ string_of_int (id x)
