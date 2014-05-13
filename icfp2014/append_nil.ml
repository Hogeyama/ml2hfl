(*{SPEC}
valcps append :
   (int -> (bool -> int -> X) -> X) ->
   ((bool ->
     (int -> (bool -> int -> X) -> X) ->
     bool -> int ->
     (bool ->
      (bool -> i:int -> bool -> j:int[i=j] -> (bool -> bool -> x:int -> bool -> bool -> y:int[x=y] -> X) -> X) ->
      bool -> bool -> int -> X) -> X) -> X) -> X
{SPEC}*)
let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec make_list n =
  if n < 0
  then []
  else n :: make_list n

(*{SPEC}
valcps list_eq :
   ((bool -> i:int -> bool -> j:int[i=j] -> (bool -> bool -> x:int -> bool -> bool -> y:int[x=y] -> X) -> X) ->
    (bool ->
     (bool -> int -> bool -> int -> (bool -> bool -> int -> bool -> bool -> int -> X) -> X)
     -> X) -> X)
{SPEC}*)
let rec list_eq xsys =
  match xsys with
    [], [] -> true
  | x::xs', y::ys' -> x = y && list_eq (xs', ys')
  | _ -> false

let main n =
  let xs = make_list n in
  assert (list_eq (append [] xs, xs))
