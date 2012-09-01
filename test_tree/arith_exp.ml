exception Not_implemented

type exp = Int of int | Add of exp * exp | Sub of exp * exp

let rec eval t =
  match t with
      Int n -> n
    | Add(t1,t2) -> eval t1 + eval t2
    | Sub(t1,t2) -> raise Not_implemented

let rec make_exp () =
  if Random.bool ()
  then Int (Random.int 10)
  else Add(make_exp (), make_exp ())

let main u =
  let t = make_exp () in
    eval t
