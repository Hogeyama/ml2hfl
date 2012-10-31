
exception Not_found

type dict = Empty | Cons of int * int * dict

let rec assoc n dict =
  match dict with
      Empty -> raise Not_found
    | Cons(m,x,dict') -> if n = m then x else assoc n dict'

let rec fib dict n =
  if n <= 0 then dict, 0
  else if n = 1 then dict, 1
  else
    try
      dict, assoc n dict
    with Not_found ->
      let dict',x1 = fib dict (n-2) in
      let dict'',x2 = fib dict' (n-1) in
      let x = x1 + x2 in
        Cons(n,x,dict''), x

let main n =
  let _,x = fib Empty n in
    assert (x >= 0)
