(*(*
let compose f g x = f (g x)
let id x = x
let rec fib n =
  if n <= 1
  then n
  else fib (n-1) + fib (n-2)
let main n =
  let fg = compose id fib in
  assert (fg x = fib x)
*)

(int->int) * (int->int) -> int->int

let apply_opt f x =
  if is_none x
  then none
  else some (f (get_val x))

(int*int -> int*int) -> int->int
fg:(int*int -> int*int) -> (int->int)*(int*int->int*int)
let compose fg x =
  let y = snd fg x in
  fst fg y, fg

let compose fg xyz =
  let y = get_val (snd (fg (none, some (fst x)))) in
  some (get_val (fst fg (some y, none))), fg (snd xyz)

let compose fg xyz =
  let y = get_val (snd (fg (none, some (fst x)))) in
  some (get_val (fst fg (some y, none))), fg (snd xyz)

let id x = x
let rec fib n =
  if n <= 1
  then n
  else fib (n-1) + fib (n-2)
let idfib (x,n) =
  if n <= 1
  then x, n
  else x, fib (n-1) + fib (n-2)

let main n =
  let fg = compose idfib
*)

let rec append xs ys =
  match xs 0 with
    None -> ys
  | Some x ->
      let xs' i = xs (i+1) in
      let zs = append xs' ys in
      fun i -> if i = 0 then x else zs (i-1)

let rec append xsys =
  match fst xs 0 with
    None -> snd ys
  | Some x ->
      let xs i = fst xsys (Some i', None) in
      let xs' i = xs (i+1) in
      let xs'ys (i,j) = xs' i, ys j in
      let zs = append xs' ys in
      fun i -> if i = 0 then x else zs (i-1)

let rec append xsys =
