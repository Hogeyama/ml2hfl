(*
let rec rev acc xs =
  match xs with
    [] -> acc
  | x :: xs' -> rev (x::acc) xs'
let rev xs = rev [] xs

let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let main n =
  let xs = make_list n in
  assert (list_eq xs (rev (rev xs)))
*)


(*
let rec rev acc xs =
  if is_none (xs 0)
  then acc
  else
    let x = snd (xs 0) in
    let xs' = tl xs in
    rev (cons x acc) xs'
*)

let rec assume b = if b then () else assume b

let none = (0, 0)
let some x = (1, x)
let is_none (x,_) = x = 0

let nil x = none
let cons x xs = fun i -> if i = 0 then some x else xs (i-1)
let tl xs = fun i -> xs (i+1)

let rec make_list n =
  if n < 0
  then nil
  else cons (Random.int 0) (make_list (n-1))

(*{SPEC}
valcps list_eq :
  (i:int -> j:int[i=j] -> (int -> x:int -> int -> y:int[x=y] -> X) -> X) -> (bool -> X) -> X
{SPEC}*)
let rec list_eq xsys =
  let x,y = xsys (0,0) in
  let b1 = is_none x in
  let b2 = is_none y in
  if b1 && b2 then true
  else if not b1 && not b2 then
    let (_,x),(_,y) = xsys (0,0) in
    let xs' = tl (fun i -> fst (xsys (i,0))) in
    let ys' = tl (fun i -> snd (xsys (0,i))) in
    let xsys' (i,j) = xs' i, ys' j in
    let xsys' (i,j) = xsys (i+1, j+1) in
    list_eq xsys'
  else false

(*{SPEC}
valcps rev :
  (i:int -> ((x:int[i=0; x=0]) -> int -> X) -> X) ->
  (i:int -> j:int[i=j] -> (int -> x:int -> int -> y:int[x=y] -> X) -> X) ->
  ((i:int -> j:int[i=j] -> (int -> x:int -> int -> y:int[x=y] -> X) -> X) -> X) -> X
{SPEC}*)
(*rev : acc:int list -> xs:int list -> {rs:int list | nth i rs = } *)
let rec rev acc xs =
  if is_none (xs 0)
  then acc, xs
  else
    let x = snd (xs 0) in
    let xs' = tl xs in
    rev (cons x acc) xs', xs

let main n =
  let xsys = revappend nil xsxs in
  let xsys' = fun (i,j) -> xsys (i, j) in
  assert (list_eq xsys')
