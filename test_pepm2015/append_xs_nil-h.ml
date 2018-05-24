(*
let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let rec list_eq xs ys =
  match xs, ys with
    0, 0 -> true
  | x::xs', y::ys' -> x = y && list_eq xs' ys'
  | _ -> false

let main n =
  let xs = make_list n in
  assert (list_eq xs (append [] xs))
*)



(*
let rec append xs ys =
  match xs with
    [] -> ys
  | x::xs' -> x :: append xs' ys

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list n

let main n i =
  let xs = make_list n in
  let ys = append [] xs in
  assert (List.nth xs i = List.nth ys i)
*)



(*
let none = (0, 0)
let some x = (1, x)
let is_none (x,_) = x = 0

let nil x = none
let cons x xs = fun i -> if i = 0 then some x else xs (i-1)
let tl xs = fun i -> xs (i+1)

let rec append xs ys =
  if is_none (xs 0)
  then ys
  else
    let _,x = xs 0 in
    let xs' = tl xs in
    cons x (append xs' ys)

let rec make_list n =
  if n < 0
  then nil
  else cons (Random.int 0) (make_list n)

let rec list_eq xs ys =
  let b1 = is_none (xs 0) in
  let b2 = is_none (ys 0) in
  if b1 && b2 then true
  else if not b1 && not b2 then
    let _,x = xs 0 in
    let _,y = ys 0 in
    let xs' = tl xs in
    let ys' = tl ys in
    list_eq xs' ys'
  else false

let main n =
  let xs = make_list n in
  let ys = append nil xs in
  assert (list_eq xs ys)
*)


(*
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
    list_eq xsys'
  else false

let rec append xs ysys =
  if is_none (xs 0)
  then ysys
  else
    let _,x = xs 0 in
    let xs' = tl xs in
    let zszs = append xs' ysys in
    let zs = fun i -> fst (zszs (i,0)) in
    let zs' = cons x zs in
    fun (i,j) ->
    let x=  zs' i in
    let y = zs' j in
 assume (not (i=j) || fst x=fst y&&snd x =snd x); x, y

let main n =
  let xs = make_list n in
  let xsxs (i,j) =
    let x = xs i in
    let y = xs j in
    assume (not (i=j) || fst x=fst y&&snd x =snd x);
    (x,y)
  in
  let xsys = append nil xsxs in
  let xsys' = fun (i,j) -> xsys (i, j) in
  assert (list_eq xsys')
 *)


let rec assume b = if b then () else assume b

let none = (0, 0)
let some x = (1, x)
let is_none (x,_) = x = 0

let nil x = none
let cons (x:int) (xs:int->int*int) = fun i -> if i = 0 then some x else xs (i-1)
let tl xs = fun i -> xs (i+1)

let rec make_list n =
  if n < 0
  then nil
  else cons (Random.int 0) (make_list (n-1))

let trd (_,_,x) = x

let rec append (xsys:int*int -> (int*int)*(int*int)) :int*int*int -> (int*int)*(int*int)*(int*int) =
  let xs = fun i -> fst (xsys (i,0)) in
  let ys = fun i -> snd (xsys (0,i)) in
  let (b,x),_ = xsys (0,0) in
  if is_none (b,x)
  then
    (fun (i,j,k) ->
     let r1,r2 = xsys (i,j) in
     none, r2, ys k)
  else
    let _,(_,x) = xsys (0, 0) in
    let xs' = tl xs in
    let xs'ys (i,j) = xs' i, ys j in
    let xs''ys'rs = append xs'ys in
    let rs (i:int) :int*int = trd (xs''ys'rs (0,0,i)) in
    let rs' = cons x rs in
    let xs = cons x xs' in
    (fun (i,j,k) ->
     if i <= 0
     then
       if k <= 0
       then some x, ys i, some x
       else some x, ys i, rs' k
     else
       xs''ys'rs (i-1, i, i-1))

let main n i =
  let xs = make_list n in
  let xsys (i,j) = xs i, none in
  let xs'ys'rs = append xsys in
  let r1,_,r2 = xs'ys'rs (i, 0, i) in
  assert (r1 = r2)
