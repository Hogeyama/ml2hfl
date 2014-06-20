(*(*
let main n =
  let xs = make_list n in
  let ys,xs' = rev xs in
  let zs,ys' = rev (xs',ys) in
  assert (list_eq (xs', zs))

val ysxs' : (x,y):(int*int) -> {(r1,r2):int | x = n-y => r1 = r2}
val zsys' = (x,y):(int*int) -> {(r1,r2):int | x = n-y => r1 = r2}
*)


let rec rev1 n i acc xs =
  match xs with
    [] -> acc
  | x :: xs' -> rev1 n (i-1) (x::acc) xs'
let rev1 n xs = rev1 n n [] xs

let rec rev2 n i acc xsys =
  match xsys with
    _,[] -> acc
  | _,x :: xs' -> rev2 n (i-1) (x::acc) xs'
let rev2 n xsys = rev2 n n [] xsys

let main n =
  let xs = make_list n in
  let ysxs' = rev1 xs in
  let zsxs'ys' = rev2 (xs,ys) in
  let xs'zs (i,j) =
    let r1,r2,r3 = zsxs'ys' (i,None,j) in
    r1,r3
  in
  assert (list_eq xs'zs)
 *)



let rec list_eq (xs, ys) =
  match xs, ys with
    [], [] -> true
  | x::xs', y::ys' -> x = y && list_eq (xs', ys')
  | _ -> false

let rec make_list n =
  if n < 0
  then []
  else Random.int 0 :: make_list (n-1)


let rec snoc xs x =
  match xs with
  | [] -> [x]
  | x'::xs' -> x' :: snoc xs' x

let rec rev1 xs =
  match xs with
  | [] -> []
  | x::xs' -> snoc (rev1 xs) x

let rec rev_append xs ys =
  match xs with
  | [] -> ys
  | x::xs' -> rev_append xs (x::ys)

let rev2 xs = rev_append xs []

let main n =
  let xs = make_list n in
  assert (list_eq (rev1 xs, rev2 xs))
