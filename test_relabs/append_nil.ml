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
let rec loop x = loop x

let rec append (l1,xs) (l2,ys) =
  if l1 = 0 then (l2,ys)
  else if l1 > 0 then
    let x = xs 0 in
    let xs' i = xs (i+1) in
    let l3,zs = append (l1-1,xs') (l2,ys) in
    let zs' i = if i = 0 then x else zs (i-1) in
    l3+1, zs'
  else loop ()

let rec make_list n =
  if n <= 0
  then (0, fun i -> loop ())
  else
    let x = Random.int 10 in
    let l,xs = make_list (n-1) in
    l+1, fun i -> if i=0 then x else xs (i-1)

let rec list_eq (l1,xs) (l2,ys) =
  if l1 = 0 && l2 = 0
  then true
  else
    if l1 > 0 && l2 > 0
    then
      let x = xs 0 in
      let y = ys 0 in
      let l1',xs' = l1-1, fun i -> xs (x+1) in
      let l2',ys' = l2-1, fun i -> ys (x+1) in
      x = y && list_eq (l1',xs') (l2',ys')
    else l1 < 0 || l2 < 0

let main n =
  let l,xs = make_list n in
  let l',xs' = append (0, fun i -> loop ()) (l,xs) in
  assert (list_eq (l,xs) (l',xs'))
*)

let rec loop x =
  match x with
    None -> None
  | Some x -> loop (Some x)

type i = int option

let rec append (l1xsl2ys:i*i->((int*i)*(int*i))) =
  let l1xs : i->int*i = fun x -> fst (l1xsl2ys (x,None)) in
  let l2ys : i->int*i = fun x -> snd (l1xsl2ys (None,x)) in
  let l1 = fst (l1xs None) in
  let xs : i->i = fun i -> snd (l1xs i) in
  if l1 = 0 then l2ys
  else if l1 > 0 then
    let z = 0 in
    let x =
      let z' = z in
      let r = l1xsl2ys (Some z, None) in
      snd (fst r)
    in
    let xs' (i:i) : i =
      match i with
        None -> None
      | Some i ->
        let i' = i + 1 in
        let r = l1xsl2ys (Some i', None) in
        snd (fst r)
    in
    let l1xs'l2ys (i, j) = (l1-1,xs' i),l2ys j in
    let l3zs : i->int*i = append l1xs'l2ys in
    let l3 = fst (l3zs None) in
    let zs' i =
      match i with
        None -> None
      | Some i ->
        if i = 0
        then x
        else
          let i' = i - 1 in
          snd (l3zs (Some i'))
    in
    fun i -> l3+1, zs' i
  else let rec loop x = loop x in loop ()

let rec make_list n =
  if n <= 0
  then fun i -> 0, loop i
  else
    let x = Random.int 10 in
    let lxs = make_list (n-1) in
    let l = fst (lxs None) in
    let xs i =
      match i with
        None -> None
      | Some i ->
      if i=0 then Some x else snd (lxs (Some (i-1)))
    in
    fun i -> l+1, xs i

let rec list_eq l1xsl2ys =
  let l1 = fst (fst (l1xsl2ys (None, None))) in
  let l2 = fst (snd (l1xsl2ys (None, None))) in
  if l1 = 0 && l2 = 0
  then true
  else
    if l1 > 0 && l2 > 0
    then
      let x =
        snd (fst (l1xsl2ys (Some 0, None)))
      in
      let y =
        snd (snd (l1xsl2ys (Some 0, Some 0)))
      in
      let l1' = l1-1 in
      let xs' i =
        match i with
          None -> None
        | Some i ->
        snd (fst (l1xsl2ys (Some (i+1), None)))
      in
      let l2' = l2-1 in
      let ys' i =
        match i with
          None -> None
        | Some i ->
        snd (snd (l1xsl2ys (Some (i+1), Some (i+1))))
      in
      let l1xsl2ys' (i, j) = (l1', xs' i), (l2', ys' i) in
      x = y && list_eq l1xsl2ys'
    else l1 < 0 || l2 < 0

let main n =
  let lxs = make_list n in
  let l'xs' = append (fun (i, j) -> (0, loop i), (lxs j)) in
  assert (list_eq (fun (i,j) -> (lxs i, l'xs' j)))
