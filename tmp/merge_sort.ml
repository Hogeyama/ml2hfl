let none = (false,0)
let some x = (true,x)
let rec loop () = loop ()
let nil _ = none
let cons x xs =
  fun i -> if i = 0 then some x else xs (i-1)

(*{SPEC}
inlinef none

valcps merge :
   ((i1:int -> j1:int -> (b1:bool -> r1:int -> b2:bool -> r2:int[not b1 || not b2 || not (0 <= i1) || not (i1 <= j1) || r1 <= r2] -> X) -> X) ->
    (i2:int -> j2:int -> (b3:bool -> r3:int -> b4:bool -> r4:int[not b3 || not b4 || not (0 <= i2) || not (i2 <= j2) || r3 <= r4] -> X) -> X) ->
    ((i3:int -> j3:int -> (b5:bool -> r5:int -> b6:bool -> r6:int[not b5 || not b6 || not (0 <= i3) || not (i3 <= j3) || r5 <= r6] -> X) -> X) -> X) -> X)
{SPEC}*)
let rec merge xsxs ysys =
  let xs i = fst (xsxs (i,0)) in
  let ys i = fst (ysys (i,0)) in
  let (b1,x),_ = xsxs (0,0) in
  if not b1
  then ysys
  else
    let (b2,y),_ = xsxs (0,0) in
    if not b2
    then xsxs
    else
      let xs'xs' (i,j) = xsxs (i+1,j+1) in
      let ys'ys' (i,j) = ysys (i+1,j+1) in
      if x <= y
      then
        let rsrs = merge xs'xs' ysys in
        fun (i,j) ->
        if i <= 0
        then
          if j <= 0
          then some x, some x
          else some x, snd (rsrs (0,j))
        else
          if j <= 0
          then fst (rsrs (0,j)), some x
          else rsrs (i-1,j-1)
      else
        let rsrs = merge xsxs ys'ys' in
        fun (i,j) ->
        if i <= 0
        then
          if j <= 0
          then some y, some y
          else some y, snd (rsrs (0,j-1))
        else
          if j <= 0
          then fst (rsrs (0,j)), some y
          else rsrs (i-1,j-1)

let rec split xs =
  let b1,x = xs 0 in
  if not b1 then
    nil,nil
  else
    let b2,y = xs 1 in
    if not b2 then
      (fun i -> if i = 0 then some x else none), nil
    else
      let xs' i = xs (i+2) in
      let ys,zs = split xs' in
      cons x ys, cons y zs

(*{SPEC}
valcps merge_sort :
   ((int -> (bool -> int -> X) -> X) ->
    ((i:int -> j:int ->
      (b1:bool -> r1:int ->
       b2:bool -> r2:int[not b1 || not b2 || not (0 <= i) || not (i <= j) || r1 <= r2] -> X) -> X) -> X) -> X)
{SPEC}*)
let rec merge_sort xs =
  let b1,x = xs 0 in
  if not b1 then
    fun (i,j) -> none,none
  else
    let b2,y = xs 1 in
    if not b2 then
      fun (i,j) ->
      if i <= 0
      then
        if j <= 0
        then some x, some x
        else some x, none
      else
        if j <= 0
        then none, some x
        else none, none
    else
      let ys,zs = split xs in
      let ys'ys' = merge_sort ys in
      let zs'zs' = merge_sort zs in
      merge ys'ys' zs'zs'

let rec make_list n =
  if n = 0
  then nil
  else cons (Random.int 0) @@ make_list (n-1)

let main n i j =
  let xs = make_list n in
  let ysys = merge_sort xs in
  if 0 < i && i < j
  then
    let (b1,r1),(b2,r2) = ysys (i,j) in
    if b1 && b2
    then assert (r1 <= r2)
