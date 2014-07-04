(*{SPEC}
valcps insert :
  int ->
  ((i1:int -> j1:int ->
    (b1:bool -> r1:int ->
     b2:bool -> r2:int[not b1 || not b2 || not (i1 <= j1); i1 < 0; j1 < 0; r1 <= r2] -> X) -> X)
   ->
   ((i2:int -> j2:int ->
     (b3:bool -> r3:int ->
      b4:bool -> r4:int[not b3 || not b4 || not (i2 <= j2); i2 < 0; j2 < 0; r3 <= r4] ->
      X)
     -> X)
    -> X) -> X)

inlinef none
inlinef some
{SPEC}*)
let is_none (b,_) = not b
let none = (false,0)
let some x = (true,x)

let rec loop () = loop ()

let rec insert (x:int) ysys =
  let b,y = fst (ysys (0,0)) in
  if not b
  then
    let rs i = if i<=0 then some x else none in
    let rsrs (i,j) =
      if i = j then
        let r = rs i in
        r, r
      else
        rs i, rs j
    in
    rsrs
  else
    let ys'ys' (i,j) = ysys (i+1, j+1) in
    if x < y then
      (fun (i,j) ->
       if i <= 0 then
         if j <= 0 then
           some x, some x
         else
           let (b1,y'),(b2,y'') = ysys (0, j-1) in
           if not b2 then loop () else
           if j-1 = 0 && y' <> y'' then loop () else
           if b <> b1 || y <> y' then loop () else
           some x, some y''
       else
         if j <= 0 then
           fst (ysys (i-1, 0)), some x
         else
           ysys (i-1, j-1))
    else
      let rsrs = insert x ys'ys' in
      (fun (i,j) ->
       if i <= 0 then
         if j <= 0 then
           some y, some y
         else
           let (b1,y'),(b2,y'') = rsrs (0, j-1) in
           if not b2 then loop () else
           if j-1 = 0 && y' <> y'' then loop () else
           if b <> b1 || y <> y' then loop () else
           some y, some y''
       else
         if j <= 0 then
           fst (rsrs (i, 0)), some y
         else
           rsrs (i-1, j-1))

(*{SPEC}
valcps insertsort :
   ((i1:int -> j1:int ->
     (b1:bool -> r1:int ->
      b2:bool -> r2:int -> X) -> X)
    ->
    ((i2:int -> j2:int ->
      (b3:bool -> r3:int ->
       b4:bool -> r4:int[not b3 || not b4 || not (i2 <= j2); i2 < 0; j2 < 0; r3 <= r4] ->
       X)
      -> X)
     -> X) -> X)
{SPEC}*)
let rec insertsort xsxs =
  if is_none (fst (xsxs (0,0)))
  then
    let rsrs i = none, none in
    rsrs
  else
    let _,x = fst (xsxs (0,0)) in
    let xs'xs' (i,j) = xsxs(i+1, j+1) in
    insert x (insertsort xs'xs')

let rec make_list n =
  if n = 0
  then fun i -> none
  else
    let n = Random.int 0 in
    let xs = make_list (n-1) in
    fun i ->
    if i = 0 then some n
    else xs (i - 1)

(*{SPEC}
valcps check :
   ((i:int ->
     j:int ->
     (b1:bool -> r1:int ->
      b2:bool -> r2:int[not b1 || not b2 || not (i <= j); i < 0; j < 0; r1 <= r2] -> X)
     -> X)
    -> (x_14:bool[x_14] -> X) -> X)
{SPEC}*)
let rec check xsxs =
  let (b1,x1),(b2,x2) = xsxs (0,0) in
  if not b1 then
    true
  else if not b2 then
    true
  else
    let xs'xs' (i,j) = xsxs (i + 1,j+1) in
    x1 <= x2 && check xs'xs'

let main n =
  let xs = make_list n in
  let xsxs (i,j) =
    if i = j
    then
      let r = xs i in
      r, r
    else
      xs i, xs j
  in
  let ysys = insertsort xsxs in
  assert (check ysys)
