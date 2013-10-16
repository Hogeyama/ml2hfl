let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n-1) + fib (n-2)

let rec fib' a b n =
  if n = 0 then
    a
  else if n = 1 then
    b
  else
    fib' b (a+b) (n-1)

let rec fib'' n =
  if n = 0 then
    (0,0)
  else if n = 1 then
    (0,1)
  else
    let r = fib'' (n-1) in
    snd r, fst r + snd r

let fib_fib a b n = fib' a b n, fib'' n

let main a b n =
  let r1,r2 = fib_fib a b n in
  if a = 0 && b = 1 then
  assert (r1 = snd r2)
