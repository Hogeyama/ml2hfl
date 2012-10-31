let rec  f g x = if x=0 then (if true then g x else g x) else f(f g) (x-1) in
  assert(f succ 5 = 0)

