let rec f g x = if x >= 0 then g x else f (f g) (g x) in
let succ x = x + 1 in
  assert (f succ n >= 0)
