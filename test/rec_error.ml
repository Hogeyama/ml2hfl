let rec f g x = if g x>=3 then fail else  f (f g) (g x) in
let succ x = x+1 in
f succ 0
