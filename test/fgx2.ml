let rec f g x = if g x>=3 then assert false else f (f g) (g x) in
let rec succ x = x+1 in
f succ 0
