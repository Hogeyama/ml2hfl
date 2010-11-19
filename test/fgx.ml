let rec f g x = if x<=0 then g x else f(f g) (x-1) in
let succ x = x+1 in
 assert(f succ 2 <1)
