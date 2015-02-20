let lock st = assert (st=0); 1
let unlock st = assert (st=1); 0
let f n st : int= if n > 0 then lock (st) else st
let g n st : int = if n > 0 then unlock (st) else st

(*{SPEC}
type f : n:int -> {s:int | not (n > 0) || s = 0} -> {r:int | not (n > 0) || r = 1}
type g : n:int -> {s:int | not (n > 0) || s = 1} -> {r:int | not (n > 0) || r = 0}
{SPEC}*)
