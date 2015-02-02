let f n k = if n >= 0 then () else k 0
let g n = assert (n = 0)
let main n = f n g

(*{SPEC}
type f : int -> ({n:int | n=0} -> unit) -> unit
{SPEC}*)
