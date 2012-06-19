let tl s i = s (i + 1)
let make n i = if i < n then true else false
let rec length s = if not (s 0) then 0 else 1 + (length (tl s))
let main n = (*if n >= 0 then*) assert (length (make n) >= n) (*else ()*)
