let tl len s i = s (i + 1)
let make n i = if i < n then true else false
let rec length l s = if not (s 0) then 0 else 1 + (length (l - 1) (tl l s))
let main n = (*if n >= 0 then*) assert (length n (make n) >= n) (*else ()*)
