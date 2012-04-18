let rec tl len s i = s (i+1)
let rec mklist n i = if i<n then 1 else 0
let rec length l s = if s 0=0 then 0 else 1+ (length (l-1) (tl l s))
let rec main n = (*if n>=0 then*) assert (length n (mklist n)>=n) (*else ()*)
