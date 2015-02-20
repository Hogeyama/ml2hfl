let rec copy x = if x=0 then 0 else 1 + copy (x-1)

(*{SPEC}
type copy : x:int -> {r:int | r = x}
{SPEC}*)
