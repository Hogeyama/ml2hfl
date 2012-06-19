let rec make n = fun i -> if i < n then true else false

let rec tail s = fun i -> s (i + 1)

let rec concat s1 s2 =
  if not (s1 0) then
    s2
  else
    fun i ->
      if i = 0 then
        s1 0
      else
        concat (tail s1) s2 (i - 1)

let main n1 n2 =
  if n1 >= 0 && n2 >= 0 then
		  let s1 = make n1 in
		  let s2 = make n2 in
		  let s = concat s1 s2 in
		  assert (not (s (n1 + n2)))
