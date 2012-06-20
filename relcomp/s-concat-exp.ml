let rec concat s1 s2 =
  if not (s1 0) then
    s2
  else
    fun i ->
      if i = 0 then
        s1 0
      else
        concat (fun i -> s1 (i + 1)) s2 (i - 1)

let nth s i = s i

let main n1 n2 =
  if n1 >= 0 && n2 >= 0 then
		  let s1 = fun i -> if i < n1 then true else false in
		  let s2 = fun i -> if i < n2 then true else false in
		  let s = concat s1 s2 in
		  assert (not (nth s (n1 + n2)))
