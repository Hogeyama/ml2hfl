let rec f n =
  if n>0 then
    let rec g m =
      if m>0 then
	g (m-1)
      else
	0
    in
    g n + f (n-1)
  else
    0
in
let rec h v =
  if v>0 then
    h (v-1) + f v
  else
    0
in
h (Random.int 0)
