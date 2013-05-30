let rec div_by_2 n = if n < 1 then 0 else 1 + div_by_2 (n-2) in
let rec upper_bound m n check =
  if (n - m) < 2 then m
  else
    let mid = div_by_2 (m + n) in
    if check mid then
      upper_bound mid n check
    else
      upper_bound m mid check
in
let m = Random.int 0 in
let n = Random.int 0 in
let less_than (m : int) n = n < m in
if m > 0 && n > 0 then
  upper_bound m (m+n+n) (less_than (m+n))
else 0
