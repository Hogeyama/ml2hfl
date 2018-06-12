let rec sum (prev_set_flag_sum_36:bool) (s_prev_sum_n_35:int) (n:int) =
  if prev_set_flag_sum_36
  then
    if s_prev_sum_n_35 > n && n >= 0 then () else assert false;
  sum_without_checking_44 prev_set_flag_sum_36 s_prev_sum_n_35 n
and sum_without_checking_44 (_:bool) (_:int) (n:int) =
  let set_flag_sum_37 = true
  in
  let s_sum_n_34 = n
  in
  if n <= 0
  then
    0
  else
    n + sum set_flag_sum_37 s_sum_n_34 (n - 1)
and u_32 = sum_without_checking_44 false 0 (Random.int 0)
let u_598 = ()
