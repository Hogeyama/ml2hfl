let rec sum (prev_set_flag_sum_35:bool) (s_prev_sum_n_34:int) (n:int) =
  if prev_set_flag_sum_35 then assert false;
  sum_without_checking_43 prev_set_flag_sum_35 s_prev_sum_n_34 n
and sum_without_checking_43 (_:bool) (_:int) (n:int) =
  let set_flag_sum_36 = true
  in
  let s_sum_n_33 = n
  in
  if n <= 0 then 0 else n + sum set_flag_sum_36 s_sum_n_33 (n - 1)
and u_31 = sum_without_checking_43 false 0 (Random.int 0)
let u_99 = ()
