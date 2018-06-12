let rec f (prev_set_flag_f_42:bool) (s_prev_f_n_41:int) (n:int) =
  if prev_set_flag_f_42
  then
    if (-1) + s_prev_f_n_41 > (-1) + n && (-1) + n >= 0
    then
      ()
    else
      assert false;
  f_without_checking_53 prev_set_flag_f_42 s_prev_f_n_41 n
and f_without_checking_53 (_:bool) (_:int) (n:int) =
  let set_flag_f_43 = true
  in
  let s_f_n_40 = n
  in
  let r = Random.int 0
  in
  let delta = if r > 0 then r else 1 - r
  in
  let n_next = n - delta
  in
  (if n_next > 0
   then
     f_without_checking_53 set_flag_f_43 s_f_n_40 n_next)
and u_38 = f false 0 (Random.int 0)
let u_1379 = ()
