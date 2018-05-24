let rec f (prev_set_flag_f_41:bool) (s_prev_f_n_40:int) (n:int) =
  if prev_set_flag_f_41
  then
    if (-1) + s_prev_f_n_40 > (-1) + n && (-1) + n >= 0
    then
      ()
    else
      assert false;
  f_without_checking_52 prev_set_flag_f_41 s_prev_f_n_40 n
and f_without_checking_52 (_:bool) (_:int) (n:int) =
  let set_flag_f_42 = true
  in
  let s_f_n_39 = n
  in
  let r = Random.int 0
  in
  let delta = if r > 0 then r else 1 - r
  in
  let n_next = n - delta
  in
  (if n_next > 0
   then
     f_without_checking_52 set_flag_f_42 s_f_n_39 n_next)
and u_37 = f false 0 (Random.int 0)
let u_1378 = ()
