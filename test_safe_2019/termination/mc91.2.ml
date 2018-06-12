let rec mc91 (prev_set_flag_mc91_38:bool) (s_prev_mc91_n_37:int) (n:int) =
  if prev_set_flag_mc91_38
  then
    if 111 + -s_prev_mc91_n_37 > 111 + -n && 111 + -n >= 0
    then
      ()
    else
      assert false;
  mc91_without_checking_46 prev_set_flag_mc91_38 s_prev_mc91_n_37 n
and mc91_without_checking_46 (_:bool) (_:int) (n:int) =
  let set_flag_mc91_39 = true
  in
  let s_mc91_n_36 = n
  in
  if n > 100
  then
    n - 10
  else
    mc91_without_checking_46
      set_flag_mc91_39 s_mc91_n_36
      (mc91 set_flag_mc91_39 s_mc91_n_36 (n + 11))
and u_34 = mc91_without_checking_46 false 0 (Random.int 0)
let u_692 = ()
