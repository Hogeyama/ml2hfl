let rec mc91 (prev_set_flag_mc91_38:bool) (s_prev_mc91_n_37:int) (n:int) =
  if prev_set_flag_mc91_38 then assert false;
  mc91_without_checking_47 prev_set_flag_mc91_38 s_prev_mc91_n_37 n
and mc91_without_checking_47 (_:bool) (_:int) (n:int) =
  let set_flag_mc91_39 = true
  in
  let s_mc91_n_36 = n
  in
  if n > 100
  then
    n - 10
  else
    mc91_without_checking_47
      set_flag_mc91_39 s_mc91_n_36
      (mc91_without_checking_47 set_flag_mc91_39 s_mc91_n_36 (n + 11))
let main (set_flag_mc91_39:bool) (s_mc91_n_36:int) (():unit) =
  mc91 set_flag_mc91_39 s_mc91_n_36 (Random.int 0)
let u_123 = main false 0 ()
