let rec bin (x_DO_NOT_CARE_98:bool) (x_DO_NOT_CARE_99:int)
           (x_DO_NOT_CARE_100:int) (n:int) (prev_set_flag_bin_83:bool)
           (s_prev_bin_n_81:int) (s_prev_bin_k_82:int) (k:int) =
  if prev_set_flag_bin_83
  then
    if
      s_prev_bin_k_82 > k && k >= 0 ||
      s_prev_bin_k_82 >= k &&
      (s_prev_bin_n_81 + -s_prev_bin_k_82 > n + -k && n + -k >= 0)
    then
      ()
    else
      assert false;
  bin_without_checking_96
    x_DO_NOT_CARE_98 x_DO_NOT_CARE_99 x_DO_NOT_CARE_100 n
    prev_set_flag_bin_83 s_prev_bin_n_81 s_prev_bin_k_82 k
and bin_without_checking_96 (_:bool) (_:int) (_:int) (n:int) (_:bool) 
                           (_:int) (_:int) (k:int) =
  let set_flag_bin_84 = true
  in
  let s_bin_k_80 = k
  in
  let s_bin_n_79 = n
  in
  if n = 0
  then
    1
  else
    if k <= 0 || k >= n
    then
      1
    else
      bin_without_checking_96
        set_flag_bin_84 s_bin_n_79 s_bin_k_80 (n - 1) set_flag_bin_84
        s_bin_n_79 s_bin_k_80 (k - 1)
      +
      bin
        set_flag_bin_84 s_bin_n_79 s_bin_k_80 (n - 1) set_flag_bin_84
        s_bin_n_79 s_bin_k_80 k
let main (set_flag_bin_84:bool) (s_bin_n_79:int) (s_bin_k_80:int) (():unit) =
  let n = Random.int 0
  in
  let k = Random.int 0
  in
  if n >= 0 && k >= 0
  then
    bin_without_checking_96
      set_flag_bin_84 s_bin_n_79 s_bin_k_80 n set_flag_bin_84 s_bin_n_79
      s_bin_k_80 k
  else
    0
let u_9294 = main false 0 0 ()
