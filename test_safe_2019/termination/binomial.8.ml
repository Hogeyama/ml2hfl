let rec bin (x_DO_NOT_CARE_99:bool) (x_DO_NOT_CARE_100:int)
           (x_DO_NOT_CARE_101:int) (n:int) (prev_set_flag_bin_84:bool)
           (s_prev_bin_n_82:int) (s_prev_bin_k_83:int) (k:int) =
  if prev_set_flag_bin_84
  then
    if
      s_prev_bin_k_83 > k && k >= 0 ||
      s_prev_bin_k_83 >= k &&
      (s_prev_bin_n_82 + -s_prev_bin_k_83 > n + -k && n + -k >= 0)
    then
      ()
    else
      assert false;
  bin_without_checking_97
    x_DO_NOT_CARE_99 x_DO_NOT_CARE_100 x_DO_NOT_CARE_101 n
    prev_set_flag_bin_84 s_prev_bin_n_82 s_prev_bin_k_83 k
and bin_without_checking_97 (_:bool) (_:int) (_:int) (n:int) (_:bool) 
                           (_:int) (_:int) (k:int) =
  let set_flag_bin_85 = true
  in
  let s_bin_k_81 = k
  in
  let s_bin_n_80 = n
  in
  if n = 0
  then
    1
  else
    if k <= 0 || k >= n
    then
      1
    else
      bin
        set_flag_bin_85 s_bin_n_80 s_bin_k_81 (n - 1) set_flag_bin_85
        s_bin_n_80 s_bin_k_81 (k - 1)
      +
      bin_without_checking_97
        set_flag_bin_85 s_bin_n_80 s_bin_k_81 (n - 1) set_flag_bin_85
        s_bin_n_80 s_bin_k_81 k
let main (set_flag_bin_85:bool) (s_bin_n_80:int) (s_bin_k_81:int) (():unit) =
  let n = Random.int 0
  in
  let k = Random.int 0
  in
  if n >= 0 && k >= 0
  then
    bin_without_checking_97
      set_flag_bin_85 s_bin_n_80 s_bin_k_81 n set_flag_bin_85 s_bin_n_80
      s_bin_k_81 k
  else
    0
let u_10751 = main false 0 0 ()
