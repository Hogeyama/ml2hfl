let rec mult (x_DO_NOT_CARE_64:bool) (x_DO_NOT_CARE_65:int)
            (x_DO_NOT_CARE_66:int) (m:int) (prev_set_flag_mult_50:bool)
            (s_prev_mult_m_48:int) (s_prev_mult_n_49:int) (n:int) =
  if prev_set_flag_mult_50
  then
    if s_prev_mult_m_48 > m && m >= 0 then () else assert false;
  mult_without_checking_62
    x_DO_NOT_CARE_64 x_DO_NOT_CARE_65 x_DO_NOT_CARE_66 m
    prev_set_flag_mult_50 s_prev_mult_m_48 s_prev_mult_n_49 n
and mult_without_checking_62 (_:bool) (_:int) (_:int) (m:int)
                            (_:bool) (_:int) (_:int) (n:int) =
  let set_flag_mult_51 = true
  in
  let s_mult_n_47 = n
  in
  let s_mult_m_46 = m
  in
  if m > 0
  then
    n +
    mult
      set_flag_mult_51 s_mult_m_46 s_mult_n_47 (m - 1)
      set_flag_mult_51 s_mult_m_46 s_mult_n_47 n
  else
    0
and u_44 =
  let m = Random.int 0
  in
  let n = Random.int 0
  in
  if m > 0
  then
    mult_without_checking_62 false 0 0 m false 0 0 n
  else
    0
let u_1223 = ()
