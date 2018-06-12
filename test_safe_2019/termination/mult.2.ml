let rec mult (x_DO_NOT_CARE_65:bool) (x_DO_NOT_CARE_66:int)
            (x_DO_NOT_CARE_67:int) (m:int) (prev_set_flag_mult_51:bool)
            (s_prev_mult_m_49:int) (s_prev_mult_n_50:int) (n:int) =
  if prev_set_flag_mult_51
  then
    if s_prev_mult_m_49 > m && m >= 0 then () else assert false;
  mult_without_checking_63
    x_DO_NOT_CARE_65 x_DO_NOT_CARE_66 x_DO_NOT_CARE_67 m
    prev_set_flag_mult_51 s_prev_mult_m_49 s_prev_mult_n_50 n
and mult_without_checking_63 (_:bool) (_:int) (_:int) (m:int)
                            (_:bool) (_:int) (_:int) (n:int) =
  let set_flag_mult_52 = true
  in
  let s_mult_n_48 = n
  in
  let s_mult_m_47 = m
  in
  if m > 0
  then
    n +
    mult
      set_flag_mult_52 s_mult_m_47 s_mult_n_48 (m - 1)
      set_flag_mult_52 s_mult_m_47 s_mult_n_48 n
  else
    0
and u_45 =
  let m = Random.int 0
  in
  let n = Random.int 0
  in
  if m > 0
  then
    mult_without_checking_63 false 0 0 m false 0 0 n
  else
    0
let u_1224 = ()
