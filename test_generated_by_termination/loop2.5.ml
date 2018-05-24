let rec f (x_DO_NOT_CARE_87:bool) (x_DO_NOT_CARE_88:int)
         (x_DO_NOT_CARE_89:int) (m:int) (prev_set_flag_f_73:bool)
         (s_prev_f_m_71:int) (s_prev_f_n_72:int) (n:int) =
  if prev_set_flag_f_73
  then
    if s_prev_f_n_72 > n && n >= 0 then () else assert false;
  f_without_checking_85
    x_DO_NOT_CARE_87 x_DO_NOT_CARE_88 x_DO_NOT_CARE_89 m
    prev_set_flag_f_73 s_prev_f_m_71 s_prev_f_n_72 n
and f_without_checking_85 (_:bool) (_:int) (_:int) (m:int) (_:bool)
                         (_:int) (_:int) (n:int) =
  let set_flag_f_74 = true
  in
  let s_f_n_70 = n
  in
  let s_f_m_69 = m
  in
  let r = Random.int 0
  in
  if r > 0 && m > 0
  then
    f
      set_flag_f_74 s_f_m_69 s_f_n_70 (m - 1) set_flag_f_74 s_f_m_69
      s_f_n_70 n
  else
    (if r <= 0 && n > 0
     then
       f_without_checking_85
         set_flag_f_74 s_f_m_69 s_f_n_70 m set_flag_f_74 s_f_m_69
         s_f_n_70 (n - 1))
let main (set_flag_f_74:bool) (s_f_m_69:int) (s_f_n_70:int) (():unit) =
  f_without_checking_85
    set_flag_f_74 s_f_m_69 s_f_n_70 (Random.int 0) set_flag_f_74
    s_f_m_69 s_f_n_70 (Random.int 0)
let u_6072 = main false 0 0 ()
