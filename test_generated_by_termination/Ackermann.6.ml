let rec ack (x_DO_NOT_CARE_94:bool) (x_DO_NOT_CARE_95:int)
           (x_DO_NOT_CARE_96:int) (m:int) (prev_set_flag_ack_79:bool)
           (s_prev_ack_m_77:int) (s_prev_ack_n_78:int) (n:int) =
  if prev_set_flag_ack_79
  then
    if
      s_prev_ack_m_77 > m && m >= 0 ||
      s_prev_ack_m_77 >= m && (s_prev_ack_n_78 > n && n >= 0)
    then
      ()
    else
      assert false;
  ack_without_checking_92
    x_DO_NOT_CARE_94 x_DO_NOT_CARE_95 x_DO_NOT_CARE_96 m prev_set_flag_ack_79
    s_prev_ack_m_77 s_prev_ack_n_78 n
and ack_without_checking_92 (_:bool) (_:int) (_:int) (m:int) (_:bool) 
                           (_:int) (_:int) (n:int) =
  let set_flag_ack_80 = true
  in
  let s_ack_n_76 = n
  in
  let s_ack_m_75 = m
  in
  if m = 0
  then
    n + 1
  else
    if n = 0
    then
      ack_without_checking_92
        set_flag_ack_80 s_ack_m_75 s_ack_n_76 (m - 1) set_flag_ack_80
        s_ack_m_75 s_ack_n_76 1
    else
      ack_without_checking_92
        set_flag_ack_80 s_ack_m_75 s_ack_n_76 (m - 1) set_flag_ack_80
        s_ack_m_75 s_ack_n_76
        (ack_without_checking_92
          set_flag_ack_80 s_ack_m_75 s_ack_n_76 m set_flag_ack_80 s_ack_m_75
          s_ack_n_76 (n - 1))
let main (set_flag_ack_80:bool) (s_ack_m_75:int) (s_ack_n_76:int) (():unit) =
  let m = Random.int 0
  in
  let n = Random.int 0
  in
  if n > 0 && m > 0
  then
    ack
      set_flag_ack_80 s_ack_m_75 s_ack_n_76 m set_flag_ack_80 s_ack_m_75
      s_ack_n_76 n
  else
    0
let u_11048 = main false 0 0 ()
