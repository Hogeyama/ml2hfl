let rec foldr (_:bool) (_:int) (_:int) (h_EXPARAM_94:int) (_:bool) (_:int)
             (_:int)
             (h:(bool ->
                   int -> int -> int -> bool -> int -> int -> int -> int))
             (_:bool) (_:int) (_:int) (e:int) (set_flag_sum_171:bool)
             (s_sum_m_166:int) (s_sum_n_167:int) (l:int) =
  if l = 0
  then
    e
  else
    h
      set_flag_sum_171 s_sum_m_166 s_sum_n_167 (Random.int 0)
      set_flag_sum_171 s_sum_m_166 s_sum_n_167
      (foldr
        set_flag_sum_171 s_sum_m_166 s_sum_n_167
        (0 * l + (0 * e + (0 * h_EXPARAM_94 + 0))) set_flag_sum_171
        s_sum_m_166 s_sum_n_167 h set_flag_sum_171 s_sum_m_166 s_sum_n_167 
        e set_flag_sum_171 s_sum_m_166 s_sum_n_167 (l - 1))
let rec sum (x_DO_NOT_CARE_184:bool) (x_DO_NOT_CARE_185:int)
           (x_DO_NOT_CARE_186:int) (m:int) (prev_set_flag_sum_170:bool)
           (s_prev_sum_m_168:int) (s_prev_sum_n_169:int) (n:int) =
  if prev_set_flag_sum_170 then assert false;
  sum_without_checking_182
    x_DO_NOT_CARE_184 x_DO_NOT_CARE_185 x_DO_NOT_CARE_186 m
    prev_set_flag_sum_170 s_prev_sum_m_168 s_prev_sum_n_169 n
and sum_without_checking_182 (_:bool) (_:int) (_:int) (m:int) (_:bool)
                            (_:int) (_:int) (n:int) =
  let set_flag_sum_171 = true
  in
  let s_sum_n_167 = n
  in
  let s_sum_m_166 = m
  in
  m + n
let main (set_flag_sum_171:bool) (s_sum_m_166:int) (s_sum_n_167:int)
        (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    foldr
      set_flag_sum_171 s_sum_m_166 s_sum_n_167 (0 * l + (0 * l + 0))
      set_flag_sum_171 s_sum_m_166 s_sum_n_167 sum set_flag_sum_171
      s_sum_m_166 s_sum_n_167 (Random.int 0) set_flag_sum_171 s_sum_m_166
      s_sum_n_167 l
  else
    0
let u_15589 = main false 0 0 ()
