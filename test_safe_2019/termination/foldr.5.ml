let c6_COEFFICIENT_97 = 0
let c5_COEFFICIENT_96 = 0
let c4_COEFFICIENT_95 = 0
let c3_COEFFICIENT_94 = 0
let c2_COEFFICIENT_91 = 0
let c1_COEFFICIENT_90 = 0
let c0_COEFFICIENT_89 = 0
let rec foldr (_:bool) (_:int) (_:int) (h_EXPARAM_93:int) (_:bool) (_:int)
             (_:int)
             (h:(bool ->
                   int -> int -> int -> bool -> int -> int -> int -> int))
             (_:bool) (_:int) (_:int) (e:int) (set_flag_sum_170:bool)
             (s_sum_m_165:int) (s_sum_n_166:int) (l:int) =
  if l = 0
  then
    e
  else
    h
      set_flag_sum_170 s_sum_m_165 s_sum_n_166 (Random.int 0)
      set_flag_sum_170 s_sum_m_165 s_sum_n_166
      (foldr
        set_flag_sum_170 s_sum_m_165 s_sum_n_166
        (c6_COEFFICIENT_97 * l +
         (c5_COEFFICIENT_96 * e +
          (c4_COEFFICIENT_95 * h_EXPARAM_93 + c3_COEFFICIENT_94)))
        set_flag_sum_170 s_sum_m_165 s_sum_n_166 h set_flag_sum_170
        s_sum_m_165 s_sum_n_166 e set_flag_sum_170 s_sum_m_165 s_sum_n_166
        (l - 1))
let rec sum (x_DO_NOT_CARE_183:bool) (x_DO_NOT_CARE_184:int)
           (x_DO_NOT_CARE_185:int) (m:int) (prev_set_flag_sum_169:bool)
           (s_prev_sum_m_167:int) (s_prev_sum_n_168:int) (n:int) =
  if prev_set_flag_sum_169 then assert false;
  sum_without_checking_181
    x_DO_NOT_CARE_183 x_DO_NOT_CARE_184 x_DO_NOT_CARE_185 m
    prev_set_flag_sum_169 s_prev_sum_m_167 s_prev_sum_n_168 n
and sum_without_checking_181 (_:bool) (_:int) (_:int) (m:int) (_:bool)
                            (_:int) (_:int) (n:int) =
  let set_flag_sum_170 = true
  in
  let s_sum_n_166 = n
  in
  let s_sum_m_165 = m
  in
  m + n
let main (set_flag_sum_170:bool) (s_sum_m_165:int) (s_sum_n_166:int)
        (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    foldr
      set_flag_sum_170 s_sum_m_165 s_sum_n_166
      (c2_COEFFICIENT_91 * l +
       (c1_COEFFICIENT_90 * l + c0_COEFFICIENT_89))
      set_flag_sum_170 s_sum_m_165 s_sum_n_166 sum set_flag_sum_170
      s_sum_m_165 s_sum_n_166 (Random.int 0) set_flag_sum_170 s_sum_m_165
      s_sum_n_166 l
  else
    0
let u_15588 = main false 0 0 ()
