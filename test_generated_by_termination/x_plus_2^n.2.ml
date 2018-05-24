let c1_COEFFICIENT_82 = 0
let c0_COEFFICIENT_81 = 0
let succ (_:bool) (_:int) (_:int) (n:int) = n + 1
let rec g (x_DO_NOT_CARE_153:bool) (x_DO_NOT_CARE_154:int)
         (x_DO_NOT_CARE_155:int) (r_EXPARAM_84:int) (x_DO_NOT_CARE_150:bool)
         (x_DO_NOT_CARE_151:int) (x_DO_NOT_CARE_152:int)
         (r:(bool -> int -> int -> int -> int)) (prev_set_flag_g_129:bool)
         (s_prev_g_r_EXPARAM_126:int) (s_prev_g_a_128:int) (a:int) =
  if prev_set_flag_g_129 then assert false;
  g_without_checking_148
    x_DO_NOT_CARE_153 x_DO_NOT_CARE_154 x_DO_NOT_CARE_155 r_EXPARAM_84
    x_DO_NOT_CARE_150 x_DO_NOT_CARE_151 x_DO_NOT_CARE_152 r
    prev_set_flag_g_129 s_prev_g_r_EXPARAM_126 s_prev_g_a_128 a
and g_without_checking_148 (_:bool) (_:int) (_:int) (r_EXPARAM_84:int)
                          (_:bool) (_:int) (_:int)
                          (r:(bool -> int -> int -> int -> int)) (_:bool)
                          (_:int) (_:int) (a:int) =
  let set_flag_g_130 = true
  in
  let s_g_a_125 = a
  in
  let s_g_r_EXPARAM_123 = r_EXPARAM_84
  in
  r
    set_flag_g_130 s_g_r_EXPARAM_123 s_g_a_125
    (r set_flag_g_130 s_g_r_EXPARAM_123 s_g_a_125 a)
let rec f (set_flag_g_130:bool) (s_g_r_EXPARAM_123:int) (s_g_a_125:int)
         (n:int) =
  if n = 0
  then
    succ
  else
    g
      set_flag_g_130 s_g_r_EXPARAM_123 s_g_a_125
      (c1_COEFFICIENT_82 * n + c0_COEFFICIENT_81) set_flag_g_130
      s_g_r_EXPARAM_123 s_g_a_125
      (f set_flag_g_130 s_g_r_EXPARAM_123 s_g_a_125 (n - 1))
let main (_:bool) (_:int) (_:int) (n:int) (set_flag_g_130:bool)
        (s_g_r_EXPARAM_123:int) (s_g_a_125:int) (x:int) =
  if n >= 0 && x >= 0
  then
    f
      set_flag_g_130 s_g_r_EXPARAM_123 s_g_a_125 n set_flag_g_130
      s_g_r_EXPARAM_123 s_g_a_125 x
  else
    0
let u_2778 = main false 0 0 (Random.int 0) false 0 0 (Random.int 0)
