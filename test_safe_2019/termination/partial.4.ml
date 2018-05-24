let c3_COEFFICIENT_65 = 0
let c2_COEFFICIENT_62 = 0
let c1_COEFFICIENT_61 = 0
let c0_COEFFICIENT_60 = 0
let rec f_58 (x_DO_NOT_CARE_134:bool) (x_DO_NOT_CARE_135:int)
            (x_DO_NOT_CARE_136:int) (lb:int) (prev_set_flag_f_121:bool)
            (s_prev_f_lb_119:int) (s_prev_f_n_120:int) (n:int) =
  if prev_set_flag_f_121 then assert false;
  f_without_checking_132
    x_DO_NOT_CARE_134 x_DO_NOT_CARE_135 x_DO_NOT_CARE_136 lb
    prev_set_flag_f_121 s_prev_f_lb_119 s_prev_f_n_120 n
and f_without_checking_132 (_:bool) (_:int) (_:int) (lb:int) (_:bool)
                          (_:int) (_:int) (n:int) =
  let set_flag_f_122 = true
  in
  let s_f_n_118 = n
  in
  let s_f_lb_117 = lb
  in
  n > lb
and f (_:bool) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int)
     (p_EXPARAM_59:int) (set_flag_f_122:bool) (s_f_lb_117:int)
     (s_f_n_118:int) (p:(bool -> int -> int -> int -> bool)) =
  if p set_flag_f_122 s_f_lb_117 s_f_n_118 x
  then
    f
      set_flag_f_122 s_f_lb_117 s_f_n_118 (x - 1) set_flag_f_122
      s_f_lb_117 s_f_n_118
      (c2_COEFFICIENT_62 * p_EXPARAM_59 +
       (c1_COEFFICIENT_61 * x + c0_COEFFICIENT_60))
      set_flag_f_122 s_f_lb_117 s_f_n_118 p
and u_56 =
  let gt = f_58
  in
  f
    false 0 0 (Random.int 0) false 0 0 c3_COEFFICIENT_65 false 0 0
    (gt false 0 0 0)
let u_7454 = ()
