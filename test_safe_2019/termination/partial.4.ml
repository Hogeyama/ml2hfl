let rec f_59 (x_DO_NOT_CARE_135:bool) (x_DO_NOT_CARE_136:int)
            (x_DO_NOT_CARE_137:int) (lb:int) (prev_set_flag_f_122:bool)
            (s_prev_f_lb_120:int) (s_prev_f_n_121:int) (n:int) =
  if prev_set_flag_f_122 then assert false;
  f_without_checking_133
    x_DO_NOT_CARE_135 x_DO_NOT_CARE_136 x_DO_NOT_CARE_137 lb
    prev_set_flag_f_122 s_prev_f_lb_120 s_prev_f_n_121 n
and f_without_checking_133 (_:bool) (_:int) (_:int) (lb:int) (_:bool)
                          (_:int) (_:int) (n:int) =
  let set_flag_f_123 = true
  in
  let s_f_n_119 = n
  in
  let s_f_lb_118 = lb
  in
  n > lb
and f (_:bool) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int)
     (p_EXPARAM_60:int) (set_flag_f_123:bool) (s_f_lb_118:int)
     (s_f_n_119:int) (p:(bool -> int -> int -> int -> bool)) =
  if p set_flag_f_123 s_f_lb_118 s_f_n_119 x
  then
    f
      set_flag_f_123 s_f_lb_118 s_f_n_119 (x - 1) set_flag_f_123
      s_f_lb_118 s_f_n_119 (0 * p_EXPARAM_60 + (0 * x + 0))
      set_flag_f_123 s_f_lb_118 s_f_n_119 p
and u_57 =
  let gt = f_59
  in
  f false 0 0 (Random.int 0) false 0 0 0 false 0 0 (gt false 0 0 0)
let u_7455 = ()
