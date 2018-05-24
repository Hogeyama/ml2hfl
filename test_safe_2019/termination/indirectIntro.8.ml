let c5_COEFFICIENT_115 = 0
let c4_COEFFICIENT_114 = 0
let c3_COEFFICIENT_113 = 0
let c2_COEFFICIENT_112 = 0
let c1_COEFFICIENT_109 = 0
let c0_COEFFICIENT_108 = 0
let rec app (_:bool) (_:int) (i:int) (_:bool) (_:int) (h_EXPARAM_111:int)
           (_:bool) (_:int)
           (h:(bool -> int -> int -> bool -> int -> unit -> unit)) (_:bool)
           (_:int) (v:int) (set_flag_f_243:bool) (s_f_x_240:int) (u:unit) =
  if i >= 0
  then
    app
      set_flag_f_243 s_f_x_240 (i - 1) set_flag_f_243 s_f_x_240
      (c5_COEFFICIENT_115 * v +
       (c4_COEFFICIENT_114 * h_EXPARAM_111 +
        (c3_COEFFICIENT_113 * i + c2_COEFFICIENT_112)))
      set_flag_f_243 s_f_x_240 h set_flag_f_243 s_f_x_240 v set_flag_f_243
      s_f_x_240 u
  else
    h set_flag_f_243 s_f_x_240 v set_flag_f_243 s_f_x_240 u
let g (_:bool) (_:int) (():unit) = ()
let rec f (prev_set_flag_f_242:bool) (s_prev_f_x_241:int) (x:int) =
  if prev_set_flag_f_242 then assert false;
  f_without_checking_251 prev_set_flag_f_242 s_prev_f_x_241 x
and f_without_checking_251 (_:bool) (_:int) (x:int) =
  let set_flag_f_243 = true
  in
  let s_f_x_240 = x
  in
  if x > 0
  then
    app
      set_flag_f_243 s_f_x_240 (Random.int 0) set_flag_f_243 s_f_x_240
      (c1_COEFFICIENT_109 * x + c0_COEFFICIENT_108) set_flag_f_243
      s_f_x_240 f_without_checking_251 set_flag_f_243 s_f_x_240 (x - 1)
  else
    g
let main (set_flag_f_243:bool) (s_f_x_240:int) (():unit) =
  f set_flag_f_243 s_f_x_240 (Random.int 0) set_flag_f_243 s_f_x_240 ()
let u_45766 = main false 0 ()
