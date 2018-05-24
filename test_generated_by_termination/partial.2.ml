let c3_COEFFICIENT_65 = 0
let c2_COEFFICIENT_62 = 0
let c1_COEFFICIENT_61 = 0
let c0_COEFFICIENT_60 = 0
let rec f_58 (_:bool) (_:int) (_:int) (lb:int) (_:bool) (_:int) (_:int)
            (n:int) = n > lb
and f (x_DO_NOT_CARE_99:bool) (x_DO_NOT_CARE_100:int) (x_DO_NOT_CARE_101:int)
     (x:int) (x_DO_NOT_CARE_96:bool) (x_DO_NOT_CARE_97:int)
     (x_DO_NOT_CARE_98:int) (p_EXPARAM_59:int) (prev_set_flag_f_73:bool)
     (s_prev_f_x_70:int) (s_prev_f_p_EXPARAM_71:int)
     (p:(bool -> int -> int -> int -> bool)) =
  if prev_set_flag_f_73
  then
    if s_prev_f_x_70 > x && x >= 0 then () else assert false;
  f_without_checking_94
    x_DO_NOT_CARE_99 x_DO_NOT_CARE_100 x_DO_NOT_CARE_101 x
    x_DO_NOT_CARE_96 x_DO_NOT_CARE_97 x_DO_NOT_CARE_98 p_EXPARAM_59
    prev_set_flag_f_73 s_prev_f_x_70 s_prev_f_p_EXPARAM_71 p
and f_without_checking_94 (_:bool) (_:int) (_:int) (x:int) (_:bool)
                         (_:int) (_:int) (p_EXPARAM_59:int) (_:bool)
                         (_:int) (_:int)
                         (p:(bool -> int -> int -> int -> bool)) =
  let set_flag_f_74 = true
  in
  let s_f_p_EXPARAM_68 = p_EXPARAM_59
  in
  let s_f_x_67 = x
  in
  (if p set_flag_f_74 s_f_x_67 s_f_p_EXPARAM_68 x
   then
     f
       set_flag_f_74 s_f_x_67 s_f_p_EXPARAM_68 (x - 1) set_flag_f_74
       s_f_x_67 s_f_p_EXPARAM_68
       (c2_COEFFICIENT_62 * p_EXPARAM_59 +
        (c1_COEFFICIENT_61 * x + c0_COEFFICIENT_60))
       set_flag_f_74 s_f_x_67 s_f_p_EXPARAM_68 p)
and u_56 =
  let gt = f_58
  in
  f_without_checking_94
    false 0 0 (Random.int 0) false 0 0 c3_COEFFICIENT_65 false 0 0
    (gt false 0 0 0)
let u_2781 = ()
