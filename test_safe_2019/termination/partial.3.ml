let rec f_59 (_:bool) (_:int) (_:int) (lb:int) (_:bool) (_:int) (_:int)
            (n:int) = n > lb
and f (x_DO_NOT_CARE_100:bool) (x_DO_NOT_CARE_101:int)
     (x_DO_NOT_CARE_102:int) (x:int) (x_DO_NOT_CARE_97:bool)
     (x_DO_NOT_CARE_98:int) (x_DO_NOT_CARE_99:int) (p_EXPARAM_60:int)
     (prev_set_flag_f_74:bool) (s_prev_f_x_71:int)
     (s_prev_f_p_EXPARAM_72:int) (p:(bool -> int -> int -> int -> bool)) =
  if prev_set_flag_f_74
  then
    if s_prev_f_x_71 > x && x >= 0 then () else assert false;
  f_without_checking_95
    x_DO_NOT_CARE_100 x_DO_NOT_CARE_101 x_DO_NOT_CARE_102 x
    x_DO_NOT_CARE_97 x_DO_NOT_CARE_98 x_DO_NOT_CARE_99 p_EXPARAM_60
    prev_set_flag_f_74 s_prev_f_x_71 s_prev_f_p_EXPARAM_72 p
and f_without_checking_95 (_:bool) (_:int) (_:int) (x:int) (_:bool)
                         (_:int) (_:int) (p_EXPARAM_60:int) (_:bool)
                         (_:int) (_:int)
                         (p:(bool -> int -> int -> int -> bool)) =
  let set_flag_f_75 = true
  in
  let s_f_p_EXPARAM_69 = p_EXPARAM_60
  in
  let s_f_x_68 = x
  in
  (if p set_flag_f_75 s_f_x_68 s_f_p_EXPARAM_69 x
   then
     f_without_checking_95
       set_flag_f_75 s_f_x_68 s_f_p_EXPARAM_69 (x - 1) set_flag_f_75
       s_f_x_68 s_f_p_EXPARAM_69 (0 * p_EXPARAM_60 + (0 * x + 0))
       set_flag_f_75 s_f_x_68 s_f_p_EXPARAM_69 p)
and u_57 =
  let gt = f_59
  in
  f false 0 0 (Random.int 0) false 0 0 0 false 0 0 (gt false 0 0 0)
let u_5440 = ()
