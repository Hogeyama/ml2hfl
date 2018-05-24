let c1_COEFFICIENT_79 = 0
let c0_COEFFICIENT_78 = 0
let id (_:bool) (_:int) (x:unit) = x
let rec app (x_DO_NOT_CARE_153:bool) (x_DO_NOT_CARE_154:int)
           (h_EXPARAM_81:int) (x_DO_NOT_CARE_151:bool)
           (x_DO_NOT_CARE_152:int)
           (h:(bool -> int -> unit -> bool -> int -> unit -> unit))
           (prev_set_flag_app_128:bool) (s_prev_app_h_EXPARAM_125:int)
           (v:unit) =
  if prev_set_flag_app_128
  then
    if
      4 + (-4) * s_prev_app_h_EXPARAM_125 > 4 + (-4) * h_EXPARAM_81 &&
      4 + (-4) * h_EXPARAM_81 >= 0
    then
      ()
    else
      assert false;
  app_without_checking_149
    x_DO_NOT_CARE_153 x_DO_NOT_CARE_154 h_EXPARAM_81 x_DO_NOT_CARE_151
    x_DO_NOT_CARE_152 h prev_set_flag_app_128 s_prev_app_h_EXPARAM_125 
    v
and app_without_checking_149 (_:bool) (_:int) (h_EXPARAM_81:int) (_:bool)
                            (_:int)
                            (h:(bool ->
                                  int -> unit -> bool -> int -> unit -> unit))
                            (_:bool) (_:int) (v:unit) =
  let set_flag_app_129 = true
  in
  let s_app_h_EXPARAM_122 = h_EXPARAM_81
  in
  h
    set_flag_app_129 s_app_h_EXPARAM_122 () set_flag_app_129
    s_app_h_EXPARAM_122 v
let rec f (_:bool) (_:int) (n:int) (set_flag_app_129:bool)
         (s_app_h_EXPARAM_122:int) (():unit) =
  if n > 0
  then
    app
      set_flag_app_129 s_app_h_EXPARAM_122
      (c1_COEFFICIENT_79 * n + c0_COEFFICIENT_78) set_flag_app_129
      s_app_h_EXPARAM_122 (f set_flag_app_129 s_app_h_EXPARAM_122 (n - 1))
  else
    id
let main (set_flag_app_129:bool) (s_app_h_EXPARAM_122:int) (():unit) =
  f
    set_flag_app_129 s_app_h_EXPARAM_122 (Random.int 0) set_flag_app_129
    s_app_h_EXPARAM_122 () set_flag_app_129 s_app_h_EXPARAM_122 ()
let u_4476 = main false 0 ()
