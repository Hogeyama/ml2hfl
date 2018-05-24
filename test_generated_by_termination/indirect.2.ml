let c1_COEFFICIENT_75 = 0
let c0_COEFFICIENT_74 = 0
let id (_:bool) (_:int) (_:int) (x:unit) = x
let rec app (x_DO_NOT_CARE_155:bool) (x_DO_NOT_CARE_156:int)
           (x_DO_NOT_CARE_157:int) (h_EXPARAM_77:int)
           (x_DO_NOT_CARE_152:bool) (x_DO_NOT_CARE_153:int)
           (x_DO_NOT_CARE_154:int)
           (h:(bool ->
                 int -> int -> int -> bool -> int -> int -> unit -> unit))
           (x_DO_NOT_CARE_149:bool) (x_DO_NOT_CARE_150:int)
           (x_DO_NOT_CARE_151:int) (v:int) (prev_set_flag_app_126:bool)
           (s_prev_app_h_EXPARAM_122:int) (s_prev_app_v_124:int) (u:unit) =
  if prev_set_flag_app_126 then assert false;
  app_without_checking_147
    x_DO_NOT_CARE_155 x_DO_NOT_CARE_156 x_DO_NOT_CARE_157 h_EXPARAM_77
    x_DO_NOT_CARE_152 x_DO_NOT_CARE_153 x_DO_NOT_CARE_154 h
    x_DO_NOT_CARE_149 x_DO_NOT_CARE_150 x_DO_NOT_CARE_151 v
    prev_set_flag_app_126 s_prev_app_h_EXPARAM_122 s_prev_app_v_124 u
and app_without_checking_147 (_:bool) (_:int) (_:int) (h_EXPARAM_77:int)
                            (_:bool) (_:int) (_:int)
                            (h:(bool ->
                                  int ->
                                    int ->
                                      int ->
                                        bool ->
                                          int -> int -> unit -> unit))
                            (_:bool) (_:int) (_:int) (v:int) (_:bool)
                            (_:int) (_:int) (u:unit) =
  let set_flag_app_127 = true
  in
  let s_app_v_120 = v
  in
  let s_app_h_EXPARAM_118 = h_EXPARAM_77
  in
  h
    set_flag_app_127 s_app_h_EXPARAM_118 s_app_v_120 v set_flag_app_127
    s_app_h_EXPARAM_118 s_app_v_120 u
let rec f (set_flag_app_127:bool) (s_app_h_EXPARAM_118:int)
         (s_app_v_120:int) (x:int) =
  if x > 0
  then
    app
      set_flag_app_127 s_app_h_EXPARAM_118 s_app_v_120
      (c1_COEFFICIENT_75 * x + c0_COEFFICIENT_74) set_flag_app_127
      s_app_h_EXPARAM_118 s_app_v_120 f set_flag_app_127
      s_app_h_EXPARAM_118 s_app_v_120 (x - 1)
  else
    id
let main (set_flag_app_127:bool) (s_app_h_EXPARAM_118:int)
        (s_app_v_120:int) (():unit) =
  f
    set_flag_app_127 s_app_h_EXPARAM_118 s_app_v_120 (Random.int 0)
    set_flag_app_127 s_app_h_EXPARAM_118 s_app_v_120 ()
let u_2319 = main false 0 0 ()
