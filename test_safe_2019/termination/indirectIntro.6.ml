let c5_COEFFICIENT_115 = 0
let c4_COEFFICIENT_114 = 0
let c3_COEFFICIENT_113 = 0
let c2_COEFFICIENT_112 = 0
let c1_COEFFICIENT_109 = 0
let c0_COEFFICIENT_108 = 0
let rec app (x_DO_NOT_CARE_171:bool) (x_DO_NOT_CARE_172:int)
           (x_DO_NOT_CARE_173:int) (x_DO_NOT_CARE_174:int) (i:int)
           (x_DO_NOT_CARE_167:bool) (x_DO_NOT_CARE_168:int)
           (x_DO_NOT_CARE_169:int) (x_DO_NOT_CARE_170:int)
           (h_EXPARAM_111:int) (x_DO_NOT_CARE_163:bool)
           (x_DO_NOT_CARE_164:int) (x_DO_NOT_CARE_165:int)
           (x_DO_NOT_CARE_166:int)
           (h:(bool ->
                 int ->
                   int ->
                     int -> int -> bool -> int -> int -> int -> unit -> unit))
           (x_DO_NOT_CARE_159:bool) (x_DO_NOT_CARE_160:int)
           (x_DO_NOT_CARE_161:int) (x_DO_NOT_CARE_162:int) (v:int)
           (prev_set_flag_app_128:bool) (s_prev_app_i_123:int)
           (s_prev_app_h_EXPARAM_124:int) (s_prev_app_v_126:int) (u:unit) =
  if prev_set_flag_app_128
  then
    if
      s_prev_app_v_126 > v && v >= 0 ||
      s_prev_app_v_126 >= v && (1 + s_prev_app_i_123 > 1 + i && 1 + i >= 0)
    then
      ()
    else
      assert false;
  app_without_checking_157
    x_DO_NOT_CARE_171 x_DO_NOT_CARE_172 x_DO_NOT_CARE_173 x_DO_NOT_CARE_174 
    i x_DO_NOT_CARE_167 x_DO_NOT_CARE_168 x_DO_NOT_CARE_169 x_DO_NOT_CARE_170
    h_EXPARAM_111 x_DO_NOT_CARE_163 x_DO_NOT_CARE_164 x_DO_NOT_CARE_165
    x_DO_NOT_CARE_166 h x_DO_NOT_CARE_159 x_DO_NOT_CARE_160 x_DO_NOT_CARE_161
    x_DO_NOT_CARE_162 v prev_set_flag_app_128 s_prev_app_i_123
    s_prev_app_h_EXPARAM_124 s_prev_app_v_126 u
and app_without_checking_157 (_:bool) (_:int) (_:int) (_:int) (i:int)
                            (_:bool) (_:int) (_:int) (_:int)
                            (h_EXPARAM_111:int) (_:bool) (_:int) (_:int)
                            (_:int)
                            (h:(bool ->
                                  int ->
                                    int ->
                                      int ->
                                        int ->
                                          bool ->
                                            int -> int -> int -> unit -> unit))
                            (_:bool) (_:int) (_:int) (_:int) (v:int) 
                            (_:bool) (_:int) (_:int) (_:int) (u:unit) =
  let set_flag_app_129 = true
  in
  let s_app_v_121 = v
  in
  let s_app_h_EXPARAM_119 = h_EXPARAM_111
  in
  let s_app_i_118 = i
  in
  if i >= 0
  then
    app
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 (i - 1)
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121
      (c5_COEFFICIENT_115 * v +
       (c4_COEFFICIENT_114 * h_EXPARAM_111 +
        (c3_COEFFICIENT_113 * i + c2_COEFFICIENT_112)))
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 h
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 v
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 u
  else
    h
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 v
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 u
let g (_:bool) (_:int) (_:int) (_:int) (():unit) = ()
let rec f (set_flag_app_129:bool) (s_app_i_118:int) (s_app_h_EXPARAM_119:int)
         (s_app_v_121:int) (x:int) =
  if x > 0
  then
    app_without_checking_157
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121
      (Random.int 0) set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119
      s_app_v_121 (c1_COEFFICIENT_109 * x + c0_COEFFICIENT_108)
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 f
      set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121 (x - 1)
  else
    g
let main (set_flag_app_129:bool) (s_app_i_118:int) (s_app_h_EXPARAM_119:int)
        (s_app_v_121:int) (():unit) =
  f
    set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119 s_app_v_121
    (Random.int 0) set_flag_app_129 s_app_i_118 s_app_h_EXPARAM_119
    s_app_v_121 ()
let u_38141 = main false 0 0 0 ()
