let c11_COEFFICIENT_129 = 0
let c10_COEFFICIENT_128 = 0
let c9_COEFFICIENT_127 = 0
let c8_COEFFICIENT_120 = 0
let c7_COEFFICIENT_119 = 0
let c6_COEFFICIENT_118 = 0
let c5_COEFFICIENT_115 = 0
let c4_COEFFICIENT_114 = 0
let c3_COEFFICIENT_113 = 0
let c2_COEFFICIENT_112 = 0
let c1_COEFFICIENT_111 = 0
let c0_COEFFICIENT_110 = 0
let rec map (x_DO_NOT_CARE_179:bool) (x_DO_NOT_CARE_180:int)
           (x_DO_NOT_CARE_181:int) (f_EXPARAM_126:int)
           (x_DO_NOT_CARE_176:bool) (x_DO_NOT_CARE_177:int)
           (x_DO_NOT_CARE_178:int) (f:(bool -> int -> int -> int -> int))
           (prev_set_flag_map_138:bool) (s_prev_map_f_EXPARAM_135:int)
           (s_prev_map_xs_137:int) (xs:int) =
  if prev_set_flag_map_138 then assert false;
  map_without_checking_174
    x_DO_NOT_CARE_179 x_DO_NOT_CARE_180 x_DO_NOT_CARE_181 f_EXPARAM_126
    x_DO_NOT_CARE_176 x_DO_NOT_CARE_177 x_DO_NOT_CARE_178 f
    prev_set_flag_map_138 s_prev_map_f_EXPARAM_135 s_prev_map_xs_137 
    xs
and map_without_checking_174 (_:bool) (_:int) (_:int) (f_EXPARAM_126:int)
                            (_:bool) (_:int) (_:int)
                            (f:(bool -> int -> int -> int -> int))
                            (_:bool) (_:int) (_:int) (xs:int) =
  let set_flag_map_139 = true
  in
  let s_map_xs_134 = xs
  in
  let s_map_f_EXPARAM_132 = f_EXPARAM_126
  in
  if xs = 0
  then
    0
  else
    f set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 (Random.int 0) +
    map_without_checking_174
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
      (c11_COEFFICIENT_129 * xs +
       (c10_COEFFICIENT_128 * f_EXPARAM_126 + c9_COEFFICIENT_127))
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 f
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 (xs - 1)
let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int)
           (_:int) (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int)) (set_flag_map_139:bool)
           (s_map_f_EXPARAM_132:int) (s_map_xs_134:int) (x:int) =
  f
    set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
    (g set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 x)
let add (_:bool) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int) (y:int) =
  x + y
let main (set_flag_map_139:bool) (s_map_f_EXPARAM_132:int)
        (s_map_xs_134:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
      (c8_COEFFICIENT_120 * l +
       (c7_COEFFICIENT_119 * l + c6_COEFFICIENT_118))
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
      (compose
        set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
        (c2_COEFFICIENT_112 * l +
         (c1_COEFFICIENT_111 * l + c0_COEFFICIENT_110))
        set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
        (add set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 1)
        set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
        (c5_COEFFICIENT_115 * l +
         (c4_COEFFICIENT_114 * l + c3_COEFFICIENT_113))
        set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134
        (add set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 2))
      set_flag_map_139 s_map_f_EXPARAM_132 s_map_xs_134 l
  else
    0
let u_1394 = main false 0 0 ()
