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
let rec map (_:bool) (_:int) (_:int) (_:int) (f_EXPARAM_126:int) (_:bool)
           (_:int) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int -> int))
           (set_flag_compose_215:bool) (s_compose_f_EXPARAM_204:int)
           (s_compose_g_EXPARAM_206:int) (s_compose_x_208:int) (xs:int) =
  if xs = 0
  then
    0
  else
    f
      set_flag_compose_215 s_compose_f_EXPARAM_204 s_compose_g_EXPARAM_206
      s_compose_x_208 (Random.int 0)
    +
    map
      set_flag_compose_215 s_compose_f_EXPARAM_204 s_compose_g_EXPARAM_206
      s_compose_x_208
      (c11_COEFFICIENT_129 * xs +
       (c10_COEFFICIENT_128 * f_EXPARAM_126 + c9_COEFFICIENT_127))
      set_flag_compose_215 s_compose_f_EXPARAM_204 s_compose_g_EXPARAM_206
      s_compose_x_208 f set_flag_compose_215 s_compose_f_EXPARAM_204
      s_compose_g_EXPARAM_206 s_compose_x_208 (xs - 1)
let rec compose (x_DO_NOT_CARE_254:bool) (x_DO_NOT_CARE_255:int)
               (x_DO_NOT_CARE_256:int) (x_DO_NOT_CARE_257:int)
               (f_EXPARAM_122:int) (x_DO_NOT_CARE_250:bool)
               (x_DO_NOT_CARE_251:int) (x_DO_NOT_CARE_252:int)
               (x_DO_NOT_CARE_253:int)
               (f:(bool -> int -> int -> int -> int -> int))
               (x_DO_NOT_CARE_246:bool) (x_DO_NOT_CARE_247:int)
               (x_DO_NOT_CARE_248:int) (x_DO_NOT_CARE_249:int)
               (g_EXPARAM_123:int) (x_DO_NOT_CARE_242:bool)
               (x_DO_NOT_CARE_243:int) (x_DO_NOT_CARE_244:int)
               (x_DO_NOT_CARE_245:int)
               (g:(bool -> int -> int -> int -> int -> int))
               (prev_set_flag_compose_214:bool)
               (s_prev_compose_f_EXPARAM_209:int)
               (s_prev_compose_g_EXPARAM_211:int) (s_prev_compose_x_213:int)
               (x:int) =
  if prev_set_flag_compose_214 then assert false;
  compose_without_checking_240
    x_DO_NOT_CARE_254 x_DO_NOT_CARE_255 x_DO_NOT_CARE_256
    x_DO_NOT_CARE_257 f_EXPARAM_122 x_DO_NOT_CARE_250 x_DO_NOT_CARE_251
    x_DO_NOT_CARE_252 x_DO_NOT_CARE_253 f x_DO_NOT_CARE_246
    x_DO_NOT_CARE_247 x_DO_NOT_CARE_248 x_DO_NOT_CARE_249 g_EXPARAM_123
    x_DO_NOT_CARE_242 x_DO_NOT_CARE_243 x_DO_NOT_CARE_244
    x_DO_NOT_CARE_245 g prev_set_flag_compose_214
    s_prev_compose_f_EXPARAM_209 s_prev_compose_g_EXPARAM_211
    s_prev_compose_x_213 x
and compose_without_checking_240 (_:bool) (_:int) (_:int) (_:int)
                                (f_EXPARAM_122:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (f:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int)
                                (g_EXPARAM_123:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (g:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int) (x:int) =
  let set_flag_compose_215 = true
  in
  let s_compose_x_208 = x
  in
  let s_compose_g_EXPARAM_206 = g_EXPARAM_123
  in
  let s_compose_f_EXPARAM_204 = f_EXPARAM_122
  in
  f
    set_flag_compose_215 s_compose_f_EXPARAM_204 s_compose_g_EXPARAM_206
    s_compose_x_208
    (g
      set_flag_compose_215 s_compose_f_EXPARAM_204
      s_compose_g_EXPARAM_206 s_compose_x_208 x)
let add (_:bool) (_:int) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int)
       (_:int) (y:int) = x + y
let main (set_flag_compose_215:bool) (s_compose_f_EXPARAM_204:int)
        (s_compose_g_EXPARAM_206:int) (s_compose_x_208:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_compose_215 s_compose_f_EXPARAM_204
      s_compose_g_EXPARAM_206 s_compose_x_208
      (c8_COEFFICIENT_120 * l +
       (c7_COEFFICIENT_119 * l + c6_COEFFICIENT_118))
      set_flag_compose_215 s_compose_f_EXPARAM_204
      s_compose_g_EXPARAM_206 s_compose_x_208
      (compose
        set_flag_compose_215 s_compose_f_EXPARAM_204
        s_compose_g_EXPARAM_206 s_compose_x_208
        (c2_COEFFICIENT_112 * l +
         (c1_COEFFICIENT_111 * l + c0_COEFFICIENT_110))
        set_flag_compose_215 s_compose_f_EXPARAM_204
        s_compose_g_EXPARAM_206 s_compose_x_208
        (add
          set_flag_compose_215 s_compose_f_EXPARAM_204
          s_compose_g_EXPARAM_206 s_compose_x_208 1)
        set_flag_compose_215 s_compose_f_EXPARAM_204
        s_compose_g_EXPARAM_206 s_compose_x_208
        (c5_COEFFICIENT_115 * l +
         (c4_COEFFICIENT_114 * l + c3_COEFFICIENT_113))
        set_flag_compose_215 s_compose_f_EXPARAM_204
        s_compose_g_EXPARAM_206 s_compose_x_208
        (add
          set_flag_compose_215 s_compose_f_EXPARAM_204
          s_compose_g_EXPARAM_206 s_compose_x_208 2))
      set_flag_compose_215 s_compose_f_EXPARAM_204
      s_compose_g_EXPARAM_206 s_compose_x_208 l
  else
    0
let u_15577 = main false 0 0 0 ()
