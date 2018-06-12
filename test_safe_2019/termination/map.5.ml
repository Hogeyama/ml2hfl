let rec map (_:bool) (_:int) (_:int) (_:int) (f_EXPARAM_127:int) (_:bool)
           (_:int) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int -> int))
           (set_flag_compose_216:bool) (s_compose_f_EXPARAM_205:int)
           (s_compose_g_EXPARAM_207:int) (s_compose_x_209:int) (xs:int) =
  if xs = 0
  then
    0
  else
    f
      set_flag_compose_216 s_compose_f_EXPARAM_205 s_compose_g_EXPARAM_207
      s_compose_x_209 (Random.int 0)
    +
    map
      set_flag_compose_216 s_compose_f_EXPARAM_205 s_compose_g_EXPARAM_207
      s_compose_x_209 (0 * xs + (0 * f_EXPARAM_127 + 0)) set_flag_compose_216
      s_compose_f_EXPARAM_205 s_compose_g_EXPARAM_207 s_compose_x_209 
      f set_flag_compose_216 s_compose_f_EXPARAM_205 s_compose_g_EXPARAM_207
      s_compose_x_209 (xs - 1)
let rec compose (x_DO_NOT_CARE_255:bool) (x_DO_NOT_CARE_256:int)
               (x_DO_NOT_CARE_257:int) (x_DO_NOT_CARE_258:int)
               (f_EXPARAM_123:int) (x_DO_NOT_CARE_251:bool)
               (x_DO_NOT_CARE_252:int) (x_DO_NOT_CARE_253:int)
               (x_DO_NOT_CARE_254:int)
               (f:(bool -> int -> int -> int -> int -> int))
               (x_DO_NOT_CARE_247:bool) (x_DO_NOT_CARE_248:int)
               (x_DO_NOT_CARE_249:int) (x_DO_NOT_CARE_250:int)
               (g_EXPARAM_124:int) (x_DO_NOT_CARE_243:bool)
               (x_DO_NOT_CARE_244:int) (x_DO_NOT_CARE_245:int)
               (x_DO_NOT_CARE_246:int)
               (g:(bool -> int -> int -> int -> int -> int))
               (prev_set_flag_compose_215:bool)
               (s_prev_compose_f_EXPARAM_210:int)
               (s_prev_compose_g_EXPARAM_212:int) (s_prev_compose_x_214:int)
               (x:int) =
  if prev_set_flag_compose_215 then assert false;
  compose_without_checking_241
    x_DO_NOT_CARE_255 x_DO_NOT_CARE_256 x_DO_NOT_CARE_257
    x_DO_NOT_CARE_258 f_EXPARAM_123 x_DO_NOT_CARE_251 x_DO_NOT_CARE_252
    x_DO_NOT_CARE_253 x_DO_NOT_CARE_254 f x_DO_NOT_CARE_247
    x_DO_NOT_CARE_248 x_DO_NOT_CARE_249 x_DO_NOT_CARE_250 g_EXPARAM_124
    x_DO_NOT_CARE_243 x_DO_NOT_CARE_244 x_DO_NOT_CARE_245
    x_DO_NOT_CARE_246 g prev_set_flag_compose_215
    s_prev_compose_f_EXPARAM_210 s_prev_compose_g_EXPARAM_212
    s_prev_compose_x_214 x
and compose_without_checking_241 (_:bool) (_:int) (_:int) (_:int)
                                (f_EXPARAM_123:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (f:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int)
                                (g_EXPARAM_124:int) (_:bool) (_:int)
                                (_:int) (_:int)
                                (g:(bool ->
                                      int -> int -> int -> int -> int))
                                (_:bool) (_:int) (_:int) (_:int) (x:int) =
  let set_flag_compose_216 = true
  in
  let s_compose_x_209 = x
  in
  let s_compose_g_EXPARAM_207 = g_EXPARAM_124
  in
  let s_compose_f_EXPARAM_205 = f_EXPARAM_123
  in
  f
    set_flag_compose_216 s_compose_f_EXPARAM_205 s_compose_g_EXPARAM_207
    s_compose_x_209
    (g
      set_flag_compose_216 s_compose_f_EXPARAM_205
      s_compose_g_EXPARAM_207 s_compose_x_209 x)
let add (_:bool) (_:int) (_:int) (_:int) (x:int) (_:bool) (_:int) (_:int)
       (_:int) (y:int) = x + y
let main (set_flag_compose_216:bool) (s_compose_f_EXPARAM_205:int)
        (s_compose_g_EXPARAM_207:int) (s_compose_x_209:int) (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_compose_216 s_compose_f_EXPARAM_205
      s_compose_g_EXPARAM_207 s_compose_x_209 (0 * l + (0 * l + 0))
      set_flag_compose_216 s_compose_f_EXPARAM_205
      s_compose_g_EXPARAM_207 s_compose_x_209
      (compose
        set_flag_compose_216 s_compose_f_EXPARAM_205
        s_compose_g_EXPARAM_207 s_compose_x_209 (0 * l + (0 * l + 0))
        set_flag_compose_216 s_compose_f_EXPARAM_205
        s_compose_g_EXPARAM_207 s_compose_x_209
        (add
          set_flag_compose_216 s_compose_f_EXPARAM_205
          s_compose_g_EXPARAM_207 s_compose_x_209 1)
        set_flag_compose_216 s_compose_f_EXPARAM_205
        s_compose_g_EXPARAM_207 s_compose_x_209 (0 * l + (0 * l + 0))
        set_flag_compose_216 s_compose_f_EXPARAM_205
        s_compose_g_EXPARAM_207 s_compose_x_209
        (add
          set_flag_compose_216 s_compose_f_EXPARAM_205
          s_compose_g_EXPARAM_207 s_compose_x_209 2))
      set_flag_compose_216 s_compose_f_EXPARAM_205
      s_compose_g_EXPARAM_207 s_compose_x_209 l
  else
    0
let u_15578 = main false 0 0 0 ()
