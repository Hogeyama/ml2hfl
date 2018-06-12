let rec app (x_DO_NOT_CARE_211:bool) (x_DO_NOT_CARE_212:int)
           (x_DO_NOT_CARE_213:int) (f_EXPARAM_89:int)
           (x_DO_NOT_CARE_208:bool) (x_DO_NOT_CARE_209:int)
           (x_DO_NOT_CARE_210:int) (f:(bool -> int -> int -> int -> unit))
           (prev_set_flag_app_191:bool) (s_prev_app_f_EXPARAM_188:int)
           (s_prev_app_x_190:int) (x:int) =
  if prev_set_flag_app_191 then assert false;
  app_without_checking_206
    x_DO_NOT_CARE_211 x_DO_NOT_CARE_212 x_DO_NOT_CARE_213 f_EXPARAM_89
    x_DO_NOT_CARE_208 x_DO_NOT_CARE_209 x_DO_NOT_CARE_210 f
    prev_set_flag_app_191 s_prev_app_f_EXPARAM_188 s_prev_app_x_190 x
and app_without_checking_206 (_:bool) (_:int) (_:int) (f_EXPARAM_89:int)
                            (_:bool) (_:int) (_:int)
                            (f:(bool -> int -> int -> int -> unit))
                            (_:bool) (_:int) (_:int) (x:int) =
  let set_flag_app_192 = true
  in
  let s_app_x_187 = x
  in
  let s_app_f_EXPARAM_185 = f_EXPARAM_89
  in
  f set_flag_app_192 s_app_f_EXPARAM_185 s_app_x_187 x
and down (set_flag_app_192:bool) (s_app_f_EXPARAM_185:int)
        (s_app_x_187:int) (x:int) =
  if x = 0
  then
    ()
  else
    down set_flag_app_192 s_app_f_EXPARAM_185 s_app_x_187 (x - 1)
and up (set_flag_app_192:bool) (s_app_f_EXPARAM_185:int)
      (s_app_x_187:int) (x:int) =
  if x = 0
  then
    ()
  else
    up set_flag_app_192 s_app_f_EXPARAM_185 s_app_x_187 (x + 1)
let main (set_flag_app_192:bool) (s_app_f_EXPARAM_185:int)
        (s_app_x_187:int) (():unit) =
  let t1 = Random.int 0
  in
  let t2 = Random.int 0
  in
  if t1 > 0
  then
    app
      set_flag_app_192 s_app_f_EXPARAM_185 s_app_x_187
      (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0)))) set_flag_app_192
      s_app_f_EXPARAM_185 s_app_x_187 down set_flag_app_192
      s_app_f_EXPARAM_185 s_app_x_187 t1
  else
    (if t2 < 0
     then
       app_without_checking_206
         set_flag_app_192 s_app_f_EXPARAM_185 s_app_x_187
         (0 * t2 + (0 * t2 + (0 * t1 + (0 * t1 + 0)))) set_flag_app_192
         s_app_f_EXPARAM_185 s_app_x_187 up set_flag_app_192
         s_app_f_EXPARAM_185 s_app_x_187 t2)
let u_15884 = main false 0 0 ()
