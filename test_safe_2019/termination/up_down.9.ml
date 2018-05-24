let c9_COEFFICIENT_86 = 0
let c8_COEFFICIENT_85 = 0
let c7_COEFFICIENT_84 = 0
let c6_COEFFICIENT_83 = 0
let c5_COEFFICIENT_82 = 0
let c4_COEFFICIENT_80 = 0
let c3_COEFFICIENT_79 = 0
let c2_COEFFICIENT_78 = 0
let c1_COEFFICIENT_77 = 0
let c0_COEFFICIENT_76 = 0
let rec app (x_DO_NOT_CARE_210:bool) (x_DO_NOT_CARE_211:int)
           (x_DO_NOT_CARE_212:int) (f_EXPARAM_88:int)
           (x_DO_NOT_CARE_207:bool) (x_DO_NOT_CARE_208:int)
           (x_DO_NOT_CARE_209:int) (f:(bool -> int -> int -> int -> unit))
           (prev_set_flag_app_190:bool) (s_prev_app_f_EXPARAM_187:int)
           (s_prev_app_x_189:int) (x:int) =
  if prev_set_flag_app_190 then assert false;
  app_without_checking_205
    x_DO_NOT_CARE_210 x_DO_NOT_CARE_211 x_DO_NOT_CARE_212 f_EXPARAM_88
    x_DO_NOT_CARE_207 x_DO_NOT_CARE_208 x_DO_NOT_CARE_209 f
    prev_set_flag_app_190 s_prev_app_f_EXPARAM_187 s_prev_app_x_189 x
and app_without_checking_205 (_:bool) (_:int) (_:int) (f_EXPARAM_88:int)
                            (_:bool) (_:int) (_:int)
                            (f:(bool -> int -> int -> int -> unit))
                            (_:bool) (_:int) (_:int) (x:int) =
  let set_flag_app_191 = true
  in
  let s_app_x_186 = x
  in
  let s_app_f_EXPARAM_184 = f_EXPARAM_88
  in
  f set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 x
and down (set_flag_app_191:bool) (s_app_f_EXPARAM_184:int)
        (s_app_x_186:int) (x:int) =
  if x = 0
  then
    ()
  else
    down set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 (x - 1)
and up (set_flag_app_191:bool) (s_app_f_EXPARAM_184:int)
      (s_app_x_186:int) (x:int) =
  if x = 0
  then
    ()
  else
    up set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 (x + 1)
let main (set_flag_app_191:bool) (s_app_f_EXPARAM_184:int)
        (s_app_x_186:int) (():unit) =
  let t1 = Random.int 0
  in
  let t2 = Random.int 0
  in
  if t1 > 0
  then
    app_without_checking_205
      set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186
      (c9_COEFFICIENT_86 * t2 +
       (c8_COEFFICIENT_85 * t2 +
        (c7_COEFFICIENT_84 * t1 +
         (c6_COEFFICIENT_83 * t1 + c5_COEFFICIENT_82))))
      set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 down
      set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 t1
  else
    (if t2 < 0
     then
       app
         set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186
         (c4_COEFFICIENT_80 * t2 +
          (c3_COEFFICIENT_79 * t2 +
           (c2_COEFFICIENT_78 * t1 +
            (c1_COEFFICIENT_77 * t1 + c0_COEFFICIENT_76))))
         set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 up
         set_flag_app_191 s_app_f_EXPARAM_184 s_app_x_186 t2)
let u_13758 = main false 0 0 ()
