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
let rec map (_:bool) (_:int) (_:int) (f_EXPARAM_126:int) (_:bool) (_:int)
           (_:int) (f:(bool -> int -> int -> int -> int))
           (set_flag_add_299:bool) (s_add_x_294:int) (s_add_y_295:int)
           (xs:int) =
  if xs = 0
  then
    0
  else
    f set_flag_add_299 s_add_x_294 s_add_y_295 (Random.int 0) +
    map
      set_flag_add_299 s_add_x_294 s_add_y_295
      (c11_COEFFICIENT_129 * xs +
       (c10_COEFFICIENT_128 * f_EXPARAM_126 + c9_COEFFICIENT_127))
      set_flag_add_299 s_add_x_294 s_add_y_295 f set_flag_add_299 s_add_x_294
      s_add_y_295 (xs - 1)
let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int) (_:int)
           (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int)) (set_flag_add_299:bool)
           (s_add_x_294:int) (s_add_y_295:int) (x:int) =
  f
    set_flag_add_299 s_add_x_294 s_add_y_295
    (g set_flag_add_299 s_add_x_294 s_add_y_295 x)
let rec add (x_DO_NOT_CARE_312:bool) (x_DO_NOT_CARE_313:int)
           (x_DO_NOT_CARE_314:int) (x:int) (prev_set_flag_add_298:bool)
           (s_prev_add_x_296:int) (s_prev_add_y_297:int) (y:int) =
  if prev_set_flag_add_298 then assert false;
  add_without_checking_310
    x_DO_NOT_CARE_312 x_DO_NOT_CARE_313 x_DO_NOT_CARE_314 x
    prev_set_flag_add_298 s_prev_add_x_296 s_prev_add_y_297 y
and add_without_checking_310 (_:bool) (_:int) (_:int) (x:int) (_:bool)
                            (_:int) (_:int) (y:int) =
  let set_flag_add_299 = true
  in
  let s_add_y_295 = y
  in
  let s_add_x_294 = x
  in
  x + y
let main (set_flag_add_299:bool) (s_add_x_294:int) (s_add_y_295:int)
        (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_add_299 s_add_x_294 s_add_y_295
      (c8_COEFFICIENT_120 * l +
       (c7_COEFFICIENT_119 * l + c6_COEFFICIENT_118))
      set_flag_add_299 s_add_x_294 s_add_y_295
      (compose
        set_flag_add_299 s_add_x_294 s_add_y_295
        (c2_COEFFICIENT_112 * l +
         (c1_COEFFICIENT_111 * l + c0_COEFFICIENT_110))
        set_flag_add_299 s_add_x_294 s_add_y_295
        (add set_flag_add_299 s_add_x_294 s_add_y_295 1) set_flag_add_299
        s_add_x_294 s_add_y_295
        (c5_COEFFICIENT_115 * l +
         (c4_COEFFICIENT_114 * l + c3_COEFFICIENT_113))
        set_flag_add_299 s_add_x_294 s_add_y_295
        (add_without_checking_310
          set_flag_add_299 s_add_x_294 s_add_y_295 2))
      set_flag_add_299 s_add_x_294 s_add_y_295 l
  else
    0
let u_20591 = main false 0 0 ()
