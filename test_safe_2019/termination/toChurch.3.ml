let c11_COEFFICIENT_125 = 0
let c10_COEFFICIENT_124 = 0
let c9_COEFFICIENT_123 = 0
let c8_COEFFICIENT_121 = 0
let c7_COEFFICIENT_120 = 0
let c6_COEFFICIENT_119 = 0
let c5_COEFFICIENT_118 = 0
let c4_COEFFICIENT_117 = 0
let c3_COEFFICIENT_116 = 0
let c2_COEFFICIENT_113 = 0
let c1_COEFFICIENT_112 = 0
let c0_COEFFICIENT_111 = 0
let compose (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
           (_:int) (g:(bool -> int -> int -> int)) (set_flag_succ_294:bool)
           (s_succ_x_291:int) (x:int) =
  f set_flag_succ_294 s_succ_x_291 (g set_flag_succ_294 s_succ_x_291 x)
let id (_:bool) (_:int) (x:int) = x
let rec succ (prev_set_flag_succ_293:bool) (s_prev_succ_x_292:int) (x:int) =
  if prev_set_flag_succ_293 then assert false;
  succ_without_checking_314 prev_set_flag_succ_293 s_prev_succ_x_292 x
and succ_without_checking_314 (_:bool) (_:int) (x:int) =
  let set_flag_succ_294 = true
  in
  let s_succ_x_291 = x
  in
  x + 1
let rec toChurch (_:bool) (_:int) (n:int) (_:bool) (_:int)
                (f_EXPARAM_115:int) (set_flag_succ_294:bool)
                (s_succ_x_291:int) (f:(bool -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_succ_294 s_succ_x_291
      (c5_COEFFICIENT_118 * f_EXPARAM_115 +
       (c4_COEFFICIENT_117 * n + c3_COEFFICIENT_116))
      set_flag_succ_294 s_succ_x_291 f set_flag_succ_294 s_succ_x_291
      (c11_COEFFICIENT_125 * f_EXPARAM_115 +
       (c10_COEFFICIENT_124 * n + c9_COEFFICIENT_123))
      set_flag_succ_294 s_succ_x_291
      (toChurch
        set_flag_succ_294 s_succ_x_291 (n - 1) set_flag_succ_294
        s_succ_x_291
        (c8_COEFFICIENT_121 * f_EXPARAM_115 +
         (c7_COEFFICIENT_120 * n + c6_COEFFICIENT_119))
        set_flag_succ_294 s_succ_x_291 f)
let main (set_flag_succ_294:bool) (s_succ_x_291:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_succ_294 s_succ_x_291 x set_flag_succ_294 s_succ_x_291
         (c2_COEFFICIENT_113 * x +
          (c1_COEFFICIENT_112 * x + c0_COEFFICIENT_111))
         set_flag_succ_294 s_succ_x_291 succ
     in
     ())
let u_7793 = main false 0 ()
