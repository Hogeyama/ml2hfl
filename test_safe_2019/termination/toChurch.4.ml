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
let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int) (_:int)
           (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int))
           (set_flag_toChurch_361:bool) (s_toChurch_n_354:int)
           (s_toChurch_f_EXPARAM_355:int) (x:int) =
  f
    set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
    (g set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355 x)
let id (_:bool) (_:int) (_:int) (x:int) = x
let succ (_:bool) (_:int) (_:int) (x:int) = x + 1
let rec toChurch (x_DO_NOT_CARE_380:bool) (x_DO_NOT_CARE_381:int)
                (x_DO_NOT_CARE_382:int) (n:int) (x_DO_NOT_CARE_377:bool)
                (x_DO_NOT_CARE_378:int) (x_DO_NOT_CARE_379:int)
                (f_EXPARAM_115:int) (prev_set_flag_toChurch_360:bool)
                (s_prev_toChurch_n_357:int)
                (s_prev_toChurch_f_EXPARAM_358:int)
                (f:(bool -> int -> int -> int -> int)) =
  if prev_set_flag_toChurch_360 then assert false;
  toChurch_without_checking_375
    x_DO_NOT_CARE_380 x_DO_NOT_CARE_381 x_DO_NOT_CARE_382 n
    x_DO_NOT_CARE_377 x_DO_NOT_CARE_378 x_DO_NOT_CARE_379 f_EXPARAM_115
    prev_set_flag_toChurch_360 s_prev_toChurch_n_357
    s_prev_toChurch_f_EXPARAM_358 f
and toChurch_without_checking_375 (_:bool) (_:int) (_:int) (n:int)
                                 (_:bool) (_:int) (_:int)
                                 (f_EXPARAM_115:int) (_:bool) (_:int)
                                 (_:int)
                                 (f:(bool -> int -> int -> int -> int)) =
  let set_flag_toChurch_361 = true
  in
  let s_toChurch_f_EXPARAM_355 = f_EXPARAM_115
  in
  let s_toChurch_n_354 = n
  in
  if n = 0
  then
    id
  else
    compose
      set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
      (c5_COEFFICIENT_118 * f_EXPARAM_115 +
       (c4_COEFFICIENT_117 * n + c3_COEFFICIENT_116))
      set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355 
      f set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
      (c11_COEFFICIENT_125 * f_EXPARAM_115 +
       (c10_COEFFICIENT_124 * n + c9_COEFFICIENT_123))
      set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
      (toChurch_without_checking_375
        set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
        (n - 1) set_flag_toChurch_361 s_toChurch_n_354
        s_toChurch_f_EXPARAM_355
        (c8_COEFFICIENT_121 * f_EXPARAM_115 +
         (c7_COEFFICIENT_120 * n + c6_COEFFICIENT_119))
        set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355 
        f)
let main (set_flag_toChurch_361:bool) (s_toChurch_n_354:int)
        (s_toChurch_f_EXPARAM_355:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
         x set_flag_toChurch_361 s_toChurch_n_354
         s_toChurch_f_EXPARAM_355
         (c2_COEFFICIENT_113 * x +
          (c1_COEFFICIENT_112 * x + c0_COEFFICIENT_111))
         set_flag_toChurch_361 s_toChurch_n_354 s_toChurch_f_EXPARAM_355
         succ
     in
     ())
let u_10487 = main false 0 0 ()
