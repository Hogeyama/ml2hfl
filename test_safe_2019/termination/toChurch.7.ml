let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int) (_:int)
           (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int))
           (set_flag_toChurch_362:bool) (s_toChurch_n_355:int)
           (s_toChurch_f_EXPARAM_356:int) (x:int) =
  f
    set_flag_toChurch_362 s_toChurch_n_355 s_toChurch_f_EXPARAM_356
    (g set_flag_toChurch_362 s_toChurch_n_355 s_toChurch_f_EXPARAM_356 x)
let id (_:bool) (_:int) (_:int) (x:int) = x
let succ (_:bool) (_:int) (_:int) (x:int) = x + 1
let rec toChurch (x_DO_NOT_CARE_381:bool) (x_DO_NOT_CARE_382:int)
                (x_DO_NOT_CARE_383:int) (n:int) (x_DO_NOT_CARE_378:bool)
                (x_DO_NOT_CARE_379:int) (x_DO_NOT_CARE_380:int)
                (f_EXPARAM_116:int) (prev_set_flag_toChurch_361:bool)
                (s_prev_toChurch_n_358:int)
                (s_prev_toChurch_f_EXPARAM_359:int)
                (f:(bool -> int -> int -> int -> int)) =
  if prev_set_flag_toChurch_361
  then
    if s_prev_toChurch_n_358 > n && n >= 0 then () else assert false;
  toChurch_without_checking_376
    x_DO_NOT_CARE_381 x_DO_NOT_CARE_382 x_DO_NOT_CARE_383 n
    x_DO_NOT_CARE_378 x_DO_NOT_CARE_379 x_DO_NOT_CARE_380
    f_EXPARAM_116 prev_set_flag_toChurch_361 s_prev_toChurch_n_358
    s_prev_toChurch_f_EXPARAM_359 f
and toChurch_without_checking_376 (_:bool) (_:int) (_:int) (n:int)
                                 (_:bool) (_:int) (_:int)
                                 (f_EXPARAM_116:int) (_:bool) (_:int)
                                 (_:int)
                                 (f:(bool -> int -> int -> int -> int)) =
  let set_flag_toChurch_362 = true
  in
  let s_toChurch_f_EXPARAM_356 = f_EXPARAM_116
  in
  let s_toChurch_n_355 = n
  in
  if n = 0
  then
    id
  else
    compose
      set_flag_toChurch_362 s_toChurch_n_355 s_toChurch_f_EXPARAM_356
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_toChurch_362
      s_toChurch_n_355 s_toChurch_f_EXPARAM_356 f
      set_flag_toChurch_362 s_toChurch_n_355 s_toChurch_f_EXPARAM_356
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_toChurch_362
      s_toChurch_n_355 s_toChurch_f_EXPARAM_356
      (toChurch
        set_flag_toChurch_362 s_toChurch_n_355
        s_toChurch_f_EXPARAM_356 (n - 1) set_flag_toChurch_362
        s_toChurch_n_355 s_toChurch_f_EXPARAM_356
        (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_toChurch_362
        s_toChurch_n_355 s_toChurch_f_EXPARAM_356 f)
let main (set_flag_toChurch_362:bool) (s_toChurch_n_355:int)
        (s_toChurch_f_EXPARAM_356:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch_without_checking_376
         set_flag_toChurch_362 s_toChurch_n_355
         s_toChurch_f_EXPARAM_356 x set_flag_toChurch_362
         s_toChurch_n_355 s_toChurch_f_EXPARAM_356
         (0 * x + (0 * x + 0)) set_flag_toChurch_362 s_toChurch_n_355
         s_toChurch_f_EXPARAM_356 succ
     in
     ())
let u_20224 = main false 0 0 ()
