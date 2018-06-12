let compose (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
           (_:int) (g:(bool -> int -> int -> int)) (set_flag_succ_295:bool)
           (s_succ_x_292:int) (x:int) =
  f set_flag_succ_295 s_succ_x_292 (g set_flag_succ_295 s_succ_x_292 x)
let id (_:bool) (_:int) (x:int) = x
let rec succ (prev_set_flag_succ_294:bool) (s_prev_succ_x_293:int) (x:int) =
  if prev_set_flag_succ_294 then assert false;
  succ_without_checking_315 prev_set_flag_succ_294 s_prev_succ_x_293 x
and succ_without_checking_315 (_:bool) (_:int) (x:int) =
  let set_flag_succ_295 = true
  in
  let s_succ_x_292 = x
  in
  x + 1
let rec toChurch (_:bool) (_:int) (n:int) (_:bool) (_:int)
                (f_EXPARAM_116:int) (set_flag_succ_295:bool)
                (s_succ_x_292:int) (f:(bool -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_succ_295 s_succ_x_292 (0 * f_EXPARAM_116 + (0 * n + 0))
      set_flag_succ_295 s_succ_x_292 f set_flag_succ_295 s_succ_x_292
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_succ_295 s_succ_x_292
      (toChurch
        set_flag_succ_295 s_succ_x_292 (n - 1) set_flag_succ_295
        s_succ_x_292 (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_succ_295
        s_succ_x_292 f)
let main (set_flag_succ_295:bool) (s_succ_x_292:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_succ_295 s_succ_x_292 x set_flag_succ_295 s_succ_x_292
         (0 * x + (0 * x + 0)) set_flag_succ_295 s_succ_x_292 succ
     in
     ())
let u_7794 = main false 0 ()
