let compose (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
           (_:int) (g:(bool -> int -> int -> int)) (set_flag_succ_289:bool)
           (s_succ_x_286:int) (x:int) =
  f set_flag_succ_289 s_succ_x_286 (g set_flag_succ_289 s_succ_x_286 x)
let id (_:bool) (_:int) (x:int) = x
let rec succ (prev_set_flag_succ_288:bool) (s_prev_succ_x_287:int) (x:int) =
  if prev_set_flag_succ_288 then assert false;
  succ_without_checking_309 prev_set_flag_succ_288 s_prev_succ_x_287 x
and succ_without_checking_309 (_:bool) (_:int) (x:int) =
  let set_flag_succ_289 = true
  in
  let s_succ_x_286 = x
  in
  x + 1
let rec toChurch (_:bool) (_:int) (n:int) (_:bool) (_:int)
                (f_EXPARAM_114:int) (set_flag_succ_289:bool)
                (s_succ_x_286:int) (f:(bool -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_succ_289 s_succ_x_286 (0 * f_EXPARAM_114 + (0 * n + 0))
      set_flag_succ_289 s_succ_x_286 f set_flag_succ_289 s_succ_x_286
      (0 * f_EXPARAM_114 + (0 * n + 0)) set_flag_succ_289 s_succ_x_286
      (toChurch
        set_flag_succ_289 s_succ_x_286 (n - 1) set_flag_succ_289
        s_succ_x_286 (0 * f_EXPARAM_114 + (0 * n + 0)) set_flag_succ_289
        s_succ_x_286 f)
let main (set_flag_succ_289:bool) (s_succ_x_286:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_succ_289 s_succ_x_286 x set_flag_succ_289 s_succ_x_286
         0 set_flag_succ_289 s_succ_x_286 succ
     in
     ())
let u_7736 = main false 0 ()
