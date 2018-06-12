let compose (_:bool) (_:int) (_:int) (_:bool) (_:int)
           (f:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
           (_:int) (g:(bool -> int -> int -> int)) (set_flag_id_232:bool)
           (s_id_x_229:int) (x:int) =
  f set_flag_id_232 s_id_x_229 (g set_flag_id_232 s_id_x_229 x)
let rec id (prev_set_flag_id_231:bool) (s_prev_id_x_230:int) (x:int) =
  if prev_set_flag_id_231 then assert false;
  id_without_checking_258 prev_set_flag_id_231 s_prev_id_x_230 x
and id_without_checking_258 (_:bool) (_:int) (x:int) =
  let set_flag_id_232 = true
  in
  let s_id_x_229 = x
  in
  x
let succ (_:bool) (_:int) (x:int) = x + 1
let rec toChurch (_:bool) (_:int) (n:int) (_:bool) (_:int)
                (f_EXPARAM_116:int) (set_flag_id_232:bool)
                (s_id_x_229:int) (f:(bool -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_id_232 s_id_x_229 (0 * f_EXPARAM_116 + (0 * n + 0))
      set_flag_id_232 s_id_x_229 f set_flag_id_232 s_id_x_229
      (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_id_232 s_id_x_229
      (toChurch
        set_flag_id_232 s_id_x_229 (n - 1) set_flag_id_232 s_id_x_229
        (0 * f_EXPARAM_116 + (0 * n + 0)) set_flag_id_232 s_id_x_229 
        f)
let main (set_flag_id_232:bool) (s_id_x_229:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_id_232 s_id_x_229 x set_flag_id_232 s_id_x_229
         (0 * x + (0 * x + 0)) set_flag_id_232 s_id_x_229 succ
     in
     ())
let u_5642 = main false 0 ()
