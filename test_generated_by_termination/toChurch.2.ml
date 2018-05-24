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
           (_:int) (g:(bool -> int -> int -> int)) (set_flag_id_231:bool)
           (s_id_x_228:int) (x:int) =
  f set_flag_id_231 s_id_x_228 (g set_flag_id_231 s_id_x_228 x)
let rec id (prev_set_flag_id_230:bool) (s_prev_id_x_229:int) (x:int) =
  if prev_set_flag_id_230 then assert false;
  id_without_checking_257 prev_set_flag_id_230 s_prev_id_x_229 x
and id_without_checking_257 (_:bool) (_:int) (x:int) =
  let set_flag_id_231 = true
  in
  let s_id_x_228 = x
  in
  x
let succ (_:bool) (_:int) (x:int) = x + 1
let rec toChurch (_:bool) (_:int) (n:int) (_:bool) (_:int)
                (f_EXPARAM_115:int) (set_flag_id_231:bool)
                (s_id_x_228:int) (f:(bool -> int -> int -> int)) =
  if n = 0
  then
    id
  else
    compose
      set_flag_id_231 s_id_x_228
      (c5_COEFFICIENT_118 * f_EXPARAM_115 +
       (c4_COEFFICIENT_117 * n + c3_COEFFICIENT_116))
      set_flag_id_231 s_id_x_228 f set_flag_id_231 s_id_x_228
      (c11_COEFFICIENT_125 * f_EXPARAM_115 +
       (c10_COEFFICIENT_124 * n + c9_COEFFICIENT_123))
      set_flag_id_231 s_id_x_228
      (toChurch
        set_flag_id_231 s_id_x_228 (n - 1) set_flag_id_231 s_id_x_228
        (c8_COEFFICIENT_121 * f_EXPARAM_115 +
         (c7_COEFFICIENT_120 * n + c6_COEFFICIENT_119))
        set_flag_id_231 s_id_x_228 f)
let main (set_flag_id_231:bool) (s_id_x_228:int) (():unit) =
  let x = Random.int 0
  in
  (if x >= 0
   then
     let tos =
       toChurch
         set_flag_id_231 s_id_x_228 x set_flag_id_231 s_id_x_228
         (c2_COEFFICIENT_113 * x +
          (c1_COEFFICIENT_112 * x + c0_COEFFICIENT_111))
         set_flag_id_231 s_id_x_228 succ
     in
     ())
let u_5641 = main false 0 ()
