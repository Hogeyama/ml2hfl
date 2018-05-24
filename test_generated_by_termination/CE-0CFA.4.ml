let c7_COEFFICIENT_79 = 0
let c6_COEFFICIENT_78 = 0
let c5_COEFFICIENT_77 = 0
let c4_COEFFICIENT_76 = 0
let c3_COEFFICIENT_73 = 0
let c2_COEFFICIENT_72 = 0
let c1_COEFFICIENT_71 = 0
let c0_COEFFICIENT_70 = 0
let rec f_64 (prev_set_flag_f_218:bool) (s_prev_f_x_217:int) (x:int) =
  if prev_set_flag_f_218 then assert false;
  f_without_checking_228 prev_set_flag_f_218 s_prev_f_x_217 x
and f_without_checking_228 (_:bool) (_:int) (x:int) =
  let set_flag_f_219 = true
  in
  let s_f_x_216 = x
  in
  x
and omega (set_flag_f_219:bool) (s_f_x_216:int) (x:int) =
  omega set_flag_f_219 s_f_x_216 x
and f_65 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_f_219:bool)
        (s_f_x_216:int) (z:int) = y set_flag_f_219 s_f_x_216 z
and u_62 =
  let id = f_64
  in
  let f = f_65
  in
  f
    false 0 (c5_COEFFICIENT_77 * u_62 + c4_COEFFICIENT_76) false 0
    (f
      false 0 (c1_COEFFICIENT_71 * u_62 + c0_COEFFICIENT_70) false 0 
      id false 0 (c3_COEFFICIENT_73 * u_62 + c2_COEFFICIENT_72) false 
      0 omega)
    false 0 (c7_COEFFICIENT_79 * u_62 + c6_COEFFICIENT_78) false 0 id
    false 0 1
let u_7144 = ()
