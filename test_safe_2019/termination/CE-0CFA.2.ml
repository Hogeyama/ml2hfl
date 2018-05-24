let c7_COEFFICIENT_79 = 0
let c6_COEFFICIENT_78 = 0
let c5_COEFFICIENT_77 = 0
let c4_COEFFICIENT_76 = 0
let c3_COEFFICIENT_73 = 0
let c2_COEFFICIENT_72 = 0
let c1_COEFFICIENT_71 = 0
let c0_COEFFICIENT_70 = 0
let rec f_64 (_:bool) (_:int) (x:int) = x
and omega (prev_set_flag_omega_169:bool) (s_prev_omega_x_168:int) (x:int) =
  if prev_set_flag_omega_169 then assert false;
  omega_without_checking_185 prev_set_flag_omega_169 s_prev_omega_x_168 x
and omega_without_checking_185 (_:bool) (_:int) (x:int) =
  let set_flag_omega_170 = true
  in
  let s_omega_x_167 = x
  in
  omega set_flag_omega_170 s_omega_x_167 x
and f_65 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_omega_170:bool)
        (s_omega_x_167:int) (z:int) =
  y set_flag_omega_170 s_omega_x_167 z
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
      0 omega_without_checking_185)
    false 0 (c7_COEFFICIENT_79 * u_62 + c6_COEFFICIENT_78) false 0 id
    false 0 1
let u_4271 = ()
