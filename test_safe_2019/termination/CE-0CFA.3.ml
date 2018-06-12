let rec f_65 (_:bool) (_:int) (x:int) = x
and omega (prev_set_flag_omega_170:bool) (s_prev_omega_x_169:int) (x:int) =
  if prev_set_flag_omega_170 then assert false;
  omega_without_checking_186 prev_set_flag_omega_170 s_prev_omega_x_169 x
and omega_without_checking_186 (_:bool) (_:int) (x:int) =
  let set_flag_omega_171 = true
  in
  let s_omega_x_168 = x
  in
  omega_without_checking_186 set_flag_omega_171 s_omega_x_168 x
and f_66 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_omega_171:bool)
        (s_omega_x_168:int) (z:int) =
  y set_flag_omega_171 s_omega_x_168 z
and u_63 =
  let id = f_65
  in
  let f = f_66
  in
  f
    false 0 (0 * u_63 + 0) false 0
    (f
      false 0 (0 * u_63 + 0) false 0 id false 0 (0 * u_63 + 0) false 
      0 omega)
    false 0 (0 * u_63 + 0) false 0 id false 0 1
let u_5708 = ()
