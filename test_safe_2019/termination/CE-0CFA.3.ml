let rec f_65 (_:bool) (_:int) (x:int) = x
and omega (prev_set_flag_omega_162:bool) (s_prev_omega_x_161:int) (x:int) =
  if prev_set_flag_omega_162 then assert false;
  omega_without_checking_178 prev_set_flag_omega_162 s_prev_omega_x_161 x
and omega_without_checking_178 (_:bool) (_:int) (x:int) =
  let set_flag_omega_163 = true
  in
  let s_omega_x_160 = x
  in
  omega_without_checking_178 set_flag_omega_163 s_omega_x_160 x
and f_66 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_omega_163:bool)
        (s_omega_x_160:int) (z:int) =
  y set_flag_omega_163 s_omega_x_160 z
and u_63 =
  let id = f_65
  in
  let f = f_66
  in
  f
    false 0 0 false 0 (f false 0 0 false 0 id false 0 0 false 0 omega)
    false 0 0 false 0 id false 0 1
let u_5598 = ()
