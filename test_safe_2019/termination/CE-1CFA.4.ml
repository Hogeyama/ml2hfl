let rec f_65 (prev_set_flag_f_219:bool) (s_prev_f_x_218:int) (x:int) =
  if prev_set_flag_f_219 then assert false;
  f_without_checking_229 prev_set_flag_f_219 s_prev_f_x_218 x
and f_without_checking_229 (_:bool) (_:int) (x:int) =
  let set_flag_f_220 = true
  in
  let s_f_x_217 = x
  in
  x
and omega (set_flag_f_220:bool) (s_f_x_217:int) (x:int) =
  omega set_flag_f_220 s_f_x_217 x
and f_66 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_f_220:bool)
        (s_f_x_217:int) (z:int) = y set_flag_f_220 s_f_x_217 z
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
let u_7145 = ()
