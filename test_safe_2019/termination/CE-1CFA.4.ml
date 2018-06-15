let rec f_65 (prev_set_flag_f_207:bool) (s_prev_f_x_206:int) (x:int) =
  if prev_set_flag_f_207 then assert false;
  f_without_checking_217 prev_set_flag_f_207 s_prev_f_x_206 x
and f_without_checking_217 (_:bool) (_:int) (x:int) =
  let set_flag_f_208 = true
  in
  let s_f_x_205 = x
  in
  x
and omega (set_flag_f_208:bool) (s_f_x_205:int) (x:int) =
  omega set_flag_f_208 s_f_x_205 x
and f_66 (_:bool) (_:int) (_:int) (_:bool) (_:int)
        (_:(bool -> int -> int -> int)) (_:bool) (_:int) (_:int) (_:bool)
        (_:int) (y:(bool -> int -> int -> int)) (set_flag_f_208:bool)
        (s_f_x_205:int) (z:int) = y set_flag_f_208 s_f_x_205 z
and u_63 =
  let id = f_65
  in
  let f = f_66
  in
  f
    false 0 0 false 0 (f false 0 0 false 0 id false 0 0 false 0 omega)
    false 0 0 false 0 id false 0 1
let u_7004 = ()
