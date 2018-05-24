let c1_COEFFICIENT_75 = 0
let c0_COEFFICIENT_74 = 0
let id (_:bool) (_:int) (x:unit) = x
let app (_:bool) (_:int) (_:int) (_:bool) (_:int)
       (h:(bool -> int -> int -> bool -> int -> unit -> unit)) (_:bool)
       (_:int) (v:int) (set_flag_f_181:bool) (s_f_x_178:int) (u:unit) =
  h set_flag_f_181 s_f_x_178 v set_flag_f_181 s_f_x_178 u
let rec f (prev_set_flag_f_180:bool) (s_prev_f_x_179:int) (x:int) =
  if prev_set_flag_f_180 then assert false;
  f_without_checking_189 prev_set_flag_f_180 s_prev_f_x_179 x
and f_without_checking_189 (_:bool) (_:int) (x:int) =
  let set_flag_f_181 = true
  in
  let s_f_x_178 = x
  in
  if x > 0
  then
    app
      set_flag_f_181 s_f_x_178
      (c1_COEFFICIENT_75 * x + c0_COEFFICIENT_74) set_flag_f_181
      s_f_x_178 f set_flag_f_181 s_f_x_178 (x - 1)
  else
    id
let main (set_flag_f_181:bool) (s_f_x_178:int) (():unit) =
  f_without_checking_189
    set_flag_f_181 s_f_x_178 (Random.int 0) set_flag_f_181 s_f_x_178 ()
let u_10266 = main false 0 ()
