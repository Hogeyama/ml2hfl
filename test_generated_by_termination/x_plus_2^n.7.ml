let c1_COEFFICIENT_82 = 0
let c0_COEFFICIENT_81 = 0
let succ (_:bool) (_:int) (n:int) = n + 1
let g (_:bool) (_:int) (_:int) (_:bool) (_:int)
     (r:(bool -> int -> int -> int)) (set_flag_f_177:bool) (s_f_n_174:int)
     (a:int) = r set_flag_f_177 s_f_n_174 (r set_flag_f_177 s_f_n_174 a)
let rec f (prev_set_flag_f_176:bool) (s_prev_f_n_175:int) (n:int) =
  if prev_set_flag_f_176
  then
    if s_prev_f_n_175 > n && n >= 0 then () else assert false;
  f_without_checking_185 prev_set_flag_f_176 s_prev_f_n_175 n
and f_without_checking_185 (_:bool) (_:int) (n:int) =
  let set_flag_f_177 = true
  in
  let s_f_n_174 = n
  in
  if n = 0
  then
    succ
  else
    g
      set_flag_f_177 s_f_n_174
      (c1_COEFFICIENT_82 * n + c0_COEFFICIENT_81) set_flag_f_177
      s_f_n_174 (f set_flag_f_177 s_f_n_174 (n - 1))
let main (_:bool) (_:int) (n:int) (set_flag_f_177:bool)
        (s_f_n_174:int) (x:int) =
  if n >= 0 && x >= 0
  then
    f_without_checking_185
      set_flag_f_177 s_f_n_174 n set_flag_f_177 s_f_n_174 x
  else
    0
let u_13921 = main false 0 (Random.int 0) false 0 (Random.int 0)
