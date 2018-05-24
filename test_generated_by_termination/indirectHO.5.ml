let c1_COEFFICIENT_79 = 0
let c0_COEFFICIENT_78 = 0
let id (_:bool) (_:int) (x:unit) = x
let app (_:bool) (_:int) (_:int) (_:bool) (_:int)
       (h:(bool -> int -> unit -> bool -> int -> unit -> unit))
       (set_flag_f_178:bool) (s_f_n_173:int) (v:unit) =
  h set_flag_f_178 s_f_n_173 () set_flag_f_178 s_f_n_173 v
let rec f (x_DO_NOT_CARE_190:bool) (x_DO_NOT_CARE_191:int) (n:int)
         (prev_set_flag_f_177:bool) (s_prev_f_n_175:int) (u:unit) =
  if prev_set_flag_f_177 then assert false;
  f_without_checking_188
    x_DO_NOT_CARE_190 x_DO_NOT_CARE_191 n prev_set_flag_f_177
    s_prev_f_n_175 u
and f_without_checking_188 (_:bool) (_:int) (n:int) (_:bool) (_:int)
                          (():unit) =
  let set_flag_f_178 = true
  in
  let s_f_n_173 = n
  in
  if n > 0
  then
    app
      set_flag_f_178 s_f_n_173
      (c1_COEFFICIENT_79 * n + c0_COEFFICIENT_78) set_flag_f_178
      s_f_n_173 (f set_flag_f_178 s_f_n_173 (n - 1))
  else
    id
let main (set_flag_f_178:bool) (s_f_n_173:int) (():unit) =
  f_without_checking_188
    set_flag_f_178 s_f_n_173 (Random.int 0) set_flag_f_178 s_f_n_173 
    () set_flag_f_178 s_f_n_173 ()
let u_9061 = main false 0 ()
