let c5_COEFFICIENT_115 = 0
let c4_COEFFICIENT_114 = 0
let c3_COEFFICIENT_113 = 0
let c2_COEFFICIENT_112 = 0
let c1_COEFFICIENT_109 = 0
let c0_COEFFICIENT_108 = 0
let rec app (_:bool) (i:int) (_:bool) (h_EXPARAM_111:int) (_:bool)
           (h:(bool -> int -> bool -> unit -> unit)) (_:bool) (v:int)
           (set_flag_g_198:bool) (u:unit) =
  if i >= 0
  then
    app
      set_flag_g_198 (i - 1) set_flag_g_198
      (c5_COEFFICIENT_115 * v +
       (c4_COEFFICIENT_114 * h_EXPARAM_111 +
        (c3_COEFFICIENT_113 * i + c2_COEFFICIENT_112)))
      set_flag_g_198 h set_flag_g_198 v set_flag_g_198 u
  else
    h set_flag_g_198 v set_flag_g_198 u
let rec g (prev_set_flag_g_197:bool) (u:unit) =
  if prev_set_flag_g_197 then assert false;
  g_without_checking_212 prev_set_flag_g_197 u
and g_without_checking_212 (_:bool) (():unit) =
  let set_flag_g_198 = true
  in
  ()
let rec f (set_flag_g_198:bool) (x:int) =
  if x > 0
  then
    app
      set_flag_g_198 (Random.int 0) set_flag_g_198
      (c1_COEFFICIENT_109 * x + c0_COEFFICIENT_108) set_flag_g_198 f
      set_flag_g_198 (x - 1)
  else
    g
let main (set_flag_g_198:bool) (():unit) =
  f set_flag_g_198 (Random.int 0) set_flag_g_198 ()
let u_42239 = main false ()
