let rec map (_:bool) (_:int) (_:int) (f_EXPARAM_127:int) (_:bool) (_:int)
           (_:int) (f:(bool -> int -> int -> int -> int))
           (set_flag_add_300:bool) (s_add_x_295:int) (s_add_y_296:int)
           (xs:int) =
  if xs = 0
  then
    0
  else
    f set_flag_add_300 s_add_x_295 s_add_y_296 (Random.int 0) +
    map
      set_flag_add_300 s_add_x_295 s_add_y_296
      (0 * xs + (0 * f_EXPARAM_127 + 0)) set_flag_add_300 s_add_x_295
      s_add_y_296 f set_flag_add_300 s_add_x_295 s_add_y_296 (xs - 1)
let compose (_:bool) (_:int) (_:int) (_:int) (_:bool) (_:int) (_:int)
           (f:(bool -> int -> int -> int -> int)) (_:bool) (_:int) (_:int)
           (_:int) (_:bool) (_:int) (_:int)
           (g:(bool -> int -> int -> int -> int)) (set_flag_add_300:bool)
           (s_add_x_295:int) (s_add_y_296:int) (x:int) =
  f
    set_flag_add_300 s_add_x_295 s_add_y_296
    (g set_flag_add_300 s_add_x_295 s_add_y_296 x)
let rec add (x_DO_NOT_CARE_313:bool) (x_DO_NOT_CARE_314:int)
           (x_DO_NOT_CARE_315:int) (x:int) (prev_set_flag_add_299:bool)
           (s_prev_add_x_297:int) (s_prev_add_y_298:int) (y:int) =
  if prev_set_flag_add_299 then assert false;
  add_without_checking_311
    x_DO_NOT_CARE_313 x_DO_NOT_CARE_314 x_DO_NOT_CARE_315 x
    prev_set_flag_add_299 s_prev_add_x_297 s_prev_add_y_298 y
and add_without_checking_311 (_:bool) (_:int) (_:int) (x:int) (_:bool)
                            (_:int) (_:int) (y:int) =
  let set_flag_add_300 = true
  in
  let s_add_y_296 = y
  in
  let s_add_x_295 = x
  in
  x + y
let main (set_flag_add_300:bool) (s_add_x_295:int) (s_add_y_296:int)
        (():unit) =
  let l = Random.int 0
  in
  if l >= 0
  then
    map
      set_flag_add_300 s_add_x_295 s_add_y_296 (0 * l + (0 * l + 0))
      set_flag_add_300 s_add_x_295 s_add_y_296
      (compose
        set_flag_add_300 s_add_x_295 s_add_y_296 (0 * l + (0 * l + 0))
        set_flag_add_300 s_add_x_295 s_add_y_296
        (add_without_checking_311
          set_flag_add_300 s_add_x_295 s_add_y_296 1)
        set_flag_add_300 s_add_x_295 s_add_y_296 (0 * l + (0 * l + 0))
        set_flag_add_300 s_add_x_295 s_add_y_296
        (add set_flag_add_300 s_add_x_295 s_add_y_296 2))
      set_flag_add_300 s_add_x_295 s_add_y_296 l
  else
    0
let u_24421 = main false 0 0 ()
