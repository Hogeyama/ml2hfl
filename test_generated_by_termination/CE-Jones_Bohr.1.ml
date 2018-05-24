let c9_COEFFICIENT_274 = 0
let c8_COEFFICIENT_273 = 0
let c7_COEFFICIENT_272 = 0
let c6_COEFFICIENT_261 = 0
let c5_COEFFICIENT_260 = 0
let c4_COEFFICIENT_255 = 0
let c3_COEFFICIENT_254 = 0
let c2_COEFFICIENT_245 = 0
let c1_COEFFICIENT_244 = 0
let c0_COEFFICIENT_238 = 0
let rec f1 (x_DO_NOT_CARE_355:bool) (x_DO_NOT_CARE_356:int) (u:unit)
          (x_DO_NOT_CARE_353:bool) (x_DO_NOT_CARE_354:int)
          (c_EXPARAM_281:int) (x_DO_NOT_CARE_351:bool)
          (x_DO_NOT_CARE_352:int) (c:(bool -> int -> unit -> unit))
          (prev_set_flag_f1_291:bool) (s_prev_f1_c_EXPARAM_288:int) (
          d:unit) =
  if prev_set_flag_f1_291 then assert false;
  f1_without_checking_349
    x_DO_NOT_CARE_355 x_DO_NOT_CARE_356 u x_DO_NOT_CARE_353
    x_DO_NOT_CARE_354 c_EXPARAM_281 x_DO_NOT_CARE_351 x_DO_NOT_CARE_352 
    c prev_set_flag_f1_291 s_prev_f1_c_EXPARAM_288 d
and f1_without_checking_349 (_:bool) (_:int) (():unit) (_:bool) (_:int)
                           (c_EXPARAM_281:int) (_:bool) (_:int)
                           (_:(bool -> int -> unit -> unit)) (_:bool)
                           (_:int) (d:unit) =
  let set_flag_f1_292 = true
  in
  let s_f1_c_EXPARAM_284 = c_EXPARAM_281
  in
  d
let f2 (_:bool) (_:int) (u:unit) (_:bool) (_:int) (a_EXPARAM_267:int)
      (_:bool) (_:int)
      (a:(bool ->
            int ->
              int ->
                bool ->
                  int ->
                    (bool ->
                       int ->
                         int ->
                           bool ->
                             int ->
                               (bool -> int -> unit -> unit) ->
                                 bool -> int -> unit -> unit) ->
                      bool -> int -> unit -> unit))
      (_:bool) (_:int) (b_EXPARAM_270:int) (set_flag_f1_292:bool)
      (s_f1_c_EXPARAM_284:int) (_:(bool -> int -> unit -> unit)) =
  a
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (c9_COEFFICIENT_274 * b_EXPARAM_270 +
     (c8_COEFFICIENT_273 * a_EXPARAM_267 + c7_COEFFICIENT_272))
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (f1 set_flag_f1_292 s_f1_c_EXPARAM_284 u)
let f3 (_:bool) (_:int) (u:unit) (_:bool) (_:int) (a_EXPARAM_249:int)
      (set_flag_f1_292:bool) (s_f1_c_EXPARAM_284:int)
      (a:(bool ->
            int ->
              int ->
                bool ->
                  int ->
                    (bool ->
                       int ->
                         int ->
                           bool ->
                             int ->
                               (bool -> int -> unit -> unit) ->
                                 bool -> int -> unit -> unit) ->
                      bool -> int -> unit -> unit)) =
  a
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (c6_COEFFICIENT_261 * a_EXPARAM_249 + c5_COEFFICIENT_260)
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (f2
      set_flag_f1_292 s_f1_c_EXPARAM_284 u set_flag_f1_292
      s_f1_c_EXPARAM_284
      (c4_COEFFICIENT_255 * a_EXPARAM_249 + c3_COEFFICIENT_254)
      set_flag_f1_292 s_f1_c_EXPARAM_284 a)
let f4 (_:bool) (_:int) (():unit) (_:bool) (_:int) (v:unit) = v
let f5 (_:bool) (_:int) (u:unit) (_:bool) (_:int) (e_EXPARAM_242:int)
      (set_flag_f1_292:bool) (s_f1_c_EXPARAM_284:int)
      (e:(bool ->
            int ->
              int ->
                bool ->
                  int ->
                    (bool -> int -> unit -> unit) ->
                      bool -> int -> unit -> unit)) =
  e
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (c2_COEFFICIENT_245 * e_EXPARAM_242 + c1_COEFFICIENT_244)
    set_flag_f1_292 s_f1_c_EXPARAM_284
    (f4 set_flag_f1_292 s_f1_c_EXPARAM_284 u)
let main (set_flag_f1_292:bool) (s_f1_c_EXPARAM_284:int) (u:unit) =
  let zz_1032 =
    f3
      set_flag_f1_292 s_f1_c_EXPARAM_284 u set_flag_f1_292
      s_f1_c_EXPARAM_284 c0_COEFFICIENT_238 set_flag_f1_292
      s_f1_c_EXPARAM_284 (f5 set_flag_f1_292 s_f1_c_EXPARAM_284 u)
  in
  ()
let u_2634 = main false 0 ()
