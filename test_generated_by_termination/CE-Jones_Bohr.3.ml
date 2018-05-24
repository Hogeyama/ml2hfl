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
let f1 (_:bool) (_:int) (():unit) (_:bool) (_:int) (_:int) (_:bool) (_:int)
      (_:(bool -> int -> unit -> unit)) (_:bool) (_:int) (d:unit) = d
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
      (_:bool) (_:int) (b_EXPARAM_270:int) (set_flag_f3_492:bool)
      (s_f3_a_EXPARAM_486:int) (_:(bool -> int -> unit -> unit)) =
  a
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (c9_COEFFICIENT_274 * b_EXPARAM_270 +
     (c8_COEFFICIENT_273 * a_EXPARAM_267 + c7_COEFFICIENT_272))
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (f1 set_flag_f3_492 s_f3_a_EXPARAM_486 u)
let rec f3 (x_DO_NOT_CARE_527:bool) (x_DO_NOT_CARE_528:int) (u:unit)
          (x_DO_NOT_CARE_525:bool) (x_DO_NOT_CARE_526:int)
          (a_EXPARAM_249:int) (prev_set_flag_f3_491:bool)
          (s_prev_f3_a_EXPARAM_489:int)
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
  if prev_set_flag_f3_491 then assert false;
  f3_without_checking_523
    x_DO_NOT_CARE_527 x_DO_NOT_CARE_528 u x_DO_NOT_CARE_525
    x_DO_NOT_CARE_526 a_EXPARAM_249 prev_set_flag_f3_491
    s_prev_f3_a_EXPARAM_489 a
and f3_without_checking_523 (_:bool) (_:int) (u:unit) (_:bool) (_:int)
                           (a_EXPARAM_249:int) (_:bool) (_:int)
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
                                                    (bool ->
                                                       int ->
                                                         unit -> unit) ->
                                                      bool ->
                                                        int ->
                                                          unit -> unit) ->
                                           bool -> int -> unit -> unit)) =
  let set_flag_f3_492 = true
  in
  let s_f3_a_EXPARAM_486 = a_EXPARAM_249
  in
  a
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (c6_COEFFICIENT_261 * a_EXPARAM_249 + c5_COEFFICIENT_260)
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (f2
      set_flag_f3_492 s_f3_a_EXPARAM_486 u set_flag_f3_492
      s_f3_a_EXPARAM_486
      (c4_COEFFICIENT_255 * a_EXPARAM_249 + c3_COEFFICIENT_254)
      set_flag_f3_492 s_f3_a_EXPARAM_486 a)
let f4 (_:bool) (_:int) (():unit) (_:bool) (_:int) (v:unit) = v
let f5 (_:bool) (_:int) (u:unit) (_:bool) (_:int) (e_EXPARAM_242:int)
      (set_flag_f3_492:bool) (s_f3_a_EXPARAM_486:int)
      (e:(bool ->
            int ->
              int ->
                bool ->
                  int ->
                    (bool -> int -> unit -> unit) ->
                      bool -> int -> unit -> unit)) =
  e
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (c2_COEFFICIENT_245 * e_EXPARAM_242 + c1_COEFFICIENT_244)
    set_flag_f3_492 s_f3_a_EXPARAM_486
    (f4 set_flag_f3_492 s_f3_a_EXPARAM_486 u)
let main (set_flag_f3_492:bool) (s_f3_a_EXPARAM_486:int) (u:unit) =
  let zz_1032 =
    f3
      set_flag_f3_492 s_f3_a_EXPARAM_486 u set_flag_f3_492
      s_f3_a_EXPARAM_486 c0_COEFFICIENT_238 set_flag_f3_492
      s_f3_a_EXPARAM_486 (f5 set_flag_f3_492 s_f3_a_EXPARAM_486 u)
  in
  ()
let u_13848 = main false 0 ()
