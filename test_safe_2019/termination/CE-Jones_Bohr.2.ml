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
let f1 (_:bool) (_:int) (_:int) (():unit) (_:bool) (_:int) (_:int) (_:int)
      (_:bool) (_:int) (_:int) (_:(bool -> int -> int -> unit -> unit))
      (_:bool) (_:int) (_:int) (d:unit) = d
let rec f2 (x_DO_NOT_CARE_446:bool) (x_DO_NOT_CARE_447:int)
          (x_DO_NOT_CARE_448:int) (u:unit) (x_DO_NOT_CARE_443:bool)
          (x_DO_NOT_CARE_444:int) (x_DO_NOT_CARE_445:int) (a_EXPARAM_267:int)
          (x_DO_NOT_CARE_440:bool) (x_DO_NOT_CARE_441:int)
          (x_DO_NOT_CARE_442:int)
          (a:(bool ->
                int ->
                  int ->
                    int ->
                      bool ->
                        int ->
                          int ->
                            (bool ->
                               int ->
                                 int ->
                                   int ->
                                     bool ->
                                       int ->
                                         int ->
                                           (bool ->
                                              int -> int -> unit -> unit) ->
                                             bool ->
                                               int -> int -> unit -> unit) ->
                              bool -> int -> int -> unit -> unit))
          (x_DO_NOT_CARE_437:bool) (x_DO_NOT_CARE_438:int)
          (x_DO_NOT_CARE_439:int) (b_EXPARAM_270:int)
          (prev_set_flag_f2_389:bool) (s_prev_f2_a_EXPARAM_385:int)
          (s_prev_f2_b_EXPARAM_387:int)
          (b:(bool -> int -> int -> unit -> unit)) =
  if prev_set_flag_f2_389 then assert false;
  f2_without_checking_435
    x_DO_NOT_CARE_446 x_DO_NOT_CARE_447 x_DO_NOT_CARE_448 u
    x_DO_NOT_CARE_443 x_DO_NOT_CARE_444 x_DO_NOT_CARE_445 a_EXPARAM_267
    x_DO_NOT_CARE_440 x_DO_NOT_CARE_441 x_DO_NOT_CARE_442 a
    x_DO_NOT_CARE_437 x_DO_NOT_CARE_438 x_DO_NOT_CARE_439 b_EXPARAM_270
    prev_set_flag_f2_389 s_prev_f2_a_EXPARAM_385 s_prev_f2_b_EXPARAM_387
    b
and f2_without_checking_435 (_:bool) (_:int) (_:int) (u:unit) (_:bool)
                           (_:int) (_:int) (a_EXPARAM_267:int) (_:bool)
                           (_:int) (_:int)
                           (a:(bool ->
                                 int ->
                                   int ->
                                     int ->
                                       bool ->
                                         int ->
                                           int ->
                                             (bool ->
                                                int ->
                                                  int ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          int ->
                                                            (bool ->
                                                               int ->
                                                                 int ->
                                                                   unit ->
                                                                    unit) ->
                                                              bool ->
                                                                int ->
                                                                  int ->
                                                                    unit ->
                                                                    unit) ->
                                               bool ->
                                                 int ->
                                                   int -> unit -> unit))
                           (_:bool) (_:int) (_:int) (b_EXPARAM_270:int)
                           (_:bool) (_:int) (_:int)
                           (_:(bool -> int -> int -> unit -> unit)) =
  let set_flag_f2_390 = true
  in
  let s_f2_b_EXPARAM_382 = b_EXPARAM_270
  in
  let s_f2_a_EXPARAM_380 = a_EXPARAM_267
  in
  a
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (c9_COEFFICIENT_274 * b_EXPARAM_270 +
     (c8_COEFFICIENT_273 * a_EXPARAM_267 + c7_COEFFICIENT_272))
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (f1 set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 u)
let f3 (_:bool) (_:int) (_:int) (u:unit) (_:bool) (_:int) (_:int)
      (a_EXPARAM_249:int) (set_flag_f2_390:bool) (s_f2_a_EXPARAM_380:int)
      (s_f2_b_EXPARAM_382:int)
      (a:(bool ->
            int ->
              int ->
                int ->
                  bool ->
                    int ->
                      int ->
                        (bool ->
                           int ->
                             int ->
                               int ->
                                 bool ->
                                   int ->
                                     int ->
                                       (bool ->
                                          int -> int -> unit -> unit) ->
                                         bool ->
                                           int -> int -> unit -> unit) ->
                          bool -> int -> int -> unit -> unit)) =
  a
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (c6_COEFFICIENT_261 * a_EXPARAM_249 + c5_COEFFICIENT_260)
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (f2
      set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 u
      set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
      (c4_COEFFICIENT_255 * a_EXPARAM_249 + c3_COEFFICIENT_254)
      set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 a)
let f4 (_:bool) (_:int) (_:int) (():unit) (_:bool) (_:int) (_:int)
      (v:unit) = v
let f5 (_:bool) (_:int) (_:int) (u:unit) (_:bool) (_:int) (_:int)
      (e_EXPARAM_242:int) (set_flag_f2_390:bool) (s_f2_a_EXPARAM_380:int)
      (s_f2_b_EXPARAM_382:int)
      (e:(bool ->
            int ->
              int ->
                int ->
                  bool ->
                    int ->
                      int ->
                        (bool -> int -> int -> unit -> unit) ->
                          bool -> int -> int -> unit -> unit)) =
  e
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (c2_COEFFICIENT_245 * e_EXPARAM_242 + c1_COEFFICIENT_244)
    set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
    (f4 set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 u)
let main (set_flag_f2_390:bool) (s_f2_a_EXPARAM_380:int)
        (s_f2_b_EXPARAM_382:int) (u:unit) =
  let zz_1032 =
    f3
      set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 u
      set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382
      c0_COEFFICIENT_238 set_flag_f2_390 s_f2_a_EXPARAM_380
      s_f2_b_EXPARAM_382
      (f5 set_flag_f2_390 s_f2_a_EXPARAM_380 s_f2_b_EXPARAM_382 u)
  in
  ()
let u_8540 = main false 0 0 ()
