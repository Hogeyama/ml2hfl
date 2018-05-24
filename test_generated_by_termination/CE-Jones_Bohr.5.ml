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
      (_:bool) (_:int) (b_EXPARAM_270:int) (set_flag_f5_665:bool)
      (s_f5_e_EXPARAM_659:int) (_:(bool -> int -> unit -> unit)) =
  a
    set_flag_f5_665 s_f5_e_EXPARAM_659
    (c9_COEFFICIENT_274 * b_EXPARAM_270 +
     (c8_COEFFICIENT_273 * a_EXPARAM_267 + c7_COEFFICIENT_272))
    set_flag_f5_665 s_f5_e_EXPARAM_659
    (f1 set_flag_f5_665 s_f5_e_EXPARAM_659 u)
let f3 (_:bool) (_:int) (u:unit) (_:bool) (_:int) (a_EXPARAM_249:int)
      (set_flag_f5_665:bool) (s_f5_e_EXPARAM_659:int)
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
    set_flag_f5_665 s_f5_e_EXPARAM_659
    (c6_COEFFICIENT_261 * a_EXPARAM_249 + c5_COEFFICIENT_260) set_flag_f5_665
    s_f5_e_EXPARAM_659
    (f2
      set_flag_f5_665 s_f5_e_EXPARAM_659 u set_flag_f5_665 s_f5_e_EXPARAM_659
      (c4_COEFFICIENT_255 * a_EXPARAM_249 + c3_COEFFICIENT_254)
      set_flag_f5_665 s_f5_e_EXPARAM_659 a)
let f4 (_:bool) (_:int) (():unit) (_:bool) (_:int) (v:unit) = v
let rec f5 (x_DO_NOT_CARE_682:bool) (x_DO_NOT_CARE_683:int) (u:unit)
          (x_DO_NOT_CARE_680:bool) (x_DO_NOT_CARE_681:int)
          (e_EXPARAM_242:int) (prev_set_flag_f5_664:bool)
          (s_prev_f5_e_EXPARAM_662:int)
          (e:(bool ->
                int ->
                  int ->
                    bool ->
                      int ->
                        (bool -> int -> unit -> unit) ->
                          bool -> int -> unit -> unit)) =
  if prev_set_flag_f5_664 then assert false;
  f5_without_checking_678
    x_DO_NOT_CARE_682 x_DO_NOT_CARE_683 u x_DO_NOT_CARE_680
    x_DO_NOT_CARE_681 e_EXPARAM_242 prev_set_flag_f5_664
    s_prev_f5_e_EXPARAM_662 e
and f5_without_checking_678 (_:bool) (_:int) (u:unit) (_:bool) (_:int)
                           (e_EXPARAM_242:int) (_:bool) (_:int)
                           (e:(bool ->
                                 int ->
                                   int ->
                                     bool ->
                                       int ->
                                         (bool -> int -> unit -> unit) ->
                                           bool -> int -> unit -> unit)) =
  let set_flag_f5_665 = true
  in
  let s_f5_e_EXPARAM_659 = e_EXPARAM_242
  in
  e
    set_flag_f5_665 s_f5_e_EXPARAM_659
    (c2_COEFFICIENT_245 * e_EXPARAM_242 + c1_COEFFICIENT_244)
    set_flag_f5_665 s_f5_e_EXPARAM_659
    (f4 set_flag_f5_665 s_f5_e_EXPARAM_659 u)
let main (set_flag_f5_665:bool) (s_f5_e_EXPARAM_659:int) (u:unit) =
  let zz_1032 =
    f3
      set_flag_f5_665 s_f5_e_EXPARAM_659 u set_flag_f5_665
      s_f5_e_EXPARAM_659 c0_COEFFICIENT_238 set_flag_f5_665
      s_f5_e_EXPARAM_659 (f5 set_flag_f5_665 s_f5_e_EXPARAM_659 u)
  in
  ()
let u_20830 = main false 0 ()
