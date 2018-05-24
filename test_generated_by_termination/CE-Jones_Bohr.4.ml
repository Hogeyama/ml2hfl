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
let f1 (_:bool) (():unit) (_:bool) (_:int) (_:bool)
      (_:(bool -> unit -> unit)) (_:bool) (d:unit) = d
let f2 (_:bool) (u:unit) (_:bool) (a_EXPARAM_267:int) (_:bool)
      (a:(bool ->
            int ->
              bool ->
                (bool ->
                   int ->
                     bool -> (bool -> unit -> unit) -> bool -> unit -> unit) ->
                  bool -> unit -> unit))
      (_:bool) (b_EXPARAM_270:int) (set_flag_f4_580:bool)
      (_:(bool -> unit -> unit)) =
  a
    set_flag_f4_580
    (c9_COEFFICIENT_274 * b_EXPARAM_270 +
     (c8_COEFFICIENT_273 * a_EXPARAM_267 + c7_COEFFICIENT_272))
    set_flag_f4_580 (f1 set_flag_f4_580 u)
let f3 (_:bool) (u:unit) (_:bool) (a_EXPARAM_249:int) (set_flag_f4_580:bool)
      (a:(bool ->
            int ->
              bool ->
                (bool ->
                   int ->
                     bool -> (bool -> unit -> unit) -> bool -> unit -> unit) ->
                  bool -> unit -> unit)) =
  a
    set_flag_f4_580 (c6_COEFFICIENT_261 * a_EXPARAM_249 + c5_COEFFICIENT_260)
    set_flag_f4_580
    (f2
      set_flag_f4_580 u set_flag_f4_580
      (c4_COEFFICIENT_255 * a_EXPARAM_249 + c3_COEFFICIENT_254)
      set_flag_f4_580 a)
let rec f4 (x_DO_NOT_CARE_603:bool) (u:unit) (prev_set_flag_f4_579:bool)
          (v:unit) =
  if prev_set_flag_f4_579 then assert false;
  f4_without_checking_601 x_DO_NOT_CARE_603 u prev_set_flag_f4_579 v
and f4_without_checking_601 (_:bool) (():unit) (_:bool) (v:unit) =
  let set_flag_f4_580 = true
  in
  v
let f5 (_:bool) (u:unit) (_:bool) (e_EXPARAM_242:int)
      (set_flag_f4_580:bool)
      (e:(bool ->
            int -> bool -> (bool -> unit -> unit) -> bool -> unit -> unit)) =
  e
    set_flag_f4_580
    (c2_COEFFICIENT_245 * e_EXPARAM_242 + c1_COEFFICIENT_244)
    set_flag_f4_580 (f4 set_flag_f4_580 u)
let main (set_flag_f4_580:bool) (u:unit) =
  let zz_1032 =
    f3
      set_flag_f4_580 u set_flag_f4_580 c0_COEFFICIENT_238
      set_flag_f4_580 (f5 set_flag_f4_580 u)
  in
  ()
let u_17265 = main false ()
