let rec append (x_DO_NOT_CARE_63:bool) (x_DO_NOT_CARE_64:int)
              (x_DO_NOT_CARE_65:int) (xs:int) (prev_set_flag_append_47:bool)
              (s_prev_append_xs_45:int) (s_prev_append_ys_46:int) (ys:int) =
  if prev_set_flag_append_47 then assert false;
  append_without_checking_61
    x_DO_NOT_CARE_63 x_DO_NOT_CARE_64 x_DO_NOT_CARE_65 xs
    prev_set_flag_append_47 s_prev_append_xs_45 s_prev_append_ys_46 ys
and append_without_checking_61 (_:bool) (_:int) (_:int) (xs:int) (_:bool)
                              (_:int) (_:int) (ys:int) =
  let set_flag_append_48 = true
  in
  let s_append_ys_44 = ys
  in
  let s_append_xs_43 = xs
  in
  if xs <= 0
  then
    ys
  else
    let xs' = xs - 1
    in
    1 +
    append_without_checking_61
      set_flag_append_48 s_append_xs_43 s_append_ys_44 xs'
      set_flag_append_48 s_append_xs_43 s_append_ys_44 ys
let main (set_flag_append_48:bool) (s_append_xs_43:int)
        (s_append_ys_44:int) (():unit) =
  let l1 = Random.int 0
  in
  let l2 = Random.int 0
  in
  append
    set_flag_append_48 s_append_xs_43 s_append_ys_44 l1
    set_flag_append_48 s_append_xs_43 s_append_ys_44 l2
let u_246 = main false 0 0 ()
