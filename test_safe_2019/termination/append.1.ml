let rec append (x_DO_NOT_CARE_64:bool) (x_DO_NOT_CARE_65:int)
              (x_DO_NOT_CARE_66:int) (xs:int) (prev_set_flag_append_48:bool)
              (s_prev_append_xs_46:int) (s_prev_append_ys_47:int) (ys:int) =
  if prev_set_flag_append_48 then assert false;
  append_without_checking_62
    x_DO_NOT_CARE_64 x_DO_NOT_CARE_65 x_DO_NOT_CARE_66 xs
    prev_set_flag_append_48 s_prev_append_xs_46 s_prev_append_ys_47 ys
and append_without_checking_62 (_:bool) (_:int) (_:int) (xs:int) (_:bool)
                              (_:int) (_:int) (ys:int) =
  let set_flag_append_49 = true
  in
  let s_append_ys_45 = ys
  in
  let s_append_xs_44 = xs
  in
  if xs <= 0
  then
    ys
  else
    let xs' = xs - 1
    in
    1 +
    append_without_checking_62
      set_flag_append_49 s_append_xs_44 s_append_ys_45 xs'
      set_flag_append_49 s_append_xs_44 s_append_ys_45 ys
let main (set_flag_append_49:bool) (s_append_xs_44:int)
        (s_append_ys_45:int) (():unit) =
  let l1 = Random.int 0
  in
  let l2 = Random.int 0
  in
  append
    set_flag_append_49 s_append_xs_44 s_append_ys_45 l1
    set_flag_append_49 s_append_xs_44 s_append_ys_45 l2
let u_247 = main false 0 0 ()
