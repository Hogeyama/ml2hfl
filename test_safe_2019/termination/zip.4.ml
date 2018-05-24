let rec zip (x_DO_NOT_CARE_72:bool) (x_DO_NOT_CARE_73:int)
           (x_DO_NOT_CARE_74:int) (xs:int) (prev_set_flag_zip_55:bool)
           (s_prev_zip_xs_53:int) (s_prev_zip_ys_54:int) (ys:int) =
  if prev_set_flag_zip_55
  then
    if s_prev_zip_xs_53 > xs && xs >= 0 then () else assert false;
  zip_without_checking_70
    x_DO_NOT_CARE_72 x_DO_NOT_CARE_73 x_DO_NOT_CARE_74 xs
    prev_set_flag_zip_55 s_prev_zip_xs_53 s_prev_zip_ys_54 ys
and zip_without_checking_70 (_:bool) (_:int) (_:int) (xs:int)
                           (_:bool) (_:int) (_:int) (ys:int) =
  let set_flag_zip_56 = true
  in
  let s_zip_ys_52 = ys
  in
  let s_zip_xs_51 = xs
  in
  if xs <= 0
  then
    0
  else
    let xs' = xs - 1
    in
    if ys <= 0
    then
      0
    else
      let ys' = ys - 1
      in
      1 +
      zip
        set_flag_zip_56 s_zip_xs_51 s_zip_ys_52 xs' set_flag_zip_56
        s_zip_xs_51 s_zip_ys_52 ys'
let main (set_flag_zip_56:bool) (s_zip_xs_51:int) (s_zip_ys_52:int)
        (():unit) =
  let l1 = Random.int 0
  in
  let l2 = Random.int 0
  in
  zip_without_checking_70
    set_flag_zip_56 s_zip_xs_51 s_zip_ys_52 l1 set_flag_zip_56
    s_zip_xs_51 s_zip_ys_52 l2
let u_3760 = main false 0 0 ()
