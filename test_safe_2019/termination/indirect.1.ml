let c1_COEFFICIENT_75 = 0
let c0_COEFFICIENT_74 = 0
let rec id (prev_set_flag_id_81:bool) (x:unit) =
  if prev_set_flag_id_81 then assert false;
  id_without_checking_108 prev_set_flag_id_81 x
and id_without_checking_108 (_:bool) (x:unit) =
  let set_flag_id_82 = true
  in
  x
let app (_:bool) (_:int) (_:bool)
       (h:(bool -> int -> bool -> unit -> unit)) (_:bool) (v:int)
       (set_flag_id_82:bool) (u:unit) =
  h set_flag_id_82 v set_flag_id_82 u
let rec f (set_flag_id_82:bool) (x:int) =
  if x > 0
  then
    app
      set_flag_id_82 (c1_COEFFICIENT_75 * x + c0_COEFFICIENT_74)
      set_flag_id_82 f set_flag_id_82 (x - 1)
  else
    id
let main (set_flag_id_82:bool) (():unit) =
  f set_flag_id_82 (Random.int 0) set_flag_id_82 ()
let u_394 = main false ()
