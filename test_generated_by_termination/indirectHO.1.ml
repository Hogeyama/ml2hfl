let c1_COEFFICIENT_79 = 0
let c0_COEFFICIENT_78 = 0
let rec id (prev_set_flag_id_85:bool) (x:unit) =
  if prev_set_flag_id_85 then assert false;
  id_without_checking_112 prev_set_flag_id_85 x
and id_without_checking_112 (_:bool) (x:unit) =
  let set_flag_id_86 = true
  in
  x
let app (_:bool) (_:int) (_:bool)
       (h:(bool -> unit -> bool -> unit -> unit)) (set_flag_id_86:bool)
       (v:unit) = h set_flag_id_86 () set_flag_id_86 v
let rec f (_:bool) (n:int) (set_flag_id_86:bool) (():unit) =
  if n > 0
  then
    app
      set_flag_id_86 (c1_COEFFICIENT_79 * n + c0_COEFFICIENT_78)
      set_flag_id_86 (f set_flag_id_86 (n - 1))
  else
    id
let main (set_flag_id_86:bool) (():unit) =
  f set_flag_id_86 (Random.int 0) set_flag_id_86 () set_flag_id_86 ()
let u_396 = main false ()
