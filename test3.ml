let rand_int () = Random.int 0
 let rec finish_1010 (s_9089:(bool)) (set_flag_9090:bool) (p_x_9091:int) (x_2005:unit) : unit  =
   let s_9096 = fst ((true), ()) in
   let u_2011 = snd ((true), ()) in
   finish_1010 s_9096 set_flag_9090 0 ()
 and reduce_1011 (x_1012:int) (s_9102:(bool)) (set_flag_9103:bool) (p_x_9104:int)
                (r_1013:((bool) -> bool -> int -> int -> ((bool) * int))) =
   if x_1012 <= 0 then (s_9102, x_1012) else r_1013 s_9102 set_flag_9103 0 x_1012

 let rec explore_1014 (x_1015:int) (s_9116:(bool)) (set_flag_9117:bool) (
                     p_x_9118:int) (r_1016:((bool) -> bool -> int -> int -> ((bool) * int))) : unit =
   let x_9119 =
     if set_flag_9117
     then
       let u_9124 = if not ( s_9116) then(
                      assert ((1 * p_x_9118) + 0 > (1 * x_1015) + 0)) in
       rand_int () = 0
     else
       true
   in
   let sp_9132 = if x_9119 then ((false), (x_1015)) else (s_9116, (p_x_9118)) in
   let s_9120 = fst sp_9132 in
   let p_x_9122 =  (snd sp_9132) in
   let s__r_reduce_9139 = reduce_1011 x_1015 s_9120 true p_x_9122 r_1016 in
   let s_9140 = fst s__r_reduce_9139 in
   let y_1017 = snd s__r_reduce_9139 in
   if y_1017 <= 0
   then
     finish_1010 s_9140 true p_x_9122 ()
   else
     explore_1014 y_1017 s_9140 true p_x_9122 r_1016

 let f_1018 (s_9156:(bool)) (set_flag_9157:bool) (p_x_9158:int) (x_1019:int) = (s_9156, x_1019 - 2)
 let main_1020 (s_9164:(bool)) (set_flag_9165:bool) (p_x_9166:int) (x_2103:unit) =
   let s__n_9169 = (s_9164, rand_int ()) in
   let s_9170 = fst s__n_9169 in
   let t_1021 = snd s__n_9169 in
   explore_1014 t_1021 s_9170 set_flag_9165 0 f_1018

 let s__r_main_9181 () = main_1020 (false) false 0 ()
