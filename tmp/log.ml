MoCHi: Model Checker for Higher-Order Programs
  Build: _438a443 (after 2014-07-10 12:53:16 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test.ml -debug-module Tupling -disable-rc -color -tupling -gchi -list-option -abs-remove-false

parsed:
 let rec id_1008 x_1009 = if x_1009 < 0 then
                            0
                          else
                            1 + id_1008 (x_1009 - 1) in
 let rec succ_1010 x_1011 = if x_1011 < 0 then
                              1
                            else
                              1 + succ_1010 (x_1011 - 1) in
 let rec double_1012 x_1013 = if x_1013 < 0 then
                                0
                              else
                                2 + double_1012 (x_1013 - 1) in
 let eq_1014 x_1015 x_1069 =
   match x_1069 with
   | (f_1016, g_1017, h_1018) -> f_1016 x_1015 + g_1017 x_1015 = h_1018 x_1015 + 1
 in
 let main_1019 n_1020 = if eq_1014 n_1020 (id_1008, succ_1010, double_1012) then
                          ()
                        else
                          {fail} () in
 ()

set_target:
 let rec id_1008 (x_1009:int) = if x_1009 < 0 then
                                  0
                                else
                                  1 + id_1008 (x_1009 - 1) in
 let rec succ_1010 (x_1011:int) = if x_1011 < 0 then
                                    1
                                  else
                                    1 + succ_1010 (x_1011 - 1) in
 let rec double_1012 (x_1013:int) = if x_1013 < 0 then
                                      0
                                    else
                                      2 + double_1012 (x_1013 - 1) in
 let eq_1014 (x_1015:!!!) (x_1069:((!!! -> int) * (!!! -> int) * (!!! -> int))) =
   match x_1069 with
   | (f_1016, g_1017, h_1018) -> f_1016 x_1015 + g_1017 x_1015 = h_1018 x_1015 + 1
 in
 let main_1019 (n_1020:int) = if eq_1014 n_1020 (id_1008, succ_1010, double_1012) then
                                ()
                              else
                                {fail} () in
 let main_1110 = let arg1_1108 = rand_int () in
                 main_1019 arg1_1108 in
 ()

copy_poly:
 let rec id_1008 (x_1009:int) = if x_1009 < 0 then
                                  0
                                else
                                  1 + id_1008 (x_1009 - 1) in
 let rec succ_1010 (x_1011:int) = if x_1011 < 0 then
                                    1
                                  else
                                    1 + succ_1010 (x_1011 - 1) in
 let rec double_1012 (x_1013:int) = if x_1013 < 0 then
                                      0
                                    else
                                      2 + double_1012 (x_1013 - 1) in
 let eq_1111 (x_1015:int) (x_1069:((int -> int) * (int -> int) * (int -> int))) =
   match x_1069 with
   | (f_1016, g_1017, h_1018) -> f_1016 x_1015 + g_1017 x_1015 = h_1018 x_1015 + 1
 in
 let main_1019 (n_1020:int) = if eq_1111 n_1020 (id_1008, succ_1010, double_1012) then
                                ()
                              else
                                {fail} () in
 let main_1110 = let arg1_1108 = rand_int () in
                 main_1019 arg1_1108 in
 ()

encode_list:
 let rec id_1008 (x_1009:int) = if x_1009 < 0 then
                                  0
                                else
                                  1 + id_1008 (x_1009 - 1) in
 let rec succ_1010 (x_1011:int) = if x_1011 < 0 then
                                    1
                                  else
                                    1 + succ_1010 (x_1011 - 1) in
 let rec double_1012 (x_1013:int) = if x_1013 < 0 then
                                      0
                                    else
                                      2 + double_1012 (x_1013 - 1) in
 let eq_1111 (x_1015:int) (x_1069:((int -> int) * (int -> int) * (int -> int))) =
   let f_1016 = #0 x_1069 in
   let g_1017 = #1 x_1069 in
   let h_1018 = #2 x_1069 in
   f_1016 x_1015 + g_1017 x_1015 = h_1018 x_1015 + 1
 in
 let main_1019 (n_1020:int) = if eq_1111 n_1020 (id_1008, succ_1010, double_1012) then
                                ()
                              else
                                {fail} () in
 let main_1110 = let arg1_1108 = rand_int () in
                 main_1019 arg1_1108 in
 ()

ret_fun:
 let rec id_1008 (x_1009:int) =
   let b_1136 = x_1009 < 0 in
   if b_1136 then
     0
   else
     let n_1142 = x_1009 - 1 in
     let n_1143 = id_1008 n_1142 in
     1 + n_1143
 in
 let rec succ_1010 (x_1011:int) =
   let b_1146 = x_1011 < 0 in
   if b_1146 then
     1
   else
     let n_1152 = x_1011 - 1 in
     let n_1153 = succ_1010 n_1152 in
     1 + n_1153
 in
 let rec double_1012 (x_1013:int) =
   let b_1156 = x_1013 < 0 in
   if b_1156 then
     0
   else
     let n_1162 = x_1013 - 1 in
     let n_1163 = double_1012 n_1162 in
     2 + n_1163
 in
 let eq_1111 (x_1015:int) (x_1069:((int -> int) * (int -> int) * (int -> int))) =
   let f_1016 = #0 x_1069 in
   let g_1017 = #1 x_1069 in
   let h_1018 = #2 x_1069 in
   let n_1171 = f_1016 x_1015 in
   let n_1174 = g_1017 x_1015 in
   let n_1182 = n_1171 + n_1174 in
   let n_1179 = h_1018 x_1015 in
   let n_1183 = n_1179 + 1 in
   (n_1182 = n_1183, x_1069)
 in
 let main_1019 (n_1020:int) =
   let p_1193 = (id_1008, succ_1010, double_1012) in
   let f_1194 = eq_1111 n_1020 in
   let p_1214 = f_1194 p_1193 in
   let b_1195 = fst p_1214 in
   let p_1215 = snd p_1214 in
   if b_1195 then
     ()
   else
     let f_1196 = {fail} in
     let u_1198 = f_1196 () in
     u_1198
 in
 let f_1199 = rand_int in
 let n_1201 = f_1199 () in
 let u_1204 = main_1019 n_1201 in
 ()

ref_trans:
 let rec id_1008 (x_1009:int) = if x_1009 < 0 then
                                  0
                                else
                                  let x_1236 = id_1008 (x_1009 - 1) in
                                  1 + x_1236 in
 let rec succ_1010 (x_1011:int) = if x_1011 < 0 then
                                    1
                                  else
                                    let x_1243 = succ_1010 (x_1011 - 1) in
                                    1 + x_1243 in
 let rec double_1012 (x_1013:int) = if x_1013 < 0 then
                                      0
                                    else
                                      let x_1250 = double_1012 (x_1013 - 1) in
                                      2 + x_1250 in
 let
   eq_1111 (x_1015:int) 
          (x_1069:(((bool * int) * (bool * int) * (bool * int)) -> ((bool * int) * (bool * int) * (bool * int)))) =
   let x_1253 (x_2069:int) = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
   let x_1254 (x_2059:int) = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
   let x_1255 (x_2049:int) = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
   let x_1256 = let x_2048 = x_1069 ((true, x_1015), (false, 0), (false, 0)) in
                snd (#0 x_2048) in
   let x_1257 = let x_2018 = x_1069 ((true, x_1015), (true, x_1015), (false, 0)) in
                snd (#1 x_2018) in
   let x_1259 = let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
                snd (#2 x_1988) in
   (x_1256 + x_1257 = x_1259 + 1, x_1069)
 in
 let main_1019 (n_1020:int) =
   let x_1270 (xxx_1729:((bool * int) * (bool * int) * (bool * int))) =
     ((if fst (#0 xxx_1729) = false then
         (false, 0)
       else
         (true, id_1008 (snd (#0 xxx_1729)))),
      (if fst (#1 xxx_1729) = false then
         (false, 0)
       else
         (true, succ_1010 (snd (#1 xxx_1729)))),
      (if fst (#2 xxx_1729) = false then
         (false, 0)
       else
         (true, double_1012 (snd (#2 xxx_1729)))))
   in
   let x_1271 (x_1700:int) = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
   let x_1272 (x_1690:int) = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
   let x_1273 (x_1680:int) = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
   let x_1274 = eq_1111 n_1020 in
   let x_1275 = x_1274 x_1270 in
   let x_1278 (x_1668:int) = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
   let x_1279 (x_1658:int) = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
   let x_1280 (x_1648:int) = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
   if fst x_1275 then
     ()
   else
     {fail} ()
 in
 let x_1282 = rand_int () in
 let x_1283 = main_1019 x_1282 in
 ()

inline_wrapped:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_1256 = let x_2048 = x_1069 ((true, x_1015), (false, 0), (false, 0)) in
               snd (#0 x_2048) in
  let x_1257 = let x_2018 = x_1069 ((true, x_1015), (true, x_1015), (false, 0)) in
               snd (#1 x_2018) in
  let x_1259 = let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
               snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

flatten_let:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_2048 = x_1069 ((true, x_1015), (false, 0), (false, 0)) in
  let x_1256 = snd (#0 x_2048) in
  let x_2018 = x_1069 ((true, x_1015), (true, x_1015), (false, 0)) in
  let x_1257 = snd (#1 x_2018) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

NORMALIZE: x_1257
[x_1988]
NORMALIZE: x_1256
[x_2018;x_1988]
normalize let:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_2048 = x_1069 ((true, x_1015), (false, 0), (false, 0)) in
  let x_2018 = x_1069 ((true, x_1015), (true, x_1015), (false, 0)) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1256 = snd (#0 x_2048) in
  let x_1257 = snd (#1 x_2018) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

is_subsumed: x_1069 ((true, x_1015), (false, 0), (false, 0)), x_1069 ((true, x_1015), (true, x_1015), (false, 0)); x_2048 |-> x_2018
is_subsumed: x_1069 ((true, x_1015), (true, x_1015), (false, 0)), x_1069
                                                                    ((true, x_1015), (true, x_1015), (true, x_1015)); x_2018 |-> x_1988
is_subsumed: x_1069 ((true, x_1015), (true, x_1015), (false, 0)), snd (#0 x_1988); is_subsumed: 
x_1069 ((true, x_1015), (false, 0), (false, 0)), snd (#0 x_1988); is_subsumed: 
snd (#0 x_1988), snd (#1 x_1988); is_subsumed: x_1069 ((true, x_1015), (true, x_1015), (false, 0)), 
snd (#1 x_1988); is_subsumed: x_1069 ((true, x_1015), (false, 0), (false, 0)), 
snd (#1 x_1988); is_subsumed: snd (#1 x_1988), snd (#2 x_1988); is_subsumed: 
snd (#0 x_1988), snd (#2 x_1988); is_subsumed: x_1069 ((true, x_1015), (true, x_1015), (false, 0)), 
snd (#2 x_1988); is_subsumed: x_1069 ((true, x_1015), (false, 0), (false, 0)), 
snd (#2 x_1988); x_2048; x_2018
elim_same_app:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1256 = snd (#0 x_1988) in
  let x_1257 = snd (#1 x_1988) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

elim_unused_branch:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1256 = snd (#0 x_1988) in
  let x_1257 = snd (#1 x_1988) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

elim_unused_let:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1256 = snd (#0 x_1988) in
  let x_1257 = snd (#1 x_1988) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
           (true, double_1012 (snd (#2 xxx_1729))))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

TUPLE: (true, x_1015), (true, x_1015), (true, x_1015)
TUPLE: (true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), 
       (true, double_1012 (snd (#2 xxx_1729)))
id_1008
succ_1010
double_1012
compose: id_1008, if x_2157 < 0 then
                    0
                  else
                    let x_1236 = id_1008 (x_2157 - 1) in
                    1 + x_1236; succ_1010, if x_2158 < 0 then
                                             1
                                           else
                                             let x_1243 = succ_1010 (x_2158 - 1) in
                                             1 + x_1243; double_1012, 
if x_2159 < 0 then
  0
else
  let x_1250 = double_1012 (x_2159 - 1) in
  2 + x_1250; 
compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, if x_2158 < 0 then
                                           1
                                         else
                                           let x_1243 = succ_1010 (x_2158 - 1) in
                                           1 + x_1243; double_1012, if 
                                                                    x_2159 < 0 then
                                                                      
                                                                    0
                                                                    else
                                                                      
                                                                    let x_1250 = double_1012 (x_2159 - 1) in
                                                                    2 + x_1250; 
compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                         1 + x_1243; double_1012, if 
                                                                  x_2159 < 0 then
                                                                    0
                                                                  else
                                                                    let x_1250 = double_1012 (x_2159 - 1) in
                                                                    2 + x_1250; 
compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                         1 + x_1243; double_1012, let x_1250 = double_1012 (x_2159 - 1) in
                                                                  2 + x_1250; 
PB: x:id_1008
CHECK: 1 + x_1236
PB: x:succ_1010
CHECK: 1 + x_1243
PB: x:double_1012
CHECK: 2 + x_1250
compose_let
id_1008:let x_1236 = id_1008 (x_2157 - 1) in
        1 + x_1236

succ_1010:let x_1243 = succ_1010 (x_2158 - 1) in
          1 + x_1243

double_1012:let x_1250 = double_1012 (x_2159 - 1) in
            2 + x_1250

PB: x:double_1012
CHECK: 2 + x_2164
PB: x:succ_1010
CHECK: 1 + x_2166
PB: x:id_1008
CHECK: 1 + x_2168
compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                         1 + x_1243; double_1012, 0; 
PB: x:id_1008
CHECK: 1 + x_1236
PB: x:succ_1010
CHECK: 1 + x_1243
PB: x:double_1012
CHECK: 0
compose_let
id_1008:let x_1236 = id_1008 (x_2157 - 1) in
        1 + x_1236

succ_1010:let x_1243 = succ_1010 (x_2158 - 1) in
          1 + x_1243

double_1012:0

compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, 1; double_1012, if x_2159 < 0 then
                                                           0
                                                         else
                                                           let x_1250 = double_1012 (x_2159 - 1) in
                                                           2 + x_1250; 
compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, 1; double_1012, let x_1250 = double_1012 (x_2159 - 1) in
                                                         2 + x_1250; 
PB: x:id_1008
CHECK: 1 + x_1236
PB: x:succ_1010
CHECK: 1
PB: x:double_1012
CHECK: 2 + x_1250
compose_let
id_1008:let x_1236 = id_1008 (x_2157 - 1) in
        1 + x_1236

succ_1010:1

double_1012:let x_1250 = double_1012 (x_2159 - 1) in
            2 + x_1250

compose: id_1008, let x_1236 = id_1008 (x_2157 - 1) in
                  1 + x_1236; succ_1010, 1; double_1012, 0; 
PB: x:id_1008
CHECK: 1 + x_1236
PB: x:succ_1010
CHECK: 1
PB: x:double_1012
CHECK: 0
compose_let
id_1008:let x_1236 = id_1008 (x_2157 - 1) in
        1 + x_1236

succ_1010:1

double_1012:0

compose: id_1008, 0; succ_1010, if x_2158 < 0 then
                                  1
                                else
                                  let x_1243 = succ_1010 (x_2158 - 1) in
                                  1 + x_1243; double_1012, if x_2159 < 0 then
                                                             0
                                                           else
                                                             let x_1250 = double_1012 (x_2159 - 1) in
                                                             2 + x_1250; 
compose: id_1008, 0; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                1 + x_1243; double_1012, if x_2159 < 0 then
                                                           0
                                                         else
                                                           let x_1250 = double_1012 (x_2159 - 1) in
                                                           2 + x_1250; 
compose: id_1008, 0; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                1 + x_1243; double_1012, let x_1250 = double_1012 (x_2159 - 1) in
                                                         2 + x_1250; 
PB: x:id_1008
CHECK: 0
PB: x:succ_1010
CHECK: 1 + x_1243
PB: x:double_1012
CHECK: 2 + x_1250
compose_let
id_1008:0

succ_1010:let x_1243 = succ_1010 (x_2158 - 1) in
          1 + x_1243

double_1012:let x_1250 = double_1012 (x_2159 - 1) in
            2 + x_1250

compose: id_1008, 0; succ_1010, let x_1243 = succ_1010 (x_2158 - 1) in
                                1 + x_1243; double_1012, 0; 
PB: x:id_1008
CHECK: 0
PB: x:succ_1010
CHECK: 1 + x_1243
PB: x:double_1012
CHECK: 0
compose_let
id_1008:0

succ_1010:let x_1243 = succ_1010 (x_2158 - 1) in
          1 + x_1243

double_1012:0

compose: id_1008, 0; succ_1010, 1; double_1012, if x_2159 < 0 then
                                                  0
                                                else
                                                  let x_1250 = double_1012 (x_2159 - 1) in
                                                  2 + x_1250; 
compose: id_1008, 0; succ_1010, 1; double_1012, let x_1250 = double_1012 (x_2159 - 1) in
                                                2 + x_1250; 
PB: x:id_1008
CHECK: 0
PB: x:succ_1010
CHECK: 1
PB: x:double_1012
CHECK: 2 + x_1250
compose_let
id_1008:0

succ_1010:1

double_1012:let x_1250 = double_1012 (x_2159 - 1) in
            2 + x_1250

compose: id_1008, 0; succ_1010, 1; double_1012, 0; 
PB: x:id_1008
CHECK: 0
PB: x:succ_1010
CHECK: 1
PB: x:double_1012
CHECK: 0
compose_let
id_1008:0

succ_1010:1

double_1012:0

ADD: id_succ_double_2160
tupled:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_1236 = id_1008 (x_1009 - 1) in
                           1 + x_1236 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_1243 = succ_1010 (x_1011 - 1) in
                             1 + x_1243 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_1250 = double_1012 (x_1013 - 1) in
                               2 + x_1250 in
let rec id_succ_double_2160 x_2157 x_2158 x_2159 =
  if x_2157 < 0 then
    if x_2158 < 0 then
      if x_2159 < 0 then
        let r_2231 = 0 in
        let r_2232 = 1 in
        let r_2233 = 0 in
        (r_2231, r_2232, r_2233)
      else
        let r_2222 = 0 in
        let r_2223 = 1 in
        let x_1250 = double_1012 (x_2159 - 1) in
        let r_2224 = 2 + x_1250 in
        (r_2222, r_2223, r_2224)
    else
      if x_2159 < 0 then
        let r_2213 = 0 in
        let x_1243 = succ_1010 (x_2158 - 1) in
        let r_2214 = 1 + x_1243 in
        let r_2215 = 0 in
        (r_2213, r_2214, r_2215)
      else
        let r_2204 = 0 in
        let x_1243 = succ_1010 (x_2158 - 1) in
        let r_2205 = 1 + x_1243 in
        let x_1250 = double_1012 (x_2159 - 1) in
        let r_2206 = 2 + x_1250 in
        (r_2204, r_2205, r_2206)
  else
    if x_2158 < 0 then
      if x_2159 < 0 then
        let x_1236 = id_1008 (x_2157 - 1) in
        let r_2195 = 1 + x_1236 in
        let r_2196 = 1 in
        let r_2197 = 0 in
        (r_2195, r_2196, r_2197)
      else
        let x_1236 = id_1008 (x_2157 - 1) in
        let r_2186 = 1 + x_1236 in
        let r_2187 = 1 in
        let x_1250 = double_1012 (x_2159 - 1) in
        let r_2188 = 2 + x_1250 in
        (r_2186, r_2187, r_2188)
    else
      if x_2159 < 0 then
        let x_1236 = id_1008 (x_2157 - 1) in
        let r_2177 = 1 + x_1236 in
        let x_1243 = succ_1010 (x_2158 - 1) in
        let r_2178 = 1 + x_1243 in
        let r_2179 = 0 in
        (r_2177, r_2178, r_2179)
      else
        let p_2170 = id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) in
        let x_2168 = #0 p_2170 in
        let x_2166 = #1 p_2170 in
        let x_2164 = #2 p_2170 in
        let r_2169 = 1 + x_2168 in
        let r_2167 = 1 + x_2166 in
        let r_2165 = 2 + x_2164 in
        (r_2169, r_2167, r_2165)
in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = snd (#0 (x_1069 ((true, x_2069), (false, 0), (false, 0)))) in
  let x_1254 x_2059 = snd (#1 (x_1069 ((false, 0), (true, x_2059), (false, 0)))) in
  let x_1255 x_2049 = snd (#2 (x_1069 ((false, 0), (false, 0), (true, x_2049)))) in
  let x_1988 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  let x_1256 = snd (#0 x_1988) in
  let x_1257 = snd (#1 x_1988) in
  let x_1259 = snd (#2 x_1988) in
  (x_1256 + x_1257 = x_1259 + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      ((false, 0), (if fst (#1 xxx_1729) = false then
                      (false, 0)
                    else
                      (true, succ_1010 (snd (#1 xxx_1729)))),
       (if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          (true, double_1012 (snd (#2 xxx_1729)))))
    else
      if fst (#1 xxx_1729) = false then
        ((true, id_1008 (snd (#0 xxx_1729))), (false, 0), 
         (if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            (true, double_1012 (snd (#2 xxx_1729)))))
      else
        if fst (#2 xxx_1729) = false then
          ((true, id_1008 (snd (#0 xxx_1729))), (true, succ_1010 (snd (#1 xxx_1729))), (false, 0))
        else
          let r_2237 = id_succ_double_2160 (snd (#0 xxx_1729)) (snd (#1 xxx_1729)) (snd (#2 xxx_1729)) in
          ((true, #0 r_2237), (true, #1 r_2237), (true, #2 r_2237))
  in
  let x_1271 x_1700 = snd (#0 (x_1270 ((true, x_1700), (false, 0), (false, 0)))) in
  let x_1272 x_1690 = snd (#1 (x_1270 ((false, 0), (true, x_1690), (false, 0)))) in
  let x_1273 x_1680 = snd (#2 (x_1270 ((false, 0), (false, 0), (true, x_1680)))) in
  let x_1274 = eq_1111 n_1020 in
  let x_1275 = x_1274 x_1270 in
  let x_1278 x_1668 = snd (#0 ((snd x_1275) ((true, x_1668), (false, 0), (false, 0)))) in
  let x_1279 x_1658 = snd (#1 ((snd x_1275) ((false, 0), (true, x_1658), (false, 0)))) in
  let x_1280 x_1648 = snd (#2 ((snd x_1275) ((false, 0), (false, 0), (true, x_1648)))) in
  if fst x_1275 then
    ()
  else
    {fail} ()
in
let x_1282 = rand_int () in
let x_1283 = main_1019 x_1282 in
()

normalize:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_2249 = id_1008 (x_1009 - 1) in
                           1 + x_2249 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_2256 = succ_1010 (x_1011 - 1) in
                             1 + x_2256 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_2263 = double_1012 (x_1013 - 1) in
                               2 + x_2263 in
let rec id_succ_double_2160 x_2157 x_2158 x_2159 =
  if x_2157 < 0 then
    if x_2158 < 0 then
      if x_2159 < 0 then
        (0, 1, 0)
      else
        let x_2358 = double_1012 (x_2159 - 1) in
        (0, 1, 2 + x_2358)
    else
      if x_2159 < 0 then
        let x_2347 = succ_1010 (x_2158 - 1) in
        (0, 1 + x_2347, 0)
      else
        let x_2333 = succ_1010 (x_2158 - 1) in
        let x_2338 = double_1012 (x_2159 - 1) in
        (0, 1 + x_2333, 2 + x_2338)
  else
    if x_2158 < 0 then
      if x_2159 < 0 then
        let x_2320 = id_1008 (x_2157 - 1) in
        (1 + x_2320, 1, 0)
      else
        let x_2306 = id_1008 (x_2157 - 1) in
        let x_2311 = double_1012 (x_2159 - 1) in
        (1 + x_2306, 1, 2 + x_2311)
    else
      if x_2159 < 0 then
        let x_2290 = id_1008 (x_2157 - 1) in
        let x_2295 = succ_1010 (x_2158 - 1) in
        (1 + x_2290, 1 + x_2295, 0)
      else
        let x_2274 = id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) in
        (1 + #0 x_2274, 1 + #1 x_2274, 2 + #2 x_2274)
in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = let x_2398 = x_1069 ((true, x_2069), (false, 0), (false, 0)) in
                      snd (#0 x_2398) in
  let x_1254 x_2059 = let x_2424 = x_1069 ((false, 0), (true, x_2059), (false, 0)) in
                      snd (#1 x_2424) in
  let x_1255 x_2049 = let x_2450 = x_1069 ((false, 0), (false, 0), (true, x_2049)) in
                      snd (#2 x_2450) in
  let x_2472 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  (snd (#0 x_2472) + snd (#1 x_2472) = snd (#2 x_2472) + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      let x_2616 =
        if fst (#1 xxx_1729) = false then
          (false, 0)
        else
          let x_2622 = succ_1010 (snd (#1 xxx_1729)) in
          (true, x_2622)
      in
      let x_2637 =
        if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          let x_2643 = double_1012 (snd (#2 xxx_1729)) in
          (true, x_2643)
      in
      ((false, 0), x_2616, x_2637)
    else
      if fst (#1 xxx_1729) = false then
        let x_2569 = id_1008 (snd (#0 xxx_1729)) in
        let x_2579 =
          if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            let x_2585 = double_1012 (snd (#2 xxx_1729)) in
            (true, x_2585)
        in
        ((true, x_2569), (false, 0), x_2579)
      else
        if fst (#2 xxx_1729) = false then
          let x_2533 = id_1008 (snd (#0 xxx_1729)) in
          let x_2543 = succ_1010 (snd (#1 xxx_1729)) in
          ((true, x_2533), (true, x_2543), (false, 0))
        else
          let x_2501 = id_succ_double_2160 (snd (#0 xxx_1729)) (snd (#1 xxx_1729)) (snd (#2 xxx_1729)) in
          ((true, #0 x_2501), (true, #1 x_2501), (true, #2 x_2501))
  in
  let x_1271 x_1700 = let x_2691 = x_1270 ((true, x_1700), (false, 0), (false, 0)) in
                      snd (#0 x_2691) in
  let x_1272 x_1690 = let x_2717 = x_1270 ((false, 0), (true, x_1690), (false, 0)) in
                      snd (#1 x_2717) in
  let x_1273 x_1680 = let x_2743 = x_1270 ((false, 0), (false, 0), (true, x_1680)) in
                      snd (#2 x_2743) in
  let x_2746 = eq_1111 n_1020 in
  let x_2747 = x_2746 x_1270 in
  let x_1278 x_1668 = let x_2773 = (snd x_2747) ((true, x_1668), (false, 0), (false, 0)) in
                      snd (#0 x_2773) in
  let x_1279 x_1658 = let x_2801 = (snd x_2747) ((false, 0), (true, x_1658), (false, 0)) in
                      snd (#1 x_2801) in
  let x_1280 x_1648 = let x_2829 = (snd x_2747) ((false, 0), (false, 0), (true, x_1648)) in
                      snd (#2 x_2829) in
  if fst x_2747 then
    ()
  else
    {fail} ()
in
let x_2836 = rand_int () in
let x_2837 = main_1019 x_2836 in
let x_1283 = x_2837 in
()

replace[3]: x_2472
APPS: x_2472 = x_1069 ...0... x_1015 ...
APPS: x_2472 = x_1069 ...1... x_1015 ...
APPS: x_2472 = x_1069 ...2... x_1015 ...
USED: x_2472 = x_1069 ...0... x_1015 ...
USED: x_2472 = x_1069 ...1... x_1015 ...
USED: x_2472 = x_1069 ...2... x_1015 ...
MUST: x_2472 = x_1069 ...2... x_1015 ...
MUST: x_2472 = x_1069 ...1... x_1015 ...
MUST: x_2472 = x_1069 ...0... x_1015 ...
replace_app:
let rec id_1008 x_1009 = if x_1009 < 0 then
                           0
                         else
                           let x_2249 = id_1008 (x_1009 - 1) in
                           1 + x_2249 in
let rec succ_1010 x_1011 = if x_1011 < 0 then
                             1
                           else
                             let x_2256 = succ_1010 (x_1011 - 1) in
                             1 + x_2256 in
let rec double_1012 x_1013 = if x_1013 < 0 then
                               0
                             else
                               let x_2263 = double_1012 (x_1013 - 1) in
                               2 + x_2263 in
let rec id_succ_double_2160 x_2157 x_2158 x_2159 =
  if x_2157 < 0 then
    if x_2158 < 0 then
      if x_2159 < 0 then
        (0, 1, 0)
      else
        let x_2358 = double_1012 (x_2159 - 1) in
        (0, 1, 2 + x_2358)
    else
      if x_2159 < 0 then
        let x_2347 = succ_1010 (x_2158 - 1) in
        (0, 1 + x_2347, 0)
      else
        let x_2333 = succ_1010 (x_2158 - 1) in
        let x_2338 = double_1012 (x_2159 - 1) in
        (0, 1 + x_2333, 2 + x_2338)
  else
    if x_2158 < 0 then
      if x_2159 < 0 then
        let x_2320 = id_1008 (x_2157 - 1) in
        (1 + x_2320, 1, 0)
      else
        let x_2306 = id_1008 (x_2157 - 1) in
        let x_2311 = double_1012 (x_2159 - 1) in
        (1 + x_2306, 1, 2 + x_2311)
    else
      if x_2159 < 0 then
        let x_2290 = id_1008 (x_2157 - 1) in
        let x_2295 = succ_1010 (x_2158 - 1) in
        (1 + x_2290, 1 + x_2295, 0)
      else
        let x_2274 = id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) in
        (1 + #0 x_2274, 1 + #1 x_2274, 2 + #2 x_2274)
in
let eq_1111 x_1015 x_1069 =
  let x_1253 x_2069 = let x_2398 = x_1069 ((true, x_2069), (false, 0), (false, 0)) in
                      snd (#0 x_2398) in
  let x_1254 x_2059 = let x_2424 = x_1069 ((false, 0), (true, x_2059), (false, 0)) in
                      snd (#1 x_2424) in
  let x_1255 x_2049 = let x_2450 = x_1069 ((false, 0), (false, 0), (true, x_2049)) in
                      snd (#2 x_2450) in
  let x_2472 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
  (snd (#0 x_2472) + snd (#1 x_2472) = snd (#2 x_2472) + 1, x_1069)
in
let main_1019 n_1020 =
  let x_1270 xxx_1729 =
    if fst (#0 xxx_1729) = false then
      let x_2616 =
        if fst (#1 xxx_1729) = false then
          (false, 0)
        else
          let x_2622 = succ_1010 (snd (#1 xxx_1729)) in
          (true, x_2622)
      in
      let x_2637 =
        if fst (#2 xxx_1729) = false then
          (false, 0)
        else
          let x_2643 = double_1012 (snd (#2 xxx_1729)) in
          (true, x_2643)
      in
      ((false, 0), x_2616, x_2637)
    else
      if fst (#1 xxx_1729) = false then
        let x_2569 = id_1008 (snd (#0 xxx_1729)) in
        let x_2579 =
          if fst (#2 xxx_1729) = false then
            (false, 0)
          else
            let x_2585 = double_1012 (snd (#2 xxx_1729)) in
            (true, x_2585)
        in
        ((true, x_2569), (false, 0), x_2579)
      else
        if fst (#2 xxx_1729) = false then
          let x_2533 = id_1008 (snd (#0 xxx_1729)) in
          let x_2543 = succ_1010 (snd (#1 xxx_1729)) in
          ((true, x_2533), (true, x_2543), (false, 0))
        else
          let x_2501 = id_succ_double_2160 (snd (#0 xxx_1729)) (snd (#1 xxx_1729)) (snd (#2 xxx_1729)) in
          ((true, #0 x_2501), (true, #1 x_2501), (true, #2 x_2501))
  in
  let x_1271 x_1700 = let x_2691 = x_1270 ((true, x_1700), (false, 0), (false, 0)) in
                      snd (#0 x_2691) in
  let x_1272 x_1690 = let x_2717 = x_1270 ((false, 0), (true, x_1690), (false, 0)) in
                      snd (#1 x_2717) in
  let x_1273 x_1680 = let x_2743 = x_1270 ((false, 0), (false, 0), (true, x_1680)) in
                      snd (#2 x_2743) in
  let x_2746 = eq_1111 n_1020 in
  let x_2747 = x_2746 x_1270 in
  let x_1278 x_1668 = let x_2773 = (snd x_2747) ((true, x_1668), (false, 0), (false, 0)) in
                      snd (#0 x_2773) in
  let x_1279 x_1658 = let x_2801 = (snd x_2747) ((false, 0), (true, x_1658), (false, 0)) in
                      snd (#1 x_2801) in
  let x_1280 x_1648 = let x_2829 = (snd x_2747) ((false, 0), (false, 0), (true, x_1648)) in
                      snd (#2 x_2829) in
  if fst x_2747 then
    ()
  else
    {fail} ()
in
let x_2836 = rand_int () in
let x_2837 = main_1019 x_2836 in
let x_1283 = x_2837 in
()

tupling:
 let rec id_1008 (x_1009:int) = if x_1009 < 0 then
                                  0
                                else
                                  let x_2249 = id_1008 (x_1009 - 1) in
                                  1 + x_2249 in
 let rec succ_1010 (x_1011:int) = if x_1011 < 0 then
                                    1
                                  else
                                    let x_2256 = succ_1010 (x_1011 - 1) in
                                    1 + x_2256 in
 let rec double_1012 (x_1013:int) = if x_1013 < 0 then
                                      0
                                    else
                                      let x_2263 = double_1012 (x_1013 - 1) in
                                      2 + x_2263 in
 let rec id_succ_double_2160 (x_2157:int) (x_2158:int) (x_2159:int) =
   if x_2157 < 0 then
     if x_2158 < 0 then
       if x_2159 < 0 then
         (0, 1, 0)
       else
         let x_2358 = double_1012 (x_2159 - 1) in
         (0, 1, 2 + x_2358)
     else
       if x_2159 < 0 then
         let x_2347 = succ_1010 (x_2158 - 1) in
         (0, 1 + x_2347, 0)
       else
         let x_2333 = succ_1010 (x_2158 - 1) in
         let x_2338 = double_1012 (x_2159 - 1) in
         (0, 1 + x_2333, 2 + x_2338)
   else
     if x_2158 < 0 then
       if x_2159 < 0 then
         let x_2320 = id_1008 (x_2157 - 1) in
         (1 + x_2320, 1, 0)
       else
         let x_2306 = id_1008 (x_2157 - 1) in
         let x_2311 = double_1012 (x_2159 - 1) in
         (1 + x_2306, 1, 2 + x_2311)
     else
       if x_2159 < 0 then
         let x_2290 = id_1008 (x_2157 - 1) in
         let x_2295 = succ_1010 (x_2158 - 1) in
         (1 + x_2290, 1 + x_2295, 0)
       else
         let x_2274 = id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) in
         (1 + #0 x_2274, 1 + #1 x_2274, 2 + #2 x_2274)
 in
 let
   eq_1111 (x_1015:int) 
          (x_1069:(((bool * int) * (bool * int) * (bool * int)) -> ((bool * int) * (bool * int) * (bool * int)))) =
   let x_1253 (x_2069:int) = let x_2398 = x_1069 ((true, x_2069), (false, 0), (false, 0)) in
                             snd (#0 x_2398) in
   let x_1254 (x_2059:int) = let x_2424 = x_1069 ((false, 0), (true, x_2059), (false, 0)) in
                             snd (#1 x_2424) in
   let x_1255 (x_2049:int) = let x_2450 = x_1069 ((false, 0), (false, 0), (true, x_2049)) in
                             snd (#2 x_2450) in
   let x_2472 = x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) in
   (snd (#0 x_2472) + snd (#1 x_2472) = snd (#2 x_2472) + 1, x_1069)
 in
 let main_1019 (n_1020:int) =
   let x_1270 (xxx_1729:((bool * int) * (bool * int) * (bool * int))) =
     if fst (#0 xxx_1729) = false then
       let x_2616 =
         if fst (#1 xxx_1729) = false then
           (false, 0)
         else
           let x_2622 = succ_1010 (snd (#1 xxx_1729)) in
           (true, x_2622)
       in
       let x_2637 =
         if fst (#2 xxx_1729) = false then
           (false, 0)
         else
           let x_2643 = double_1012 (snd (#2 xxx_1729)) in
           (true, x_2643)
       in
       ((false, 0), x_2616, x_2637)
     else
       if fst (#1 xxx_1729) = false then
         let x_2569 = id_1008 (snd (#0 xxx_1729)) in
         let x_2579 =
           if fst (#2 xxx_1729) = false then
             (false, 0)
           else
             let x_2585 = double_1012 (snd (#2 xxx_1729)) in
             (true, x_2585)
         in
         ((true, x_2569), (false, 0), x_2579)
       else
         if fst (#2 xxx_1729) = false then
           let x_2533 = id_1008 (snd (#0 xxx_1729)) in
           let x_2543 = succ_1010 (snd (#1 xxx_1729)) in
           ((true, x_2533), (true, x_2543), (false, 0))
         else
           let x_2501 = id_succ_double_2160 (snd (#0 xxx_1729)) (snd (#1 xxx_1729)) (snd (#2 xxx_1729)) in
           ((true, #0 x_2501), (true, #1 x_2501), (true, #2 x_2501))
   in
   let x_1271 (x_1700:int) = let x_2691 = x_1270 ((true, x_1700), (false, 0), (false, 0)) in
                             snd (#0 x_2691) in
   let x_1272 (x_1690:int) = let x_2717 = x_1270 ((false, 0), (true, x_1690), (false, 0)) in
                             snd (#1 x_2717) in
   let x_1273 (x_1680:int) = let x_2743 = x_1270 ((false, 0), (false, 0), (true, x_1680)) in
                             snd (#2 x_2743) in
   let x_2746 = eq_1111 n_1020 in
   let x_2747 = x_2746 x_1270 in
   let x_1278 (x_1668:int) = let x_2773 = (snd x_2747) ((true, x_1668), (false, 0), (false, 0)) in
                             snd (#0 x_2773) in
   let x_1279 (x_1658:int) = let x_2801 = (snd x_2747) ((false, 0), (true, x_1658), (false, 0)) in
                             snd (#1 x_2801) in
   let x_1280 (x_1648:int) = let x_2829 = (snd x_2747) ((false, 0), (false, 0), (true, x_1648)) in
                             snd (#2 x_2829) in
   if fst x_2747 then
     ()
   else
     {fail} ()
 in
 let x_2836 = rand_int () in
 let x_2837 = main_1019 x_2836 in
 let x_1283 = x_2837 in
 ()

CPS:
 let rec id_1008 (x_1009:int) (k_id_2848:(int -> X)) =
   if x_1009 < 0 then
     k_id_2848 0
   else
     let x_2249 (k_id_x_2855:(int -> X)) = id_1008 (x_1009 - 1) k_id_x_2855 in
     x_2249 (fun (x_2861:int) -> k_id_2848 (1 + x_2861))
 in
 let rec succ_1010 (x_1011:int) (k_succ_2872:(int -> X)) =
   if x_1011 < 0 then
     k_succ_2872 1
   else
     let x_2256 (k_succ_x_2879:(int -> X)) = succ_1010 (x_1011 - 1) k_succ_x_2879 in
     x_2256 (fun (x_2885:int) -> k_succ_2872 (1 + x_2885))
 in
 let rec double_1012 (x_1013:int) (k_double_2896:(int -> X)) =
   if x_1013 < 0 then
     k_double_2896 0
   else
     let x_2263 (k_double_x_2903:(int -> X)) = double_1012 (x_1013 - 1) k_double_x_2903 in
     x_2263 (fun (x_2909:int) -> k_double_2896 (2 + x_2909))
 in
 let rec id_succ_double_2160 (x_2157:int) (x_2158:int) (x_2159:int) (k_id_succ_double_2920:((int * int * int) -> X)) =
   if x_2157 < 0 then
     if x_2158 < 0 then
       if x_2159 < 0 then
         k_id_succ_double_2920 (0, 1, 0)
       else
         let x_2358 (k_id_succ_double_x_2935:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_2935 in
         x_2358 (fun (x_2949:int) -> k_id_succ_double_2920 (0, 1, 2 + x_2949))
     else
       if x_2159 < 0 then
         let x_2347 (k_id_succ_double_x_2960:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_2960 in
         x_2347 (fun (x_2974:int) -> k_id_succ_double_2920 (0, 1 + x_2974, 0))
       else
         let x_2333 (k_id_succ_double_x_2981:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_2981 in
         x_2333
           (fun (x_3008:int) ->
              (let x_2338 (k_id_succ_double_x_2993:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_2993 in
               x_2338 (fun (x_3007:int) -> k_id_succ_double_2920 (0, 1 + x_3008, 2 + x_3007))))
   else
     if x_2158 < 0 then
       if x_2159 < 0 then
         let x_2320 (k_id_succ_double_x_3023:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3023 in
         x_2320 (fun (x_3037:int) -> k_id_succ_double_2920 (1 + x_3037, 1, 0))
       else
         let x_2306 (k_id_succ_double_x_3044:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3044 in
         x_2306
           (fun (x_3071:int) ->
              (let x_2311 (k_id_succ_double_x_3056:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_3056 in
               x_2311 (fun (x_3070:int) -> k_id_succ_double_2920 (1 + x_3071, 1, 2 + x_3070))))
     else
       if x_2159 < 0 then
         let x_2290 (k_id_succ_double_x_3082:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3082 in
         x_2290
           (fun (x_3109:int) ->
              (let x_2295 (k_id_succ_double_x_3094:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_3094 in
               x_2295 (fun (x_3108:int) -> k_id_succ_double_2920 (1 + x_3109, 1 + x_3108, 0))))
       else
         let x_2274 (k_id_succ_double_x_3118:((int * int * int) -> X)) =
           id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118
         in
         x_2274 (fun (x_3132:(int * int * int)) -> k_id_succ_double_2920 (1 + #0 x_3132, 1 + #1 x_3132, 2 + #2 x_3132))
 in
 let
   eq_1111 (x_1015:int) 
          (x_1069:(((bool * int) * (bool * int) * (bool * int)) ->
                     (((bool * int) * (bool * int) * (bool * int)) -> X) -> X))
          (k_eq_3159:((bool * 
                       (((bool * int) * (bool * int) * (bool * int)) ->
                          (((bool * int) * (bool * int) * (bool * int)) -> X) -> X)) -> X)) =
   let x_2472 (k_eq_x_3342:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     x_1069 ((true, x_1015), (true, x_1015), (true, x_1015)) k_eq_x_3342
   in
   x_2472
     (fun (x_3363:((bool * int) * (bool * int) * (bool * int))) ->
        k_eq_3159 (snd (#0 x_3363) + snd (#1 x_3363) = snd (#2 x_3363) + 1, x_1069))
 in
 let main_1019 (n_1020:int) (k_main_3389:(unit -> X)) =
   let
     x_1270 (xxx_1729:((bool * int) * (bool * int) * (bool * int))) 
           (k_main_x_3393:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     if fst (#0 xxx_1729) = false then
       let x_2616 (k_main_x_x_3422:((bool * int) -> X)) =
         if fst (#1 xxx_1729) = false then
           k_main_x_x_3422 (false, 0)
         else
           let x_2622 (k_main_x_x_x_3408:(int -> X)) = succ_1010 (snd (#1 xxx_1729)) k_main_x_x_x_3408 in
           x_2622 (fun (x_3420:int) -> k_main_x_x_3422 (true, x_3420))
       in
       x_2616
         (fun (x_3471:(bool * int)) ->
            (let x_2637 (k_main_x_x_3453:((bool * int) -> X)) =
               if fst (#2 xxx_1729) = false then
                 k_main_x_x_3453 (false, 0)
               else
                 let x_2643 (k_main_x_x_x_3439:(int -> X)) = double_1012 (snd (#2 xxx_1729)) k_main_x_x_x_3439 in
                 x_2643 (fun (x_3451:int) -> k_main_x_x_3453 (true, x_3451))
             in
             x_2637 (fun (x_3470:(bool * int)) -> k_main_x_3393 ((false, 0), x_3471, x_3470))))
     else
       if fst (#1 xxx_1729) = false then
         let x_2569 (k_main_x_x_3478:(int -> X)) = id_1008 (snd (#0 xxx_1729)) k_main_x_x_3478 in
         x_2569
           (fun (x_3536:int) ->
              (let x_2579 (k_main_x_x_3512:((bool * int) -> X)) =
                 if fst (#2 xxx_1729) = false then
                   k_main_x_x_3512 (false, 0)
                 else
                   let x_2585 (k_main_x_x_x_3498:(int -> X)) = double_1012 (snd (#2 xxx_1729)) k_main_x_x_x_3498 in
                   x_2585 (fun (x_3510:int) -> k_main_x_x_3512 (true, x_3510))
               in
               x_2579 (fun (x_3535:(bool * int)) -> k_main_x_3393 ((true, x_3536), (false, 0), x_3535))))
       else
         if fst (#2 xxx_1729) = false then
           let x_2533 (k_main_x_x_3543:(int -> X)) = id_1008 (snd (#0 xxx_1729)) k_main_x_x_3543 in
           x_2533
             (fun (x_3588:int) ->
                (let x_2543 (k_main_x_x_3555:(int -> X)) = succ_1010 (snd (#1 xxx_1729)) k_main_x_x_3555 in
                 x_2543 (fun (x_3587:int) -> k_main_x_3393 ((true, x_3588), (true, x_3587), (false, 0)))))
         else
           let x_2501 (k_main_x_x_3597:((int * int * int) -> X)) =
             id_succ_double_2160 (snd (#0 xxx_1729)) (snd (#1 xxx_1729)) (snd (#2 xxx_1729)) k_main_x_x_3597
           in
           x_2501
             (fun (x_3629:(int * int * int)) ->
                k_main_x_3393 ((true, #0 x_3629), (true, #1 x_3629), (true, #2 x_3629)))
   in
   let
     x_2747
           (k_main_x_3822:((bool * 
                            (((bool * int) * (bool * int) * (bool * int)) ->
                               (((bool * int) * (bool * int) * (bool * int)) -> X) -> X)) -> X)) =
     (eq_1111 n_1020) x_1270 k_main_x_3822
   in
   x_2747
     (fun (x_4007:(bool * 
                   (((bool * int) * (bool * int) * (bool * int)) ->
                      (((bool * int) * (bool * int) * (bool * int)) -> X) -> X))) ->
        (if fst x_4007 then
           k_main_3389 ()
         else
           {|fail|} () k_main_3389))
 in
 let x_2836 (k_x_4043:(int -> X)) = rand_int_cps () k_x_4043 in
 x_2836
   (fun (x_4062:int) ->
      (let x_2837 (k_x_4055:(unit -> X)) = main_1019 x_4062 k_x_4055 in
       x_2837 (fun (x_4061:unit) -> {end})))

remove_pair:
 let rec id_1008 (x_1009:int) (k_id_2848:(int -> X)) =
   if x_1009 < 0 then
     k_id_2848 0
   else
     let x_2249 (k_id_x_2855:(int -> X)) = id_1008 (x_1009 - 1) k_id_x_2855 in
     x_2249 (fun (x_2861:int) -> k_id_2848 (1 + x_2861))
 in
 let rec succ_1010 (x_1011:int) (k_succ_2872:(int -> X)) =
   if x_1011 < 0 then
     k_succ_2872 1
   else
     let x_2256 (k_succ_x_2879:(int -> X)) = succ_1010 (x_1011 - 1) k_succ_x_2879 in
     x_2256 (fun (x_2885:int) -> k_succ_2872 (1 + x_2885))
 in
 let rec double_1012 (x_1013:int) (k_double_2896:(int -> X)) =
   if x_1013 < 0 then
     k_double_2896 0
   else
     let x_2263 (k_double_x_2903:(int -> X)) = double_1012 (x_1013 - 1) k_double_x_2903 in
     x_2263 (fun (x_2909:int) -> k_double_2896 (2 + x_2909))
 in
 let rec id_succ_double_2160 (x_2157:int) (x_2158:int) (x_2159:int) (k_id_succ_double_2920:(int -> int -> int -> X)) =
   if x_2157 < 0 then
     if x_2158 < 0 then
       if x_2159 < 0 then
         k_id_succ_double_2920 0 1 0
       else
         let x_2358 (k_id_succ_double_x_2935:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_2935 in
         x_2358 (fun (x_2949:int) -> k_id_succ_double_2920 0 1 (2 + x_2949))
     else
       if x_2159 < 0 then
         let x_2347 (k_id_succ_double_x_2960:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_2960 in
         x_2347 (fun (x_2974:int) -> k_id_succ_double_2920 0 (1 + x_2974) 0)
       else
         let x_2333 (k_id_succ_double_x_2981:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_2981 in
         x_2333
           (fun (x_3008:int) ->
              (let x_2338 (k_id_succ_double_x_2993:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_2993 in
               x_2338 (fun (x_3007:int) -> k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007))))
   else
     if x_2158 < 0 then
       if x_2159 < 0 then
         let x_2320 (k_id_succ_double_x_3023:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3023 in
         x_2320 (fun (x_3037:int) -> k_id_succ_double_2920 (1 + x_3037) 1 0)
       else
         let x_2306 (k_id_succ_double_x_3044:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3044 in
         x_2306
           (fun (x_3071:int) ->
              (let x_2311 (k_id_succ_double_x_3056:(int -> X)) = double_1012 (x_2159 - 1) k_id_succ_double_x_3056 in
               x_2311 (fun (x_3070:int) -> k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070))))
     else
       if x_2159 < 0 then
         let x_2290 (k_id_succ_double_x_3082:(int -> X)) = id_1008 (x_2157 - 1) k_id_succ_double_x_3082 in
         x_2290
           (fun (x_3109:int) ->
              (let x_2295 (k_id_succ_double_x_3094:(int -> X)) = succ_1010 (x_2158 - 1) k_id_succ_double_x_3094 in
               x_2295 (fun (x_3108:int) -> k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0)))
       else
         let x_2274 (k_id_succ_double_x_3118:(int -> int -> int -> X)) =
           id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118
         in
         x_2274
           (fun (x0_3132:int) ->
              fun (x1_3132:int) -> fun (x2_3132:int) -> k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132))
 in
 let
   eq_1111 (x_1015:int) 
          (x_1069:(bool -> int -> bool -> int -> bool -> int -> (bool -> int -> bool -> int -> bool -> int -> X) -> X)) 
          (k_eq_3159:(bool ->
                        (bool ->
                           int -> bool -> int -> bool -> int -> (bool -> int -> bool -> int -> bool -> int -> X) -> X)
                          -> X)) =
   let x_2472 (k_eq_x_3342:(bool -> int -> bool -> int -> bool -> int -> X)) =
     x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342
   in
   x_2472
     (fun (x00_3363:bool) ->
        fun (x01_3363:int) ->
          fun (x10_3363:bool) ->
            fun (x11_3363:int) ->
              fun (x20_3363:bool) -> fun (x21_3363:int) -> k_eq_3159 (x01_3363 + x11_3363 = x21_3363 + 1) x_1069)
 in
 let main_1019 (n_1020:int) (k_main_3389:(unit -> X)) =
   let
     x_1270 (xxx00_1729:bool) (xxx01_1729:int) (xxx10_1729:bool) (xxx11_1729:int) (xxx20_1729:bool) (xxx21_1729:int) 
           (k_main_x_3393:(bool -> int -> bool -> int -> bool -> int -> X)) =
     if xxx00_1729 = false then
       let x_2616 (k_main_x_x_3422:(bool -> int -> X)) =
         if xxx10_1729 = false then
           k_main_x_x_3422 false 0
         else
           let x_2622 (k_main_x_x_x_3408:(int -> X)) = succ_1010 xxx11_1729 k_main_x_x_x_3408 in
           x_2622 (fun (x_3420:int) -> k_main_x_x_3422 true x_3420)
       in
       x_2616
         (fun (x0_3471:bool) ->
            fun (x1_3471:int) ->
              (let x_2637 (k_main_x_x_3453:(bool -> int -> X)) =
                 if xxx20_1729 = false then
                   k_main_x_x_3453 false 0
                 else
                   let x_2643 (k_main_x_x_x_3439:(int -> X)) = double_1012 xxx21_1729 k_main_x_x_x_3439 in
                   x_2643 (fun (x_3451:int) -> k_main_x_x_3453 true x_3451)
               in
               x_2637
                 (fun (x0_3470:bool) -> fun (x1_3470:int) -> k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470)))
     else
       if xxx10_1729 = false then
         let x_2569 (k_main_x_x_3478:(int -> X)) = id_1008 xxx01_1729 k_main_x_x_3478 in
         x_2569
           (fun (x_3536:int) ->
              (let x_2579 (k_main_x_x_3512:(bool -> int -> X)) =
                 if xxx20_1729 = false then
                   k_main_x_x_3512 false 0
                 else
                   let x_2585 (k_main_x_x_x_3498:(int -> X)) = double_1012 xxx21_1729 k_main_x_x_x_3498 in
                   x_2585 (fun (x_3510:int) -> k_main_x_x_3512 true x_3510)
               in
               x_2579 (fun (x0_3535:bool) -> fun (x1_3535:int) -> k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535)))
       else
         if xxx20_1729 = false then
           let x_2533 (k_main_x_x_3543:(int -> X)) = id_1008 xxx01_1729 k_main_x_x_3543 in
           x_2533
             (fun (x_3588:int) ->
                (let x_2543 (k_main_x_x_3555:(int -> X)) = succ_1010 xxx11_1729 k_main_x_x_3555 in
                 x_2543 (fun (x_3587:int) -> k_main_x_3393 true x_3588 true x_3587 false 0)))
         else
           let x_2501 (k_main_x_x_3597:(int -> int -> int -> X)) =
             id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597
           in
           x_2501
             (fun (x0_3629:int) ->
                fun (x1_3629:int) -> fun (x2_3629:int) -> k_main_x_3393 true x0_3629 true x1_3629 true x2_3629)
   in
   let
     x_2747
           (k_main_x_3822:(bool ->
                             (bool ->
                                int ->
                                  bool -> int -> bool -> int -> (bool -> int -> bool -> int -> bool -> int -> X) -> X)
                               -> X)) = eq_1111 n_1020 x_1270 k_main_x_3822
   in
   x_2747
     (fun (x0_4007:bool) ->
        fun (x1_4007:(bool ->
                        int -> bool -> int -> bool -> int -> (bool -> int -> bool -> int -> bool -> int -> X) -> X)) ->
          (if x0_4007 then
             k_main_3389 ()
           else
             {|fail|} () k_main_3389))
 in
 let x_2836 (k_x_4043:(int -> X)) = rand_int_cps () k_x_4043 in
 x_2836
   (fun (x_4062:int) ->
      (let x_2837 (k_x_4055:(unit -> X)) = main_1019 x_4062 k_x_4055 in
       x_2837 (fun (x_4061:unit) -> {end})))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (int -> int -> int -> (int -> int -> int -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_4308 ... --> 
  x_2836 ... --> 
  f_4334 ... --> 
  x_2837 ... --> 
  main_1019 ... --> 
  x_2747 ... --> 
  eq_1111 ... --> 
  x_2472 ... --> 
  x_1270 [2/2] ... --> 
  br_x_4350 [2/2] ... --> 
  br_x_4348 [2/2] ... --> 
  x_2501 ... --> 
  id_succ_double_2160 [1/2] ... --> 
  br_id_succ_double_4340 [1/2] ... --> 
  br_id_succ_double_4336 [1/2] ... --> 
  f_x_4332 ... --> 
  f_eq_4322 ... --> 
  f_main_4333 [2/2] ... --> 
  fail_4352 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (int ->
                         int ->
                         int -> (x_5:int -> x_6:int -> x_7:int[1 >= x_5 + x_6 - x_7 && 1 <= x_5 + x_6 - x_7] -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_4308 ... --> 
  x_2836 ... --> 
  f_4334 ... --> 
  x_2837 ... --> 
  main_1019 ... --> 
  x_2747 ... --> 
  eq_1111 ... --> 
  x_2472 ... --> 
  x_1270 [2/2] ... --> 
  br_x_4350 [2/2] ... --> 
  br_x_4348 [2/2] ... --> 
  x_2501 ... --> 
  id_succ_double_2160 [2/2] ... --> 
  br_id_succ_double_4346 [2/2] ... --> 
  br_id_succ_double_4344 [2/2] ... --> 
  x_2274 ... --> 
  id_succ_double_2160 [1/2] ... --> 
  br_id_succ_double_4340 [1/2] ... --> 
  br_id_succ_double_4336 [2/2] ... --> 
  x_2358 ... --> 
  double_1012 [1/2] ... --> 
  f_id_succ_double_4312 ... --> 
  f_id_succ_double_4321 ... --> 
  f_x_4332 ... --> 
  f_eq_4322 ... --> 
  f_main_4333 [2/2] ... --> 
  fail_4352 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 1; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 1; 1; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (int ->
                         x_2:int ->
                         x_3:int[x_3 <= x_2] ->
                         (x_5:int -> x_6:int -> x_7:int[1 >= x_5 + x_6 - x_7 && 1 <= x_5 + x_6 - x_7] -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Error trace::
  main_4308 ... --> 
  x_2836 ... --> 
  f_4334 ... --> 
  x_2837 ... --> 
  main_1019 ... --> 
  x_2747 ... --> 
  eq_1111 ... --> 
  x_2472 ... --> 
  x_1270 [2/2] ... --> 
  br_x_4350 [2/2] ... --> 
  br_x_4348 [2/2] ... --> 
  x_2501 ... --> 
  id_succ_double_2160 [2/2] ... --> 
  br_id_succ_double_4346 [1/2] ... --> 
  br_id_succ_double_4342 [1/2] ... --> 
  x_2320 ... --> 
  id_1008 [1/2] ... --> 
  f_id_succ_double_4316 ... --> 
  f_x_4332 ... --> 
  f_eq_4322 ... --> 
  f_main_4333 [2/2] ... --> 
  fail_4352 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0

(2-3) Checking counterexample ... DONE!

(2-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 1; 0

Program with abstraction types (CEGAR-cycle 3)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (x_1:int ->
                         x_2:int ->
                         x_3:int[x_3 >= x_1; x_3 <= x_2] ->
                         (x_5:int -> x_6:int -> x_7:int[1 >= x_5 + x_6 - x_7 && 1 <= x_5 + x_6 - x_7] -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(3-1) Abstracting ... DONE!

(3-2) Checking HORS ... DONE!

Error trace::
  main_4308 ... --> 
  x_2836 ... --> 
  f_4334 ... --> 
  x_2837 ... --> 
  main_1019 ... --> 
  x_2747 ... --> 
  eq_1111 ... --> 
  x_2472 ... --> 
  x_1270 [2/2] ... --> 
  br_x_4350 [2/2] ... --> 
  br_x_4348 [2/2] ... --> 
  x_2501 ... --> 
  id_succ_double_2160 [1/2] ... --> 
  br_id_succ_double_4340 [2/2] ... --> 
  br_id_succ_double_4338 [1/2] ... --> 
  x_2347 ... --> 
  succ_1010 [1/2] ... --> 
  f_id_succ_double_4313 ... --> 
  f_x_4332 ... --> 
  f_eq_4322 ... --> 
  f_main_4333 [2/2] ... --> 
  fail_4352 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(3-3) Checking counterexample ... DONE!

(3-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1

Program with abstraction types (CEGAR-cycle 4)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (x_1:int ->
                         x_2:int ->
                         x_3:int[x_3 >= x_2; x_3 >= x_1; x_3 <= x_2] ->
                         (x_5:int -> x_6:int -> x_7:int[1 >= x_5 + x_6 - x_7 && 1 <= x_5 + x_6 - x_7] -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(4-1) Abstracting ... DONE!

(4-2) Checking HORS ... DONE!

Error trace::
  main_4308 ... --> 
  x_2836 ... --> 
  f_4334 ... --> 
  x_2837 ... --> 
  main_1019 ... --> 
  x_2747 ... --> 
  eq_1111 ... --> 
  x_2472 ... --> 
  x_1270 [2/2] ... --> 
  br_x_4350 [2/2] ... --> 
  br_x_4348 [2/2] ... --> 
  x_2501 ... --> 
  id_succ_double_2160 [1/2] ... --> 
  br_id_succ_double_4340 [2/2] ... --> 
  br_id_succ_double_4338 [2/2] ... --> 
  x_2333 ... --> 
  succ_1010 [1/2] ... --> 
  f_id_succ_double_4314 ... --> 
  x_2338 ... --> 
  double_1012 [1/2] ... --> 
  f_id_succ_double_4315 ... --> 
  f_x_4332 ... --> 
  f_eq_4322 ... --> 
  f_main_4333 [2/2] ... --> 
  fail_4352 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(4-3) Checking counterexample ... DONE!

(4-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 1

Program with abstraction types (CEGAR-cycle 5)::
Main: main_4308
  main_4308 -> (x_2836 f_4334).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4337 ->
      (k_id_succ_double_2920 0 1 0).
  br_id_succ_double_4336 b_4337 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4337) -> (x_2358 x_2157 x_2158 x_2159 (f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4339 ->
      (x_2347 x_2157 x_2158 x_2159 (f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4338 b_4339 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4339) -> (x_2333 x_2157 x_2158 x_2159 (f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4341 ->
      (br_id_succ_double_4336 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4340 b_4341 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4341) -> (br_id_succ_double_4338 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4343 ->
      (x_2320 x_2157 x_2158 x_2159 (f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4342 b_4343 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4343) -> (x_2306 x_2157 x_2158 x_2159 (f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4345 ->
      (x_2290 x_2157 x_2158 x_2159 (f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4344 b_4345 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4345) -> (x_2274 x_2157 x_2158 x_2159 (f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920)).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when b_4347 ->
      (br_id_succ_double_4342 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_id_succ_double_4346 b_4347 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not b_4347) -> (br_id_succ_double_4344 (x_2159 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4349 ->
      (x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4348 b_4349 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4349) ->
      (x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when b_4351 ->
      (x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  br_x_4350 b_4351 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not b_4351) ->
      (br_x_4348 (xxx20_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  double_1012 x_1013 k_double_2896 when (x_1013 < 0) -> (k_double_2896 0).
  double_1012 x_1013 k_double_2896 when (not (x_1013 < 0)) -> (x_2263 x_1013 (f_double_4311 x_1013 k_double_2896)).
  eq_1111 x_1015 x_1069 k_eq_3159 -> (x_2472 x_1015 x_1069 (f_eq_4322 x_1015 k_eq_3159 x_1069)).
  f_4334 x_4062 -> (x_2837 x_4062 (f_4335 x_4062)).
  f_4335 x_4062 x_4061 -> end.
  f_double_4311 x_1013 k_double_2896 x_2909 -> (k_double_2896 (2 + x_2909)).
  f_eq_4322 x_1015 k_eq_3159 x_1069 x00_3363 x01_3363 x10_3363 x11_3363 x20_3363 x21_3363 ->
      (k_eq_3159 ((x01_3363 + x11_3363) = (x21_3363 + 1)) x_1069).
  f_id_4309 x_1009 k_id_2848 x_2861 -> (k_id_2848 (1 + x_2861)).
  f_id_succ_double_4312 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2949 -> (k_id_succ_double_2920 0 1 (2 + x_2949)).
  f_id_succ_double_4313 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_2974 -> (k_id_succ_double_2920 0 (1 + x_2974) 0).
  f_id_succ_double_4314 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3008 ->
      (x_2338 x_2157 x_2158 x_2159 x_3008 (f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920)).
  f_id_succ_double_4315 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_2920 x_3007 ->
      (k_id_succ_double_2920 0 (1 + x_3008) (2 + x_3007)).
  f_id_succ_double_4316 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3037 -> (k_id_succ_double_2920 (1 + x_3037) 1 0).
  f_id_succ_double_4317 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3071 ->
      (x_2311 x_2157 x_2158 x_2159 x_3071 (f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920)).
  f_id_succ_double_4318 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_2920 x_3070 ->
      (k_id_succ_double_2920 (1 + x_3071) 1 (2 + x_3070)).
  f_id_succ_double_4319 x_2157 x_2158 x_2159 k_id_succ_double_2920 x_3109 ->
      (x_2295 x_2157 x_2158 x_2159 x_3109 (f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920)).
  f_id_succ_double_4320 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_2920 x_3108 ->
      (k_id_succ_double_2920 (1 + x_3109) (1 + x_3108) 0).
  f_id_succ_double_4321 x_2157 x_2158 x_2159 k_id_succ_double_2920 x0_3132 x1_3132 x2_3132 ->
      (k_id_succ_double_2920 (1 + x0_3132) (1 + x1_3132) (2 + x2_3132)).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when x0_4007 -> (k_main_3389 ()).
  f_main_4333 n_1020 k_main_3389 x0_4007 x1_4007 when (not x0_4007) -> (fail_4352 true k_main_3389).
  f_succ_4310 x_1011 k_succ_2872 x_2885 -> (k_succ_2872 (1 + x_2885)).
  f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 x_3420 ->
      (k_main_x_x_3422 true x_3420).
  f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3471 x1_3471 ->
      (x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_3393)).
  f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 
  x_3451 -> (k_main_x_x_3453 true x_3451).
  f_x_4326 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 
  x0_3470 x1_3470 -> (k_main_x_3393 false 0 x0_3471 x1_3471 x0_3470 x1_3470).
  f_x_4327 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3536 ->
      (x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 x_3510 ->
      (k_main_x_x_3512 true x_3510).
  f_x_4329 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3535 
  x1_3535 -> (k_main_x_3393 true x_3536 false 0 x0_3535 x1_3535).
  f_x_4330 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3588 ->
      (x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  f_x_4331 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x_3587 ->
      (k_main_x_3393 true x_3588 true x_3587 false 0).
  f_x_4332 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 x0_3629 x1_3629 
  x2_3629 -> (k_main_x_3393 true x0_3629 true x1_3629 true x2_3629).
  fail_4352 b k -> {fail} => (k ()).
  id_1008 x_1009 k_id_2848 when (x_1009 < 0) -> (k_id_2848 0).
  id_1008 x_1009 k_id_2848 when (not (x_1009 < 0)) -> (x_2249 x_1009 (f_id_4309 x_1009 k_id_2848)).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      x_2157 < 0) -> (br_id_succ_double_4340 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  id_succ_double_2160 x_2157 x_2158 x_2159 k_id_succ_double_2920 when (
      not (x_2157 < 0)) -> (br_id_succ_double_4346 (x_2158 < 0) x_2157 x_2158 x_2159 k_id_succ_double_2920).
  main_1019 n_1020 k_main_3389 -> (x_2747 n_1020 (f_main_4333 n_1020 k_main_3389)).
  succ_1010 x_1011 k_succ_2872 when (x_1011 < 0) -> (k_succ_2872 1).
  succ_1010 x_1011 k_succ_2872 when (not (x_1011 < 0)) -> (x_2256 x_1011 (f_succ_4310 x_1011 k_succ_2872)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      xxx00_1729 <=> false) ->
      (x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4324 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393)).
  x_1270 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_3393 when (
      not (xxx00_1729 <=> false)) ->
      (br_x_4350 (xxx10_1729 <=> false) n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        k_main_x_3393).
  x_2249 x_1009 k_id_x_2855 -> (id_1008 (x_1009 - 1) k_id_x_2855).
  x_2256 x_1011 k_succ_x_2879 -> (succ_1010 (x_1011 - 1) k_succ_x_2879).
  x_2263 x_1013 k_double_x_2903 -> (double_1012 (x_1013 - 1) k_double_x_2903).
  x_2274 x_2157 x_2158 x_2159 k_id_succ_double_x_3118 ->
      (id_succ_double_2160 (x_2157 - 1) (x_2158 - 1) (x_2159 - 1) k_id_succ_double_x_3118).
  x_2290 x_2157 x_2158 x_2159 k_id_succ_double_x_3082 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3082).
  x_2295 x_2157 x_2158 x_2159 x_3109 k_id_succ_double_x_3094 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_3094).
  x_2306 x_2157 x_2158 x_2159 k_id_succ_double_x_3044 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3044).
  x_2311 x_2157 x_2158 x_2159 x_3071 k_id_succ_double_x_3056 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_3056).
  x_2320 x_2157 x_2158 x_2159 k_id_succ_double_x_3023 -> (id_1008 (x_2157 - 1) k_id_succ_double_x_3023).
  x_2333 x_2157 x_2158 x_2159 k_id_succ_double_x_2981 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2981).
  x_2338 x_2157 x_2158 x_2159 x_3008 k_id_succ_double_x_2993 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2993).
  x_2347 x_2157 x_2158 x_2159 k_id_succ_double_x_2960 -> (succ_1010 (x_2158 - 1) k_id_succ_double_x_2960).
  x_2358 x_2157 x_2158 x_2159 k_id_succ_double_x_2935 -> (double_1012 (x_2159 - 1) k_id_succ_double_x_2935).
  x_2472 x_1015 x_1069 k_eq_x_3342 -> (x_1069 true x_1015 true x_1015 true x_1015 k_eq_x_3342).
  x_2501 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3597 ->
      (id_succ_double_2160 xxx01_1729 xxx11_1729 xxx21_1729 k_main_x_x_3597).
  x_2533 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3543 ->
      (id_1008 xxx01_1729 k_main_x_x_3543).
  x_2543 n_1020 x_3588 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3555 ->
      (succ_1010 xxx11_1729 k_main_x_x_3555).
  x_2569 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3478 ->
      (id_1008 xxx01_1729 k_main_x_x_3478).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3512 false 0).
  x_2579 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512 when (
      not (xxx20_1729 <=> false)) ->
      (x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4328 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3512)).
  x_2585 n_1020 x_3536 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3498 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3498).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      xxx10_1729 <=> false) -> (k_main_x_x_3422 false 0).
  x_2616 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422 when (
      not (xxx10_1729 <=> false)) ->
      (x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4323 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3422)).
  x_2622 n_1020 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3408 ->
      (succ_1010 xxx11_1729 k_main_x_x_x_3408).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      xxx20_1729 <=> false) -> (k_main_x_x_3453 false 0).
  x_2637 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_3453 when (
      not (xxx20_1729 <=> false)) ->
      (x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
        (f_x_4325 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729
          k_main_x_x_3453)).
  x_2643 n_1020 x0_3471 x1_3471 xxx00_1729 xxx01_1729 xxx10_1729 xxx11_1729 xxx20_1729 xxx21_1729 k_main_x_x_x_3439 ->
      (double_1012 xxx21_1729 k_main_x_x_x_3439).
  x_2747 n_1020 k_main_x_3822 -> (eq_1111 n_1020 (x_1270 n_1020) k_main_x_3822).
  x_2836 k_x_4043 -> (rand_int k_x_4043).
  x_2837 x_4062 k_x_4055 -> (main_1019 x_4062 k_x_4055).
Types:
  main_4308 : X
  double_1012 : (int -> (int -> X) -> X)
  fail_4352 : (x_1:bool[x_1] -> (unit -> X) -> X)
  id_1008 : (int -> (int -> X) -> X)
  id_succ_double_2160 : (x_1:int ->
                         x_2:int ->
                         x_3:int[x_3 <= x_1; x_3 >= x_2; x_3 >= x_1; x_3 <= x_2] ->
                         (x_5:int -> x_6:int -> x_7:int[1 >= x_5 + x_6 - x_7 && 1 <= x_5 + x_6 - x_7] -> X) -> X)
  succ_1010 : (int -> (int -> X) -> X)

(5-1) Abstracting ... DONE!

(5-2) Checking HORS ... DONE!

Safe!

cycles: 5
total: 1.147 sec
  abst: 0.316 sec
  mc: 0.058 sec
  refine: 0.494 sec
    exparam: 0.241 sec
