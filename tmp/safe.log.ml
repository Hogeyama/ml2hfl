MoCHi: Model Checker for Higher-Order Programs
  Build: _a06df01 (after 2014-03-24 17:09:25 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt -tupling safe.ml -disable-rc -color

parsed::
 let rec f_1030 x_1031 = if x_1031 <= 0 then
                           0
                         else
                           1 + f_1030 (x_1031 - 1)
 in
 let rec g_1032 x_1033 = if x_1033 <= 0 then
                           0
                         else
                           1 + g_1032 (x_1033 - 1)
 in
 let rec fg_1034 x_1035 y_1036 =
   if x_1035 <= 0 then
     (0, g_1032 y_1036)
   else
     if y_1036 <= 0 then
       (f_1030 x_1035, 0)
     else
       (match fg_1034 (x_1035 - 1) (y_1036 - 1) with
          | (r1_1037, r2_1038) -> (1 + r1_1037, 1 + r2_1038)
          | _ -> let u_1095 = {fail}
                 in
                   _|_)
 in
 let rec fg'_1039 x_1040 y_1041 =
   if x_1040 <= 0 then
     (0, g_1032 y_1041)
   else
     if y_1041 <= 0 then
       (f_1030 x_1040, 0)
     else
       (match fg'_1039 (x_1040 - 1) (y_1041 - 1) with
          | (r1_1042, r2_1043) -> (1 + r1_1042, 1 + r2_1043)
          | _ -> let u_1142 = {fail}
                 in
                   _|_)
 in
 let rec id_1044 x_1045 = if x_1045 <= 0 then
                            x_1045
                          else
                            1 + id_1044 (x_1045 - 1)
 in
 let main_1046 n_1047 =
   if n_1047 < 0 then
     ()
   else
     let n'_1048 = id_1044 n_1047
     in
       (match fg_1034 0 n'_1048 with
          | (r1_1049, r2_1050) ->
              (match fg_1034 n'_1048 n'_1048 with
                 | (r1'_1051, r2'_1052) -> if r1'_1051 = r2'_1052 then
                                             ()
                                           else
                                             {fail} ()
                 | _ -> let u_1179 = {fail}
                        in
                          _|_)
          | _ -> let u_1180 = {fail}
                 in
                   _|_)
 in
   ()

set_target::
 let rec f_1030 x_1031 = if x_1031 <= 0 then
                           0
                         else
                           1 + f_1030 (x_1031 - 1)
 in
 let rec g_1032 x_1033 = if x_1033 <= 0 then
                           0
                         else
                           1 + g_1032 (x_1033 - 1)
 in
 let rec fg_1034 x_1035 y_1036 =
   if x_1035 <= 0 then
     (0, g_1032 y_1036)
   else
     if y_1036 <= 0 then
       (f_1030 x_1035, 0)
     else
       (match fg_1034 (x_1035 - 1) (y_1036 - 1) with
          | (r1_1037, r2_1038) -> (1 + r1_1037, 1 + r2_1038)
          | _ -> let u_1095 = {fail}
                 in
                   _|_)
 in
 let rec fg'_1039 x_1040 y_1041 =
   if x_1040 <= 0 then
     (0, g_1032 y_1041)
   else
     if y_1041 <= 0 then
       (f_1030 x_1040, 0)
     else
       (match fg'_1039 (x_1040 - 1) (y_1041 - 1) with
          | (r1_1042, r2_1043) -> (1 + r1_1042, 1 + r2_1043)
          | _ -> let u_1142 = {fail}
                 in
                   _|_)
 in
 let rec id_1044 x_1045 = if x_1045 <= 0 then
                            x_1045
                          else
                            1 + id_1044 (x_1045 - 1)
 in
 let main_1046 n_1047 =
   if n_1047 < 0 then
     ()
   else
     let n'_1048 = id_1044 n_1047
     in
       (match fg_1034 0 n'_1048 with
          | (r1_1049, r2_1050) ->
              (match fg_1034 n'_1048 n'_1048 with
                 | (r1'_1051, r2'_1052) -> if r1'_1051 = r2'_1052 then
                                             ()
                                           else
                                             {fail} ()
                 | _ -> let u_1179 = {fail}
                        in
                          _|_)
          | _ -> let u_1180 = {fail}
                 in
                   _|_)
 in
 let main_1182 = let arg1_1181 = rand_int ()
                 in
                   main_1046 arg1_1181
 in
   ()

encode_list:
 let rec f_1030 (x_1031:int) = if x_1031 <= 0 then
                                 0
                               else
                                 1 + f_1030 (x_1031 - 1)
 in
 let rec g_1032 (x_1033:int) = if x_1033 <= 0 then
                                 0
                               else
                                 1 + g_1032 (x_1033 - 1)
 in
 let rec fg_1034 (x_1035:int) (y_1036:int) =
   if x_1035 <= 0 then
     (0, g_1032 y_1036)
   else
     if y_1036 <= 0 then
       (f_1030 x_1035, 0)
     else
       let xs_1240 = fg_1034 (x_1035 - 1) (y_1036 - 1)
       in
       let r2_1038 = snd xs_1240
       in
       let r1_1037 = fst xs_1240
       in
         (1 + r1_1037, 1 + r2_1038)
 in
 let rec fg'_1039 (x_1040:int) (y_1041:int) =
   if x_1040 <= 0 then
     (0, g_1032 y_1041)
   else
     if y_1041 <= 0 then
       (f_1030 x_1040, 0)
     else
       let xs_1246 = fg'_1039 (x_1040 - 1) (y_1041 - 1)
       in
       let r2_1043 = snd xs_1246
       in
       let r1_1042 = fst xs_1246
       in
         (1 + r1_1042, 1 + r2_1043)
 in
 let rec id_1044 (x_1045:int) = if x_1045 <= 0 then
                                  x_1045
                                else
                                  1 + id_1044 (x_1045 - 1)
 in
 let main_1046 (n_1047:int) =
   if n_1047 < 0 then
     ()
   else
     let n'_1048 = id_1044 n_1047
     in
     let xs_1252 = fg_1034 0 n'_1048
     in
     let r2_1050 = snd xs_1252
     in
     let r1_1049 = fst xs_1252
     in
     let xs_1253 = fg_1034 n'_1048 n'_1048
     in
     let r2'_1052 = snd xs_1253
     in
     let r1'_1051 = fst xs_1253
     in
       if r1'_1051 = r2'_1052 then
         ()
       else
         {fail} ()
 in
 let main_1182 = let arg1_1181 = rand_int ()
                 in
                   main_1046 arg1_1181
 in
   ()

inlined:
 let rec f_1030 (x_1031:int) = if x_1031 <= 0 then
                                 0
                               else
                                 1 + f_1030 (x_1031 - 1)
 in
 let rec g_1032 (x_1033:int) = if x_1033 <= 0 then
                                 0
                               else
                                 1 + g_1032 (x_1033 - 1)
 in
 let rec fg_1034 (x_1035:int) (y_1036:int) =
   if x_1035 <= 0 then
     (0, g_1032 y_1036)
   else
     if y_1036 <= 0 then
       (f_1030 x_1035, 0)
     else
       let xs_1240 = fg_1034 (x_1035 - 1) (y_1036 - 1)
       in
         (1 + fst xs_1240, 1 + snd xs_1240)
 in
 let rec fg'_1039 (x_1040:int) (y_1041:int) =
   if x_1040 <= 0 then
     (0, g_1032 y_1041)
   else
     if y_1041 <= 0 then
       (f_1030 x_1040, 0)
     else
       let xs_1246 = fg'_1039 (x_1040 - 1) (y_1041 - 1)
       in
         (1 + fst xs_1246, 1 + snd xs_1246)
 in
 let rec id_1044 (x_1045:int) = if x_1045 <= 0 then
                                  x_1045
                                else
                                  1 + id_1044 (x_1045 - 1)
 in
 let main_1046 (n_1047:int) =
   if n_1047 < 0 then
     ()
   else
     let n'_1048 = id_1044 n_1047
     in
     let xs_1252 = fg_1034 0 n'_1048
     in
     let xs_1253 = fg_1034 n'_1048 n'_1048
     in
       if fst xs_1253 = snd xs_1253 then
         ()
       else
         {fail} ()
 in
 let main_1182 = let arg1_1181 = rand_int ()
                 in
                   main_1046 arg1_1181
 in
   ()

CPS:
 let rec f_1030 (x_1031:int) (k_f_1276:(int -> X)) =
   if x_1031 <= 0 then
     k_f_1276 0
   else
     f_1030 (x_1031 - 1) (fun x_1280 -> k_f_1276 (1 + x_1280))
 in
 let rec g_1032 (x_1033:int) (k_g_1308:(int -> X)) =
   if x_1033 <= 0 then
     k_g_1308 0
   else
     g_1032 (x_1033 - 1) (fun x_1312 -> k_g_1308 (1 + x_1312))
 in
 let rec fg_1034 (x_1035:int) (y_1036:int) (k_fg_1343:((int * int) -> X)) =
   if x_1035 <= 0 then
     g_1032 y_1036 (fun x_1347 -> k_fg_1343 (0, x_1347))
   else
     if y_1036 <= 0 then
       f_1030 x_1035 (fun x_1361 -> k_fg_1343 (x_1361, 0))
     else
       fg_1034 (x_1035 - 1) (y_1036 - 1) (fun x_1376 -> k_fg_1343 (1 + fst x_1376, 1 + snd x_1376))
 in
 let rec fg'_1039 (x_1040:int) (y_1041:int) (k_fg'_1417:((int * int) -> X)) =
   if x_1040 <= 0 then
     g_1032 y_1041 (fun x_1421 -> k_fg'_1417 (0, x_1421))
   else
     if y_1041 <= 0 then
       f_1030 x_1040 (fun x_1435 -> k_fg'_1417 (x_1435, 0))
     else
       fg'_1039 (x_1040 - 1) (y_1041 - 1) (fun x_1450 -> k_fg'_1417 (1 + fst x_1450, 1 + snd x_1450))
 in
 let rec id_1044 (x_1045:int) (k_id_1488:(int -> X)) =
   if x_1045 <= 0 then
     k_id_1488 x_1045
   else
     id_1044 (x_1045 - 1) (fun x_1492 -> k_id_1488 (1 + x_1492))
 in
 let main_1046 (n_1047:int) (k_main_1520:(unit -> X)) =
   if n_1047 < 0 then
     k_main_1520 ()
   else
     id_1044 n_1047
       (fun x_1523 ->
          fg_1034 0 x_1523
            (fun x_1536 ->
               fg_1034 x_1523 x_1523
                 (fun x_1550 -> (if fst x_1550 = snd x_1550 then
                                   k_main_1520 ()
                                 else
                                   {|fail|} () k_main_1520))))
 in
   rand_int_cps () (fun x_1582 -> main_1046 x_1582 (fun x_1579 -> end))

remove_pair:
 let rec f_1030 (x_1031:int) (k_f_1276:(int -> X)) =
   if x_1031 <= 0 then
     k_f_1276 0
   else
     f_1030 (x_1031 - 1) (fun x_1280 -> k_f_1276 (1 + x_1280))
 in
 let rec g_1032 (x_1033:int) (k_g_1308:(int -> X)) =
   if x_1033 <= 0 then
     k_g_1308 0
   else
     g_1032 (x_1033 - 1) (fun x_1312 -> k_g_1308 (1 + x_1312))
 in
 let rec fg_1034 (x_1035:int) (y_1036:int) (k_fg_1343:(int -> (int -> X))) =
   if x_1035 <= 0 then
     g_1032 y_1036 (fun x_1347 -> k_fg_1343 0 x_1347)
   else
     if y_1036 <= 0 then
       f_1030 x_1035 (fun x_1361 -> k_fg_1343 x_1361 0)
     else
       fg_1034 (x_1035 - 1) (y_1036 - 1) (fun x1_1376 -> fun x2_1376 -> k_fg_1343 (1 + x1_1376) (1 + x2_1376))
 in
 let rec fg'_1039 (x_1040:int) (y_1041:int) (k_fg'_1417:(int -> (int -> X))) =
   if x_1040 <= 0 then
     g_1032 y_1041 (fun x_1421 -> k_fg'_1417 0 x_1421)
   else
     if y_1041 <= 0 then
       f_1030 x_1040 (fun x_1435 -> k_fg'_1417 x_1435 0)
     else
       fg'_1039 (x_1040 - 1) (y_1041 - 1) (fun x1_1450 -> fun x2_1450 -> k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
 in
 let rec id_1044 (x_1045:int) (k_id_1488:(int -> X)) =
   if x_1045 <= 0 then
     k_id_1488 x_1045
   else
     id_1044 (x_1045 - 1) (fun x_1492 -> k_id_1488 (1 + x_1492))
 in
 let main_1046 (n_1047:int) (k_main_1520:(unit -> X)) =
   if n_1047 < 0 then
     k_main_1520 ()
   else
     id_1044 n_1047
       (fun x_1523 ->
          fg_1034 0 x_1523
            (fun x1_1536 ->
               fun x2_1536 ->
                 fg_1034 x_1523 x_1523
                   (fun x1_1550 ->
                      fun x2_1550 -> (if x1_1550 = x2_1550 then
                                        k_main_1520 ()
                                      else
                                        {|fail|} () k_main_1520))))
 in
   rand_int_cps () (fun x_1582 -> main_1046 x_1582 (fun x_1579 -> end))

eliminate same arguments:
 let rec f_1030 (x_1031:int) (k_f_1276:(int -> X)) =
   if x_1031 <= 0 then
     k_f_1276 0
   else
     f_1030 (x_1031 - 1) (fun x_1280 -> k_f_1276 (1 + x_1280))
 in
 let rec g_1032 (x_1033:int) (k_g_1308:(int -> X)) =
   if x_1033 <= 0 then
     k_g_1308 0
   else
     g_1032 (x_1033 - 1) (fun x_1312 -> k_g_1308 (1 + x_1312))
 in
 let rec fg_1034 (x_1035:int) (y_1036:int) (k_fg_1343:(int -> (int -> X))) =
   if x_1035 <= 0 then
     g_1032 y_1036 (fun x_1347 -> k_fg_1343 0 x_1347)
   else
     if y_1036 <= 0 then
       f_1030 x_1035 (fun x_1361 -> k_fg_1343 x_1361 0)
     else
       fg_1034 (x_1035 - 1) (y_1036 - 1) (fun x1_1376 -> fun x2_1376 -> k_fg_1343 (1 + x1_1376) (1 + x2_1376))
 in
 let rec fg'_1039 (x_1040:int) (k_fg'_1417:(int -> (int -> X))) =
   if x_1040 <= 0 then
     g_1032 x_1040 (fun x_1421 -> k_fg'_1417 0 x_1421)
   else
     if x_1040 <= 0 then
       f_1030 x_1040 (fun x_1435 -> k_fg'_1417 x_1435 0)
     else
       fg'_1039 (x_1040 - 1) (fun x1_1450 -> fun x2_1450 -> k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
 in
 let rec id_1044 (x_1045:int) (k_id_1488:(int -> X)) =
   if x_1045 <= 0 then
     k_id_1488 x_1045
   else
     id_1044 (x_1045 - 1) (fun x_1492 -> k_id_1488 (1 + x_1492))
 in
 let main_1046 (n_1047:int) (k_main_1520:(unit -> X)) =
   if n_1047 < 0 then
     k_main_1520 ()
   else
     id_1044 n_1047
       (fun x_1523 ->
          fg_1034 0 x_1523
            (fun x1_1536 ->
               fun x2_1536 ->
                 fg_1034 x_1523 x_1523
                   (fun x1_1550 ->
                      fun x2_1550 -> (if x1_1550 = x2_1550 then
                                        k_main_1520 ()
                                      else
                                        {|fail|} () k_main_1520))))
 in
   rand_int_cps () (fun x_1582 -> main_1046 x_1582 (fun x_1579 -> end))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (int -> int -> (int -> int -> X) -> X)
  g_1032 : (int -> (int -> X) -> X)
  id_1044 : (int -> (int -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_main_1708 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> int[x_1 <= 0] -> (int -> int -> X) -> X)
  g_1032 : (int -> (int -> X) -> X)
  id_1044 : (int -> (int -> X) -> X)

(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [1/2] ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> int[x_1 <= 0] -> (int -> int -> X) -> X)
  g_1032 : (int -> (int -> X) -> X)
  id_1044 : (int -> (x_3:int[x_3 <= 0] -> X) -> X)

(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [1/2] ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(2-3) Checking counterexample ... DONE!

(2-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 3)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> x_2:int[x_2 >= 1; x_1 <= 0] -> (int -> int -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 1] -> (int -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(3-1) Abstracting ... DONE!

(3-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(3-3) Checking counterexample ... DONE!

(3-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 4)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> x_2:int[x_1 >= 1; x_2 >= 1; x_1 <= 0] -> (int -> int -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 1] -> (int -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(4-1) Abstracting ... DONE!

(4-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 1; 0

(4-3) Checking counterexample ... DONE!

(4-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 1

Program with abstraction types (CEGAR-cycle 5)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> x_2:int[x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] -> (int -> int -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 1] -> (int -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(5-1) Abstracting ... DONE!

(5-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [1/2] ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0

(5-3) Checking counterexample ... DONE!

(5-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 6)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int -> x_2:int[x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] -> (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(6-1) Abstracting ... DONE!

(6-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [1/2] ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 1; 0

(6-3) Checking counterexample ... DONE!

(6-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 7)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] -> (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(7-1) Abstracting ... DONE!

(7-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0; 0; 1; 0

(7-3) Checking counterexample ... DONE!

(7-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 0; 1

Program with abstraction types (CEGAR-cycle 8)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(8-1) Abstracting ... DONE!

(8-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 1; 0

(8-3) Checking counterexample ... DONE!

(8-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0

Program with abstraction types (CEGAR-cycle 9)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(9-1) Abstracting ... DONE!

(9-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 1; 1; 0; 0; 0; 0; 0; 1; 0

(9-3) Checking counterexample ... DONE!

(9-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0

Program with abstraction types (CEGAR-cycle 10)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_1 >= 2; x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(10-1) Abstracting ... DONE!

(10-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0

(10-3) Checking counterexample ... DONE!

(10-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1

Program with abstraction types (CEGAR-cycle 11)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_1 <= 2; x_1 >= 2; x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(11-1) Abstracting ... DONE!

(11-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 0; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0

(11-3) Checking counterexample ... DONE!

(11-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 0; 1

Program with abstraction types (CEGAR-cycle 12)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 <= 2; x_1 <= 2; x_1 >= 2; x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] -> (x_3:int[x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(12-1) Abstracting ... DONE!

(12-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0

(12-3) Checking counterexample ... DONE!

(12-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 0

Program with abstraction types (CEGAR-cycle 13)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 >= 3; x_2 <= 2; x_1 <= 2; x_1 >= 2; x_2 >= 2;
                     x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1;
                     x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 3; x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] ->
             (x_3:int[x_3 >= 3; x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(13-1) Abstracting ... DONE!

(13-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 1; 0; 0; 0; 0; 0; 0; 1; 0

(13-3) Checking counterexample ... DONE!

(13-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 0; 1

Program with abstraction types (CEGAR-cycle 14)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 <= 3; x_2 >= 3; x_2 <= 2; x_1 <= 2; x_1 >= 2;
                     x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1; x_1 >= 1;
                     x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 3; x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] ->
             (x_3:int[x_3 <= 3; x_3 >= 3; x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(14-1) Abstracting ... DONE!

(14-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 0; 1; 0; 0; 0; 0; 0; 1; 0

(14-3) Checking counterexample ... DONE!

(14-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 0

Program with abstraction types (CEGAR-cycle 15)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_1 >= 3; x_2 <= 3; x_2 >= 3; x_2 <= 2; x_1 <= 2;
                     x_1 >= 2; x_2 >= 2; x_2 <= 1; x_2 <= 0; x_1 <= 1;
                     x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 3; x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] ->
             (x_3:int[x_3 <= 3; x_3 >= 3; x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(15-1) Abstracting ... DONE!

(15-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0

(15-3) Checking counterexample ... DONE!

(15-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1

Program with abstraction types (CEGAR-cycle 16)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_1 <= 3; x_1 >= 3; x_2 <= 3; x_2 >= 3; x_2 <= 2;
                     x_1 <= 2; x_1 >= 2; x_2 >= 2; x_2 <= 1; x_2 <= 0;
                     x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 3; x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] ->
             (x_3:int[x_3 <= 3; x_3 >= 3; x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(16-1) Abstracting ... DONE!

(16-2) Checking HORS ... DONE!

Error trace::
  main_1698 ... -->
  f_1711 ... -->
  main_1046 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [2/2] ... -->
  id_1044 [1/2] ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_id_1707 ... -->
  f_main_1708 ... -->
  fg_1034 [1/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [2/2] ... -->
  g_1032 [1/2] ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_g_1700 ... -->
  f_fg_1701 ... -->
  f_main_1709 ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [2/2] ... -->
  fg_1034 [2/2] ... -->
  br_fg_1713 [1/2] ... -->
  f_1030 [1/2] ... -->
  f_fg_1702 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_fg_1703 ... -->
  f_main_1710 [2/2] ... -->
  fail_1717 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 1; 0

(16-3) Checking counterexample ... DONE!

(16-4) Discovering predicates ...
DONE!

Prefix of spurious counterexample::
0; 0; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0

Program with abstraction types (CEGAR-cycle 17)::
Main: main_1698
  main_1698 -> (rand_int f_1711)
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when b_1716 -> (f_1030 x_1040 (f_fg'_1705 x_1040 k_fg'_1417))
  br_fg'_1715 b_1716 x_1040 k_fg'_1417 when (not b_1716) -> (fg'_1039 (x_1040 - 1) (f_fg'_1706 x_1040 k_fg'_1417))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when b_1714 -> (f_1030 x_1035 (f_fg_1702 x_1035 y_1036 k_fg_1343))
  br_fg_1713 b_1714 x_1035 y_1036 k_fg_1343 when (not b_1714) ->
      (fg_1034 (x_1035 - 1) (y_1036 - 1) (f_fg_1703 x_1035 y_1036 k_fg_1343))
  f_1030 x_1031 k_f_1276 when (x_1031 <= 0) -> (k_f_1276 0)
  f_1030 x_1031 k_f_1276 when (not (x_1031 <= 0)) -> (f_1030 (x_1031 - 1) (f_f_1699 x_1031 k_f_1276))
  f_1711 x_1582 -> (main_1046 x_1582 (f_1712 x_1582))
  f_1712 x_1582 x_1579 -> end
  f_f_1699 x_1031 k_f_1276 x_1280 -> (k_f_1276 (1 + x_1280))
  f_fg'_1704 x_1040 k_fg'_1417 x_1421 -> (k_fg'_1417 0 x_1421)
  f_fg'_1705 x_1040 k_fg'_1417 x_1435 -> (k_fg'_1417 x_1435 0)
  f_fg'_1706 x_1040 k_fg'_1417 x1_1450 x2_1450 -> (k_fg'_1417 (1 + x1_1450) (1 + x2_1450))
  f_fg_1701 x_1035 y_1036 k_fg_1343 x_1347 -> (k_fg_1343 0 x_1347)
  f_fg_1702 x_1035 y_1036 k_fg_1343 x_1361 -> (k_fg_1343 x_1361 0)
  f_fg_1703 x_1035 y_1036 k_fg_1343 x1_1376 x2_1376 -> (k_fg_1343 (1 + x1_1376) (1 + x2_1376))
  f_g_1700 x_1033 k_g_1308 x_1312 -> (k_g_1308 (1 + x_1312))
  f_id_1707 x_1045 k_id_1488 x_1492 -> (k_id_1488 (1 + x_1492))
  f_main_1708 n_1047 k_main_1520 x_1523 -> (fg_1034 0 x_1523 (f_main_1709 n_1047 x_1523 k_main_1520))
  f_main_1709 n_1047 x_1523 k_main_1520 x1_1536 x2_1536 ->
      (fg_1034 x_1523 x_1523 (f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520))
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (x1_1550 = x2_1550) -> (k_main_1520 ())
  f_main_1710 n_1047 x1_1536 x2_1536 x_1523 k_main_1520 x1_1550 x2_1550 when (
      not (x1_1550 = x2_1550)) -> (fail_1717 true k_main_1520)
  fail_1717 b k -> {fail} => (k ())
  fg'_1039 x_1040 k_fg'_1417 when (x_1040 <= 0) -> (g_1032 x_1040 (f_fg'_1704 x_1040 k_fg'_1417))
  fg'_1039 x_1040 k_fg'_1417 when (not (x_1040 <= 0)) -> (br_fg'_1715 (x_1040 <= 0) x_1040 k_fg'_1417)
  fg_1034 x_1035 y_1036 k_fg_1343 when (x_1035 <= 0) -> (g_1032 y_1036 (f_fg_1701 x_1035 y_1036 k_fg_1343))
  fg_1034 x_1035 y_1036 k_fg_1343 when (not (x_1035 <= 0)) -> (br_fg_1713 (y_1036 <= 0) x_1035 y_1036 k_fg_1343)
  g_1032 x_1033 k_g_1308 when (x_1033 <= 0) -> (k_g_1308 0)
  g_1032 x_1033 k_g_1308 when (not (x_1033 <= 0)) -> (g_1032 (x_1033 - 1) (f_g_1700 x_1033 k_g_1308))
  id_1044 x_1045 k_id_1488 when (x_1045 <= 0) -> (k_id_1488 x_1045)
  id_1044 x_1045 k_id_1488 when (not (x_1045 <= 0)) -> (id_1044 (x_1045 - 1) (f_id_1707 x_1045 k_id_1488))
  main_1046 n_1047 k_main_1520 when (n_1047 < 0) -> (k_main_1520 ())
  main_1046 n_1047 k_main_1520 when (not (n_1047 < 0)) -> (id_1044 n_1047 (f_main_1708 n_1047 k_main_1520))
Types:
  main_1698 : X
  f_1030 : (int -> (int -> X) -> X)
  fail_1717 : (x_1:bool[x_1] -> (unit -> X) -> X)
  fg_1034 : (x_1:int ->
             x_2:int[x_2 >= 4; x_1 <= 3; x_1 >= 3; x_2 <= 3; x_2 >= 3;
                     x_2 <= 2; x_1 <= 2; x_1 >= 2; x_2 >= 2; x_2 <= 1;
                     x_2 <= 0; x_1 <= 1; x_1 >= 1; x_2 >= 1; x_1 <= 0] ->
             (x_4:int -> x_5:int[x_4 = x_5] -> X) -> X)
  g_1032 : (x_1:int[x_1 >= 4; x_1 >= 3; x_1 >= 2; x_1 <= 0; x_1 >= 1] -> (x_3:int[x_3 = 0] -> X) -> X)
  id_1044 : (x_1:int[x_1 >= 0] ->
             (x_3:int[x_3 >= 4; x_3 <= 3; x_3 >= 3; x_3 <= 2; x_3 >= 2; x_3 <= 1; x_3 >= 0; x_3 >= 1; x_3 <= 0] -> X) -> X)

(17-1) Abstracting ... DONE!

(17-2) Checking HORS ... Restart TRecS (param: 1000 -> 2000)
