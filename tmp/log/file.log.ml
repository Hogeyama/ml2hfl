MoCHi: Model Checker for Higher-Order Programs
  Build: c81d999 (2014-06-27 16:26:06 +0900)
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test/file.ml -debug 2

File "file.ml", line 21, characters 2-24:
Warning 10: this expression should have type unit.
parsed:
 let rec loop_1008 x_1009 = loop_1008 () in
 let init_1010 = 0 in
 let opened_1011 = 1 in
 let closed_1012 = 2 in
 let ignore_1013 = 3 in
 let readit_1014 st_1015 =
   if st_1015 = opened_1011 then
     opened_1011
   else
     if st_1015 = ignore_1013 then
       st_1015
     else
       let u_1020 = {fail} () in
       _|_
 in
 let read__1016 x_1017 st_1018 = if x_1017 then
                                   readit_1014 st_1018
                                 else
                                   st_1018 in
 let closeit_1019 st_1020 =
   if st_1020 = opened_1011 then
     closed_1012
   else
     if st_1020 = ignore_1013 then
       st_1020
     else
       let u_1038 = loop_1008 () in
       0
 in
 let close__1021 x_1022 st_1023 = if x_1022 then
                                    closeit_1019 st_1023
                                  else
                                    st_1023 in
 let rec f_1024 x_1025 y_1026 st_1027 =
   let u_1074 = close__1021 y_1026 (close__1021 x_1025 st_1027) in
   f_1024 x_1025 y_1026 (read__1016 y_1026 (read__1016 x_1025 st_1027))
 in
 let next_1028 st_1029 = if st_1029 = init_1010 then
                           opened_1011
                         else
                           ignore_1013 in
 let g_1030 b3_1031 x_1032 st_1033 =
   if b3_1031 > 0 then
     f_1024 x_1032 true (next_1028 st_1033)
   else
     f_1024 x_1032 false st_1033
 in
 let main_1034 b2_1035 b3_1036 =
   let u_1120 = if b2_1035 > 0 then
                  g_1030 b3_1036 true opened_1011
                else
                  g_1030 b3_1036 false init_1010 in
   ()
 in
 ()

set_target:
 let rec loop_1008 (x_1009:unit) = loop_1008 () in
 let init_1010 = 0 in
 let opened_1011 = 1 in
 let closed_1012 = 2 in
 let ignore_1013 = 3 in
 let readit_1014 (st_1015:int) =
   if st_1015 = opened_1011 then
     opened_1011
   else
     if st_1015 = ignore_1013 then
       st_1015
     else
       let u_1020 = {fail} () in
       _|_
 in
 let read__1016 (x_1017:bool) (st_1018:int) = if x_1017 then
                                                readit_1014 st_1018
                                              else
                                                st_1018 in
 let closeit_1019 (st_1020:int) =
   if st_1020 = opened_1011 then
     closed_1012
   else
     if st_1020 = ignore_1013 then
       st_1020
     else
       let u_1038 = loop_1008 () in
       0
 in
 let close__1021 (x_1022:bool) (st_1023:int) = if x_1022 then
                                                 closeit_1019 st_1023
                                               else
                                                 st_1023 in
 let rec f_1024 (x_1025:bool) (y_1026:bool) (st_1027:int) =
   let u_1074 = close__1021 y_1026 (close__1021 x_1025 st_1027) in
   f_1024 x_1025 y_1026 (read__1016 y_1026 (read__1016 x_1025 st_1027))
 in
 let next_1028 (st_1029:int) = if st_1029 = init_1010 then
                                 opened_1011
                               else
                                 ignore_1013 in
 let g_1030 (b3_1031:int) (x_1032:bool) (st_1033:int) =
   if b3_1031 > 0 then
     f_1024 x_1032 true (next_1028 st_1033)
   else
     f_1024 x_1032 false st_1033
 in
 let main_1034 (b2_1035:int) (b3_1036:int) =
   let u_1120 = if b2_1035 > 0 then
                  g_1030 b3_1036 true opened_1011
                else
                  g_1030 b3_1036 false init_1010 in
   ()
 in
 let main_1125 = let arg1_1121 = rand_int () in
                 let arg2_1123 = rand_int () in
                 main_1034 arg1_1121 arg2_1123 in
 ()

encode_list:
 let rec loop_1008 (x_1009:unit) = loop_1008 () in
 let readit_1014 (st_1015:int) =
   if st_1015 = 1 then
     1
   else
     if st_1015 = 3 then
       st_1015
     else
       let u_1020 = {fail} () in
       _|_
 in
 let read__1016 (x_1017:bool) (st_1018:int) = if x_1017 then
                                                readit_1014 st_1018
                                              else
                                                st_1018 in
 let closeit_1019 (st_1020:int) =
   if st_1020 = 1 then
     2
   else
     if st_1020 = 3 then
       st_1020
     else
       let u_1038 = loop_1008 () in
       0
 in
 let close__1021 (x_1022:bool) (st_1023:int) = if x_1022 then
                                                 closeit_1019 st_1023
                                               else
                                                 st_1023 in
 let rec f_1024 (x_1025:bool) (y_1026:bool) (st_1027:int) =
   let u_1074 = close__1021 y_1026 (close__1021 x_1025 st_1027) in
   f_1024 x_1025 y_1026 (read__1016 y_1026 (read__1016 x_1025 st_1027))
 in
 let next_1028 (st_1029:int) = if st_1029 = 0 then
                                 1
                               else
                                 3 in
 let g_1030 (b3_1031:int) (x_1032:bool) (st_1033:int) =
   if b3_1031 > 0 then
     f_1024 x_1032 true (next_1028 st_1033)
   else
     f_1024 x_1032 false st_1033
 in
 let main_1034 (b2_1035:int) (b3_1036:int) =
   let u_1120 = if b2_1035 > 0 then
                  g_1030 b3_1036 true 1
                else
                  g_1030 b3_1036 false 0 in
   ()
 in
 let main_1125 = let arg1_1121 = rand_int () in
                 let arg2_1123 = rand_int () in
                 main_1034 arg1_1121 arg2_1123 in
 ()

CPS:
 let rec loop_1008 (x_1009:unit) (k_loop_1152:(unit -> X)) = loop_1008 () k_loop_1152 in
 let readit_1014 (st_1015:int) (k_readit_1169:(int -> X)) =
   if st_1015 = 1 then
     k_readit_1169 1
   else
     if st_1015 = 3 then
       k_readit_1169 st_1015
     else
       let u_1020 (k_readit_u_1173:(unit -> X)) = {|fail|} () k_readit_u_1173 in
       u_1020 (fun (u_1181:unit) -> _|_)
 in
 let read__1016 (x_1017:bool) (st_1018:int) (k_read__1196:(int -> X)) =
   if x_1017 then
     readit_1014 st_1018 k_read__1196
   else
     k_read__1196 st_1018
 in
 let closeit_1019 (st_1020:int) (k_closeit_1218:(int -> X)) =
   if st_1020 = 1 then
     k_closeit_1218 2
   else
     if st_1020 = 3 then
       k_closeit_1218 st_1020
     else
       let u_1038 (k_closeit_u_1225:(unit -> X)) = loop_1008 () k_closeit_u_1225 in
       u_1038 (fun (u_1231:unit) -> k_closeit_1218 0)
 in
 let close__1021 (x_1022:bool) (st_1023:int) (k_close__1246:(int -> X)) =
   if x_1022 then
     closeit_1019 st_1023 k_close__1246
   else
     k_close__1246 st_1023
 in
 let rec f_1024 (x_1025:bool) (y_1026:bool) (st_1027:int) (k_f_1268:(unit -> X)) =
   let u_1074 (k_f_u_1287:(int -> X)) =
     close__1021 x_1025 st_1027 (fun (x_1515:int) -> close__1021 y_1026 x_1515 k_f_u_1287)
   in
   u_1074
     (fun (u_1327:int) ->
        read__1016 x_1025 st_1027
          (fun (x_1517:int) -> read__1016 y_1026 x_1517 (fun (x_1519:int) -> f_1024 x_1025 y_1026 x_1519 k_f_1268)))
 in
 let next_1028 (st_1029:int) (k_next_1336:(int -> X)) = if st_1029 = 0 then
                                                          k_next_1336 1
                                                        else
                                                          k_next_1336 3 in
 let g_1030 (b3_1031:int) (x_1032:bool) (st_1033:int) (k_g_1347:(unit -> X)) =
   if b3_1031 > 0 then
     next_1028 st_1033 (fun (x_1521:int) -> f_1024 x_1032 true x_1521 k_g_1347)
   else
     f_1024 x_1032 false st_1033 k_g_1347
 in
 let main_1034 (b2_1035:int) (b3_1036:int) (k_main_1394:(unit -> X)) =
   let u_1120 (k_main_u_1422:(unit -> X)) =
     if b2_1035 > 0 then
       g_1030 b3_1036 true 1 k_main_u_1422
     else
       g_1030 b3_1036 false 0 k_main_u_1422
   in
   u_1120 (fun (u_1425:unit) -> k_main_1394 ())
 in
 let main_1125 (k_main_1433:(unit -> X)) =
   let arg1_1121 (k_main_arg1_1438:(int -> X)) = rand_int_cps () k_main_arg1_1438 in
   arg1_1121
     (fun (arg1_1468:int) ->
        (let arg2_1123 (k_main_arg2_1450:(int -> X)) = rand_int_cps () k_main_arg2_1450 in
         arg2_1123 (fun (arg2_1467:int) -> main_1034 arg1_1468 arg2_1467 k_main_1433)))
 in
 main_1125 (fun (main_1469:unit) -> {end})

replace_bottom_def:
 let loop_1008 (x_1009:unit) (k_loop_1152:(unit -> X)) = _|_ in
 let readit_1014 (st_1015:int) (k_readit_1169:(int -> X)) =
   if st_1015 = 1 then
     k_readit_1169 1
   else
     if st_1015 = 3 then
       k_readit_1169 st_1015
     else
       let u_1020 (k_readit_u_1173:(unit -> X)) = {|fail|} () k_readit_u_1173 in
       u_1020 (fun (u_1181:unit) -> _|_)
 in
 let read__1016 (x_1017:bool) (st_1018:int) (k_read__1196:(int -> X)) =
   if x_1017 then
     readit_1014 st_1018 k_read__1196
   else
     k_read__1196 st_1018
 in
 let closeit_1019 (st_1020:int) (k_closeit_1218:(int -> X)) =
   if st_1020 = 1 then
     k_closeit_1218 2
   else
     if st_1020 = 3 then
       k_closeit_1218 st_1020
     else
       let u_1038 (k_closeit_u_1225:(unit -> X)) = loop_1008 () k_closeit_u_1225 in
       u_1038 (fun (u_1231:unit) -> k_closeit_1218 0)
 in
 let close__1021 (x_1022:bool) (st_1023:int) (k_close__1246:(int -> X)) =
   if x_1022 then
     closeit_1019 st_1023 k_close__1246
   else
     k_close__1246 st_1023
 in
 let rec f_1024 (x_1025:bool) (y_1026:bool) (st_1027:int) (k_f_1268:(unit -> X)) =
   let u_1074 (k_f_u_1287:(int -> X)) =
     close__1021 x_1025 st_1027 (fun (x_1515:int) -> close__1021 y_1026 x_1515 k_f_u_1287)
   in
   u_1074
     (fun (u_1327:int) ->
        read__1016 x_1025 st_1027
          (fun (x_1517:int) -> read__1016 y_1026 x_1517 (fun (x_1519:int) -> f_1024 x_1025 y_1026 x_1519 k_f_1268)))
 in
 let next_1028 (st_1029:int) (k_next_1336:(int -> X)) = if st_1029 = 0 then
                                                          k_next_1336 1
                                                        else
                                                          k_next_1336 3 in
 let g_1030 (b3_1031:int) (x_1032:bool) (st_1033:int) (k_g_1347:(unit -> X)) =
   if b3_1031 > 0 then
     next_1028 st_1033 (fun (x_1521:int) -> f_1024 x_1032 true x_1521 k_g_1347)
   else
     f_1024 x_1032 false st_1033 k_g_1347
 in
 let main_1034 (b2_1035:int) (b3_1036:int) (k_main_1394:(unit -> X)) =
   let u_1120 (k_main_u_1422:(unit -> X)) =
     if b2_1035 > 0 then
       g_1030 b3_1036 true 1 k_main_u_1422
     else
       g_1030 b3_1036 false 0 k_main_u_1422
   in
   u_1120 (fun (u_1425:unit) -> k_main_1394 ())
 in
 let main_1125 (k_main_1433:(unit -> X)) =
   let arg1_1121 (k_main_arg1_1438:(int -> X)) = rand_int_cps () k_main_arg1_1438 in
   arg1_1121
     (fun (arg1_1468:int) ->
        (let arg2_1123 (k_main_arg2_1450:(int -> X)) = rand_int_cps () k_main_arg2_1450 in
         arg2_1123 (fun (arg2_1467:int) -> main_1034 arg1_1468 arg2_1467 k_main_1433)))
 in
 main_1125 (fun (main_1469:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1528
  main_1528 -> (main_1125 f_1539)
  arg1_1121 k_main_arg1_1438 -> (rand_int k_main_arg1_1438)
  arg2_1123 arg1_1468 k_main_arg2_1450 -> (rand_int k_main_arg2_1450)
  br_closeit_1542 b_1543 st_1020 k_closeit_1218 when b_1543 -> (k_closeit_1218 st_1020)
  br_closeit_1542 b_1543 st_1020 k_closeit_1218 when (not b_1543) ->
      (u_1038 st_1020 (f_closeit_1530 st_1020 k_closeit_1218))
  br_readit_1540 b_1541 st_1015 k_readit_1169 when b_1541 -> (k_readit_1169 st_1015)
  br_readit_1540 b_1541 st_1015 k_readit_1169 when (not b_1541) -> (u_1020 st_1015 (f_readit_1529 st_1015))
  close__1021 x_1022 st_1023 k_close__1246 when x_1022 -> (closeit_1019 st_1023 k_close__1246)
  close__1021 x_1022 st_1023 k_close__1246 when (not x_1022) -> (k_close__1246 st_1023)
  closeit_1019 st_1020 k_closeit_1218 when (st_1020 = 1) -> (k_closeit_1218 2)
  closeit_1019 st_1020 k_closeit_1218 when (not (st_1020 = 1)) ->
      (br_closeit_1542 (st_1020 = 3) st_1020 k_closeit_1218)
  f_1024 x_1025 y_1026 st_1027 k_f_1268 -> (u_1074 st_1027 x_1025 y_1026 (f_f_1532 st_1027 x_1025 y_1026 k_f_1268))
  f_1539 main_1469 -> end
  f_closeit_1530 st_1020 k_closeit_1218 u_1231 -> (k_closeit_1218 0)
  f_f_1532 st_1027 x_1025 y_1026 k_f_1268 u_1327 ->
      (read__1016 x_1025 st_1027 (f_f_1533 st_1027 u_1327 x_1025 y_1026 k_f_1268))
  f_f_1533 st_1027 u_1327 x_1025 y_1026 k_f_1268 x_1517 ->
      (read__1016 y_1026 x_1517 (f_f_1534 st_1027 u_1327 x_1025 x_1517 y_1026 k_f_1268))
  f_f_1534 st_1027 u_1327 x_1025 x_1517 y_1026 k_f_1268 x_1519 -> (f_1024 x_1025 y_1026 x_1519 k_f_1268)
  f_g_1535 b3_1031 st_1033 x_1032 k_g_1347 x_1521 -> (f_1024 x_1032 true x_1521 k_g_1347)
  f_main_1536 b2_1035 b3_1036 k_main_1394 u_1425 -> (k_main_1394 ())
  f_main_1537 k_main_1433 arg1_1468 -> (arg2_1123 arg1_1468 (f_main_1538 arg1_1468 k_main_1433))
  f_main_1538 arg1_1468 k_main_1433 arg2_1467 -> (main_1034 arg1_1468 arg2_1467 k_main_1433)
  f_readit_1529 st_1015 u_1181 -> _|_
  f_u_1531 st_1027 x_1025 y_1026 k_f_u_1287 x_1515 -> (close__1021 y_1026 x_1515 k_f_u_1287)
  g_1030 b3_1031 x_1032 st_1033 k_g_1347 when (b3_1031 > 0) ->
      (next_1028 st_1033 (f_g_1535 b3_1031 st_1033 x_1032 k_g_1347))
  g_1030 b3_1031 x_1032 st_1033 k_g_1347 when (not (b3_1031 > 0)) -> (f_1024 x_1032 false st_1033 k_g_1347)
  loop_1008 x_1009 k_loop_1152 -> _|_
  main_1034 b2_1035 b3_1036 k_main_1394 -> (u_1120 b2_1035 b3_1036 (f_main_1536 b2_1035 b3_1036 k_main_1394))
  main_1125 k_main_1433 -> (arg1_1121 (f_main_1537 k_main_1433))
  next_1028 st_1029 k_next_1336 when (st_1029 = 0) -> (k_next_1336 1)
  next_1028 st_1029 k_next_1336 when (not (st_1029 = 0)) -> (k_next_1336 3)
  read__1016 x_1017 st_1018 k_read__1196 when x_1017 -> (readit_1014 st_1018 k_read__1196)
  read__1016 x_1017 st_1018 k_read__1196 when (not x_1017) -> (k_read__1196 st_1018)
  readit_1014 st_1015 k_readit_1169 when (st_1015 = 1) -> (k_readit_1169 1)
  readit_1014 st_1015 k_readit_1169 when (not (st_1015 = 1)) -> (br_readit_1540 (st_1015 = 3) st_1015 k_readit_1169)
  u_1020 st_1015 k_readit_u_1173 -> {fail} => (k_readit_u_1173 ())
  u_1038 st_1020 k_closeit_u_1225 -> (loop_1008 () k_closeit_u_1225)
  u_1074 st_1027 x_1025 y_1026 k_f_u_1287 -> (close__1021 x_1025 st_1027 (f_u_1531 st_1027 x_1025 y_1026 k_f_u_1287))
  u_1120 b2_1035 b3_1036 k_main_u_1422 when (b2_1035 > 0) -> (g_1030 b3_1036 true 1 k_main_u_1422)
  u_1120 b2_1035 b3_1036 k_main_u_1422 when (not (b2_1035 > 0)) -> (g_1030 b3_1036 false 0 k_main_u_1422)
Types:
  main_1528 : X
  close__1021 : (x_1:bool[x_1] -> int -> (int -> X) -> X)
  f_1024 : (x_1:bool[x_1] -> x_2:bool[x_2] -> int -> (unit -> X) -> X)
  read__1016 : (x_1:bool[x_1] -> int -> (int -> X) -> X)
  u_1020 : (int -> (unit -> X) -> X)

(0-1) Abstracting ... Fatal error: exception Division_by_zero
