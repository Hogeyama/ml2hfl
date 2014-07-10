MoCHi: Model Checker for Higher-Order Programs
  Build: _684776d (after 2014-07-02 16:43:31 +0900)
  FPAT version: 221ce5c
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt -rscomp 0 -debug-module CEGAR_abst_CPS,CEGAR_abst_util test/a-max.ml

parsed:
 let make_array_1008 n_1009 i_1010 = n_1009 - i_1010 in
 let rec array_max_1011 x_1028 =
   match x_1028 with
   | (_ as n_1012) ->
       fun i_1013 ->
         fun x_1039 ->
           (match x_1039 with
            | (_ as a_1014) ->
                fun m_1015 ->
                  (if i_1013 >= n_1012 then
                     m_1015
                   else
                     let x_1016 = a_1014 i_1013 in
                     let z_1017 = if x_1016 > m_1015 then
                                    x_1016
                                  else
                                    m_1015 in
                     array_max_1011 n_1012 (i_1013 + 1) a_1014 z_1017))
 in
 let main_1018 n_1019 i_1020 =
   if n_1019 > 0 && (i_1020 >= 0 && i_1020 <= 0) then
     let m_1021 = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 in
     if m_1021 >= n_1019 then
       ()
     else
       {fail} ()
   else
     ()
 in
 ()

set_target:
 let make_array_1008 (n_1009:int) (i_1010:int) = n_1009 - i_1010 in
 let rec array_max_1011 (x_1028:int) =
   match x_1028 with
   | (_ as n_1012) ->
       fun (i_1013:int) ->
         fun (x_1039:(int -> int)) ->
           (match x_1039 with
            | (_ as a_1014) ->
                fun (m_1015:int) ->
                  (if i_1013 >= n_1012 then
                     m_1015
                   else
                     let x_1016 = a_1014 i_1013 in
                     let z_1017 = if x_1016 > m_1015 then
                                    x_1016
                                  else
                                    m_1015 in
                     array_max_1011 n_1012 (i_1013 + 1) a_1014 z_1017))
 in
 let main_1018 (n_1019:int) (i_1020:int) =
   if n_1019 > 0 && (i_1020 >= 0 && i_1020 <= 0) then
     let m_1021 = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 in
     if m_1021 >= n_1019 then
       ()
     else
       {fail} ()
   else
     ()
 in
 let main_1089 = let arg1_1085 = rand_int () in
                 let arg2_1087 = rand_int () in
                 main_1018 arg1_1085 arg2_1087 in
 ()

abst_recdata:
 let make_array_1008 (n_1009:int) (i_1010:int) = n_1009 - i_1010 in
 let rec array_max_1011 (x_1028:int) =
   match x_1028 with
   | (_ as n_1012) ->
       fun (i_1093:int) ->
         fun (x_1094:(int -> int)) ->
           (match x_1094 with
            | (_ as a_1014) ->
                fun (m_1095:int) ->
                  (if i_1093 >= n_1012 then
                     m_1095
                   else
                     let x_1096 = a_1014 i_1093 in
                     let z_1097 = if x_1096 > m_1095 then
                                    x_1096
                                  else
                                    m_1095 in
                     array_max_1011 n_1012 (i_1093 + 1) a_1014 z_1097))
 in
 let main_1018 (n_1019:int) (i_1020:int) =
   if n_1019 > 0 && (i_1020 >= 0 && i_1020 <= 0) then
     let m_1021 = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 in
     if m_1021 >= n_1019 then
       ()
     else
       {fail} ()
   else
     ()
 in
 let main_1089 = let arg1_1085 = rand_int () in
                 let arg2_1087 = rand_int () in
                 main_1018 arg1_1085 arg2_1087 in
 ()

encode_list:
 let make_array_1008 (n_1009:int) (i_1010:int) = n_1009 - i_1010 in
 let rec array_max_1011 (x_1028:int) =
   fun (i_1116:int) ->
     fun (x_1117:(int -> int)) ->
       fun (m_1118:int) ->
         (if i_1116 >= x_1028 then
            m_1118
          else
            let x_1119 = x_1117 i_1116 in
            let z_1120 = if x_1119 > m_1118 then
                           x_1119
                         else
                           m_1118 in
            array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1120)
 in
 let main_1018 (n_1019:int) (i_1020:int) =
   if n_1019 > 0 && (i_1020 >= 0 && i_1020 <= 0) then
     let m_1021 = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 in
     if m_1021 >= n_1019 then
       ()
     else
       {fail} ()
   else
     ()
 in
 let main_1089 = let arg1_1085 = rand_int () in
                 let arg2_1087 = rand_int () in
                 main_1018 arg1_1085 arg2_1087 in
 ()

CPS:
 let make_array_1008 (n_1009:int) (i_1010:int) (k_make_array_1134:(int -> X)) = k_make_array_1134 (n_1009 - i_1010) in
 let rec
   array_max_1011 (x_1028:int) (i_1116:int) (x_1117:(int -> (int -> X) -> X)) (m_1118:int) 
                 (k_array_max_1145:(int -> X)) =
   if i_1116 >= x_1028 then
     k_array_max_1145 m_1118
   else
     let x_1119 (k_array_max_x_1152:(int -> X)) = x_1117 i_1116 k_array_max_x_1152 in
     x_1119
       (fun (x_1184:int) ->
          (let z_1120 (k_array_max_z_1161:(int -> X)) =
             if x_1184 > m_1118 then
               k_array_max_z_1161 x_1184
             else
               k_array_max_z_1161 m_1118
           in
           z_1120 (fun (z_1183:int) -> array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145)))
 in
 let main_1018 (n_1019:int) (i_1020:int) (k_main_1201:(unit -> X)) =
   let k_main_1208 (b_1314:bool) =
     if b_1314 then
       let m_1316 (k_main_m_1317:(int -> X)) = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317 in
       m_1316 (fun (m_1320:int) -> (if m_1320 >= n_1019 then
                                      k_main_1201 ()
                                    else
                                      {|fail|} () k_main_1201))
     else
       k_main_1201 ()
   in
   if n_1019 > 0 then
     if i_1020 >= 0 then
       k_main_1208 (i_1020 <= 0)
     else
       k_main_1208 false
   else
     k_main_1208 false
 in
 let main_1089 (k_main_1255:(unit -> X)) =
   let arg1_1085 (k_main_arg1_1260:(int -> X)) = rand_int_cps () k_main_arg1_1260 in
   arg1_1085
     (fun (arg1_1290:int) ->
        (let arg2_1087 (k_main_arg2_1272:(int -> X)) = rand_int_cps () k_main_arg2_1272 in
         arg2_1087 (fun (arg2_1289:int) -> main_1018 arg1_1290 arg2_1289 k_main_1255)))
 in
 main_1089 (fun (main_1291:unit) -> {end})

remove_pair:
 let make_array_1008 (n_1009:int) (i_1010:int) (k_make_array_1134:(int -> X)) = k_make_array_1134 (n_1009 - i_1010) in
 let rec
   array_max_1011 (x_1028:int) (i_1116:int) (x_1117:(int -> (int -> X) -> X)) (m_1118:int) 
                 (k_array_max_1145:(int -> X)) =
   if i_1116 >= x_1028 then
     k_array_max_1145 m_1118
   else
     let x_1119 (k_array_max_x_1152:(int -> X)) = x_1117 i_1116 k_array_max_x_1152 in
     x_1119
       (fun (x_1184:int) ->
          (let z_1120 (k_array_max_z_1161:(int -> X)) =
             if x_1184 > m_1118 then
               k_array_max_z_1161 x_1184
             else
               k_array_max_z_1161 m_1118
           in
           z_1120 (fun (z_1183:int) -> array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145)))
 in
 let main_1018 (n_1019:int) (i_1020:int) (k_main_1201:(unit -> X)) =
   let k_main_1208 (b_1314:bool) =
     if b_1314 then
       let m_1316 (k_main_m_1317:(int -> X)) = array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317 in
       m_1316 (fun (m_1320:int) -> (if m_1320 >= n_1019 then
                                      k_main_1201 ()
                                    else
                                      {|fail|} () k_main_1201))
     else
       k_main_1201 ()
   in
   if n_1019 > 0 then
     if i_1020 >= 0 then
       k_main_1208 (i_1020 <= 0)
     else
       k_main_1208 false
   else
     k_main_1208 false
 in
 let main_1089 (k_main_1255:(unit -> X)) =
   let arg1_1085 (k_main_arg1_1260:(int -> X)) = rand_int_cps () k_main_arg1_1260 in
   arg1_1085
     (fun (arg1_1290:int) ->
        (let arg2_1087 (k_main_arg2_1272:(int -> X)) = rand_int_cps () k_main_arg2_1272 in
         arg2_1087 (fun (arg2_1289:int) -> main_1018 arg1_1290 arg2_1289 k_main_1255)))
 in
 main_1089 (fun (main_1291:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (int -> int -> (int -> (int -> X) -> X) -> int -> (int -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (int -> int -> (unit -> X) -> x_6:bool[x_6] -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_1447 ->
         (rand_int
           (fun arg2_1450 ->
            (if (arg1_1447 > 0)
              (l0
                (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
                  (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
              (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_1428 ->
           (if (x_1428 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_1463 k_make_array_1464 -> (k_make_array_1464 (n_1019 - i_1463))) -1
          (fun m_1445 -> (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_1447 ->
        (rand_int
          (fun arg2_1450 ->
           (if (arg1_1447 > 0)
             (l0
               (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
                 (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
             (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))))): X
ETA: (fun arg1_1447 ->
      (rand_int
        (fun arg2_1450 ->
         (if (arg1_1447 > 0)
           (l0
             (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
               (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
           (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_1450 ->
        (if (arg1_1447 > 0)
          (l0
            (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
              (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
          (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))): unit
ETA: (fun arg2_1450 ->
      (if (arg1_1447 > 0)
        (l0
          (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
            (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
        (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false)))): (int ->
unit)
ETA: (if (arg1_1447 > 0)
       (l0
         (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
           (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))))
       (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false)): unit
ETA: (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false): unit
ETA: false: x_1:bool[x_1]
ETA: (fun main_1422 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1447: int
ETA: arg2_1450: int
ETA_AUX: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false): unit
ETA: (l0
       (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
         (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false)))): unit
ETA: (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)))
       (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false)): unit
ETA: (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) false): unit
ETA: false: x_1:bool[x_1]
ETA: (fun main_1422 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1447: int
ETA: arg2_1450: int
ETA_AUX: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0))): unit
ETA: (k_main_1208 arg2_1450 arg1_1447 (fun main_1422 -> end) (arg2_1450 <= 0)): unit
ETA: (arg2_1450 <= 0): x_1:bool[x_1]
ETA: (fun main_1422 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1447: int
ETA: arg2_1450: int
ETA_AUX: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_1450:int) ->
            (if (arg1_1447 > 0)
              (l0
                (if (arg2_1450 >= 0)
                  (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                  (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
              (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_1447:int) ->
            (rand_int
              (fun (arg2_1450:int) ->
               (if (arg1_1447 > 0)
                 (l0
                   (if (arg2_1450 >= 0)
                     (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                     (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_1428 ->
          (if (x_1428 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_1428 ->
        (if (x_1428 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_1428 ->
      (if (x_1428 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_1428 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int ->
X)
ETA_AUX: k_array_max_1145: (int ->
X)
ETA_AUX: x__1480: int
ETA_AUX: (k_array_max_1145 x__1480): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__1481: int
ETA_AUX: (x_1117 x__1481): ((int -> X) ->
X)
ETA_AUX: x__1482: (int ->
X)
ETA_AUX: x__1483: int
ETA_AUX: (x__1482 x__1483): X
ETA_AUX: (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
           (fun (x__1480:int) -> (k_array_max_1145 x__1480))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1428 k_array_max_1145): X
ETA: k_array_max_1145: (int ->
X)
ETA_AUX: k_array_max_1145: (int ->
X)
ETA_AUX: x__1484: int
ETA_AUX: (k_array_max_1145 x__1484): X
ETA: x_1428: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__1485: int
ETA_AUX: (x_1117 x__1485): ((int -> X) ->
X)
ETA_AUX: x__1486: (int ->
X)
ETA_AUX: x__1487: int
ETA_AUX: (x__1486 x__1487): X
ETA_AUX: (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
           (fun (x__1484:int) -> (k_array_max_1145 x__1484))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_1428:int) ->
            (if (x_1428 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))))
                  x_1428 (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))))
                  m_1118 (fun (x__1480:int) -> (k_array_max_1145 x__1480))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_1463 k_make_array_1464 -> (k_make_array_1464 (n_1019 - i_1463))) -1
         (fun m_1445 -> (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_1463 k_make_array_1464 -> (k_make_array_1464 (n_1019 - i_1463))) -1
       (fun m_1445 -> (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_1445 -> (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int ->
X)
ETA: (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__1488: unit
ETA_AUX: (k_main_1201 x__1488): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_1463 k_make_array_1464 -> (k_make_array_1464 (n_1019 - i_1463))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_1464 -> (k_make_array_1464 (n_1019 - i_1463))): ((int -> X) ->
X)
ETA: (k_make_array_1464 (n_1019 - i_1463)): X
ETA: (n_1019 - i_1463): int
ETA_AUX: (k_make_array_1464 (n_1019 - i_1463)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
           (fun (m_1445:int) ->
            (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_1447:int) ->
         (rand_int
           (fun (arg2_1450:int) ->
            (if (arg1_1447 > 0)
              (l0
                (if (arg2_1450 >= 0)
                  (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                  (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
              (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_1428:int) ->
           (if (x_1428 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))))
                 x_1428 (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))))
                 m_1118 (fun (x__1480:int) -> (k_array_max_1145 x__1480)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
          (fun (m_1445:int) ->
           (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_1447:int) ->
              (rand_int
                (fun (arg2_1450:int) ->
                 (if (arg1_1447 > 0)
                   (l0
                     (if (arg2_1450 >= 0)
                       (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                       (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_1447:int) ->
  (rand_int
    (fun (arg2_1450:int) ->
     (if (arg1_1447 > 0)
       (l0
         (if (arg2_1450 >= 0) (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
           (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
       (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_1447:int) ->
               (rand_int
                 (fun (arg2_1450:int) ->
                  (if (arg1_1447 > 0)
                    (l0
                      (if (arg2_1450 >= 0)
                        (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                        (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_1447:int) ->
                  (rand_int
                    (fun (arg2_1450:int) ->
                     (if (arg1_1447 > 0)
                       (l0
                         (if (arg2_1450 >= 0)
                           (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                           (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))))): X
abstract_term: (fun (arg1_1447:int) ->
                (rand_int
                  (fun (arg2_1450:int) ->
                   (if (arg1_1447 > 0)
                     (l0
                       (if (arg2_1450 >= 0)
                         (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                         (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_1447, int
abst_arg: arg1_1447, int
abstract_term: (rand_int
                 (fun (arg2_1450:int) ->
                  (if (arg1_1447 > 0)
                    (l0
                      (if (arg2_1450 >= 0)
                        (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                        (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))): X
abstract_term: (fun (arg2_1450:int) ->
                (if (arg1_1447 > 0)
                  (l0
                    (if (arg2_1450 >= 0)
                      (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                      (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_1450, int
abst_arg: arg2_1450, int
abstract_term: (if (arg1_1447 > 0)
                 (l0
                   (if (arg2_1450 >= 0)
                     (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                     (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))): X
abstract_term: (arg1_1447 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_1447 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_1450 >= 0)
                   (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                   (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)))): X
abstract_term: (if (arg2_1450 >= 0)
                 (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)))
                 (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false))): X
abstract_term: (arg2_1450 >= 0): x_1:bool[x_1]
cond: (arg1_1447 > 0); true
pbs: 
p:(arg2_1450 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0))): X
abstract_term: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) (arg2_1450 <= 0)): X
abstract_term: (arg2_1450 <= 0): x_1:bool[x_1]
cond: (arg2_1450 >= 0); (arg1_1447 > 0); true
pbs: 
p:(arg2_1450 <= 0)
tt:false
ff:false

abstract_term: (fun (main_1422:unit) -> end): (unit ->
X)
abst_arg: main_1422, unit
abst_arg: main_1422, unit
abstract_term: end: X
abstract_term: arg1_1447: int
abstract_term: arg2_1450: int
abstract_term: (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false): X
abstract_term: false: x_1:bool[x_1]
cond: (not (arg2_1450 >= 0)); (arg1_1447 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_1422:unit) -> end): (unit ->
X)
abst_arg: main_1422, unit
abst_arg: main_1422, unit
abstract_term: end: X
abstract_term: arg1_1447: int
abstract_term: arg2_1450: int
abstract_term: (l1 (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_1450 arg1_1447 (fun (main_1422:unit) -> end) false): X
abstract_term: false: x_1:bool[x_1]
cond: (not (arg1_1447 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_1422:unit) -> end): (unit ->
X)
abst_arg: main_1422, unit
abst_arg: main_1422, unit
abstract_term: end: X
abstract_term: arg1_1447: int
abstract_term: arg2_1450: int
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int, k_array_max_1145:(int -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int, k_array_max_1145:(int -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_1428:int) ->
                     (if (x_1428 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__1485:int) (x__1486:(int -> X)) ->
                            (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                           (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__1481:int) (x__1482:(int -> X)) ->
                            (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                           (fun (x__1480:int) -> (k_array_max_1145 x__1480)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_1428:int) ->
    (if (x_1428 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
          (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
          (fun (x__1480:int) -> (k_array_max_1145 x__1480))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_1428:int) ->
                      (if (x_1428 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__1485:int) (x__1486:(int -> X)) ->
                             (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                            (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__1481:int) (x__1482:(int -> X)) ->
                             (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                            (fun (x__1480:int) -> (k_array_max_1145 x__1480))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_1428:int) ->
                    (if (x_1428 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__1485:int) (x__1486:(int -> X)) ->
                           (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                          (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__1481:int) (x__1482:(int -> X)) ->
                           (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                          (fun (x__1480:int) -> (k_array_max_1145 x__1480)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_1428:int) ->
                  (if (x_1428 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__1485:int) (x__1486:(int -> X)) ->
                         (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                        (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__1481:int) (x__1482:(int -> X)) ->
                         (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                        (fun (x__1480:int) -> (k_array_max_1145 x__1480))))))): X
abstract_term: (fun (x_1428:int) ->
                (if (x_1428 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__1485:int) (x__1486:(int -> X)) ->
                       (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                      (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__1481:int) (x__1482:(int -> X)) ->
                       (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                      (fun (x__1480:int) -> (k_array_max_1145 x__1480)))))): (int ->
X)
abst_arg: x_1428, int
abst_arg: x_1428, int
abstract_term: (if (x_1428 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__1485:int) (x__1486:(int -> X)) ->
                      (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))) x_1428
                     (fun (x__1484:int) -> (k_array_max_1145 x__1484))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__1481:int) (x__1482:(int -> X)) ->
                      (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))) m_1118
                     (fun (x__1480:int) -> (k_array_max_1145 x__1480))))): X
abstract_term: (x_1428 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_1428 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))))
                   x_1428 (fun (x__1484:int) -> (k_array_max_1145 x__1484)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))))
                 x_1428 (fun (x__1484:int) -> (k_array_max_1145 x__1484))): X
abstract_term: (fun (x__1484:int) -> (k_array_max_1145 x__1484)): (int ->
X)
abst_arg: x__1484, int
abst_arg: x__1484, int
abstract_term: (k_array_max_1145 x__1484): X
abstract_term: x__1484: int
abstract_term: x_1428: int
abstract_term: (fun (x__1485:int) (x__1486:(int -> X)) -> (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487)))): (
int -> (int -> X) -> X)
abst_arg: x__1485, int
abst_arg: x__1486, (int ->
X)
abst_arg: x__1485, int
abst_arg: x__1486, (int ->
X)
abstract_term: (x_1117 x__1485 (fun (x__1487:int) -> (x__1486 x__1487))): X
abstract_term: (fun (x__1487:int) -> (x__1486 x__1487)): (int ->
X)
abst_arg: x__1487, int
abst_arg: x__1487, int
abstract_term: (x__1486 x__1487): X
abstract_term: x__1487: int
abstract_term: x__1485: int
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))))
                   m_1118 (fun (x__1480:int) -> (k_array_max_1145 x__1480)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))))
                 m_1118 (fun (x__1480:int) -> (k_array_max_1145 x__1480))): X
abstract_term: (fun (x__1480:int) -> (k_array_max_1145 x__1480)): (int ->
X)
abst_arg: x__1480, int
abst_arg: x__1480, int
abstract_term: (k_array_max_1145 x__1480): X
abstract_term: x__1480: int
abstract_term: m_1118: int
abstract_term: (fun (x__1481:int) (x__1482:(int -> X)) -> (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483)))): (
int -> (int -> X) -> X)
abst_arg: x__1481, int
abst_arg: x__1482, (int ->
X)
abst_arg: x__1481, int
abst_arg: x__1482, (int ->
X)
abstract_term: (x_1117 x__1481 (fun (x__1483:int) -> (x__1482 x__1483))): X
abstract_term: (fun (x__1483:int) -> (x__1482 x__1483)): (int ->
X)
abst_arg: x__1483, int
abst_arg: x__1483, int
abstract_term: (x__1482 x__1483): X
abstract_term: x__1483: int
abstract_term: x__1481: int
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
abstract_term: i_1116: int
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X), b_1314:x_1:bool[
x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
                 (fun (m_1445:int) ->
                  (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
   (fun (m_1445:int) ->
    (if (n_1019 <= m_1445) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
                  (fun (m_1445:int) ->
                   (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
                   (fun (m_1445:int) ->
                    (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))) -1
                 (fun (m_1445:int) ->
                  (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))))))): X
abstract_term: (fun (m_1445:int) ->
                (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488)))))): (int ->
X)
abst_arg: m_1445, int
abst_arg: m_1445, int
abstract_term: (if (n_1019 <= m_1445) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))))): X
abstract_term: (n_1019 <= m_1445): x_1:bool[x_1]
cond: b_1314
pbs: b_1314 := b_1314
p:(n_1019 <= m_1445)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
abstract_term: (l1 (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488)))): X
abstract_term: (fail_1341 true (fun (x__1488:unit) -> (k_main_1201 x__1488))): X
abstract_term: (fun (x__1488:unit) -> (k_main_1201 x__1488)): (unit ->
X)
abst_arg: x__1488, unit
abst_arg: x__1488, unit
abstract_term: (k_main_1201 x__1488): X
abstract_term: x__1488: unit
abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_1445)); b_1314
pbs: b_1314 := b_1314
p:true
tt:true
ff:false

abstract_term: -1: int
abstract_term: (fun (i_1463:int) (k_make_array_1464:(int -> X)) -> (k_make_array_1464 (n_1019 - i_1463))): (
int -> (int -> X) -> X)
abst_arg: i_1463, int
abst_arg: k_make_array_1464, (int ->
X)
abst_arg: i_1463, int
abst_arg: k_make_array_1464, (int ->
X)
abstract_term: (k_make_array_1464 (n_1019 - i_1463)): X
abstract_term: (n_1019 - i_1463): int
abstract_term: i_1020: int
abstract_term: n_1019: int
k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X), b_1314:x_1:bool[
x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () rand_bool)) (l1 (k_main_1208 () false))))
        (l1 (k_main_1208 () false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 k_array_max_1145).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 k_array_max_1145)) (l1 (array_max_1011 x_1117 k_array_max_1145))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b_1314 ->
      (l0
        (array_max_1011 (fun k_make_array_1464 -> k_make_array_1464)
          (if rand_bool (l0 k_main_1201) (l1 (fail_1341 true k_main_1201))))).
  k_main_1208 k_main_1201 b_1314 -> (if b_1314 _|_ (l1 k_main_1201)).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () rand_bool)) (l1 (k_main_1208 () false))))
        (l1 (k_main_1208 () false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 k_array_max_1145).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 k_array_max_1145)) (l1 (array_max_1011 x_1117 k_array_max_1145))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b_1314 ->
      (l0 (array_max_1011 f_1490 (if rand_bool (l0 k_main_1201) (l1 (fail_1341 true k_main_1201))))).
  f_1490 k_make_array_1464 -> k_make_array_1464.
  k_main_1208 k_main_1201 b_1314 -> (if b_1314 _|_ (l1 k_main_1201)).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_1492 b_1491 =
       (if b_1491
         (l0
           (let f_1494 b_1493 =
            (if b_1493
              (l0 (let f_1496 b_1495 = (k_main_1208 () b_1495) in (if rand_bool (f_1496 true) (f_1496 false))))
              (l1 (k_main_1208 () false)))
            in (if rand_bool (f_1494 true) (f_1494 false)))) (l1 (k_main_1208 () false)))
       in (if rand_bool (f_1492 true) (f_1492 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 k_array_max_1145).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_1498 b_1497 =
           (if b_1497 (l0 (array_max_1011 x_1117 k_array_max_1145)) (l1 (array_max_1011 x_1117 k_array_max_1145))) in
           (if rand_bool (f_1498 true) (f_1498 false))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b_1314 ->
      (l0
        (array_max_1011 f_1490
          (let f_1500 b_1499 = (if b_1499 (l0 k_main_1201) (l1 (fail_1341 true k_main_1201))) in
           (if rand_bool (f_1500 true) (f_1500 false))))).
  f_1490 k_make_array_1464 -> k_make_array_1464.
  k_main_1208 k_main_1201 b_1314 -> (let f_1502 b_1501 = (if b_1501 _|_ (l1 k_main_1201)) in (f_1502 b_1314)).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_1492 b_1491 =
       (if b_1491
         (l0
           (let f_1494 b_1493 =
            (if b_1493
              (l0 (let f_1496 b_1495 = (k_main_1208 () b_1495) in (if rand_bool (f_1496 true) (f_1496 false))))
              (l1 (k_main_1208 () false)))
            in (if rand_bool (f_1494 true) (f_1494 false)))) (l1 (k_main_1208 () false)))
       in (if rand_bool (f_1492 true) (f_1492 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 k_array_max_1145).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_1498 b_1497 =
           (if b_1497 (l0 (array_max_1011 x_1117 k_array_max_1145)) (l1 (array_max_1011 x_1117 k_array_max_1145))) in
           (if rand_bool (f_1498 true) (f_1498 false))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b_1314 ->
      (l0
        (array_max_1011 f_1490
          (let f_1500 b_1499 = (if b_1499 (l0 k_main_1201) (l1 (fail_1341 true k_main_1201))) in
           (if rand_bool (f_1500 true) (f_1500 false))))).
  f_1490 k_make_array_1464 -> k_make_array_1464.
  k_main_1208 k_main_1201 b_1314 -> (let f_1502 b_1501 = (if b_1501 _|_ (l1 k_main_1201)) in (f_1502 b_1314)).

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1332 ... --> 
  main_1089 ... --> 
  arg1_1085 ... --> 
  f_main_1336 ... --> 
  arg2_1087 ... --> 
  f_main_1337 ... --> 
  main_1018 [1/2] ... --> 
  br_main_1339 [1/2] ... --> 
  k_main_1208 [1/2] ... --> 
  m_1316 ... --> 
  array_max_1011 [1/2] ... --> 
  f_k_main_1335 [2/2] ... --> 
  fail_1341 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
Dagsolve Begins
Treesolve Begins
Num restricted : 1
M1 Solve Begins
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(1-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_1900 ->
         (rand_int
           (fun arg2_1903 ->
            (if (arg1_1900 > 0)
              (l0
                (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
                  (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
              (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_1881 ->
           (if (x_1881 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_1916 k_make_array_1917 -> (k_make_array_1917 (n_1019 - i_1916))) -1
          (fun m_1898 -> (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_1900 ->
        (rand_int
          (fun arg2_1903 ->
           (if (arg1_1900 > 0)
             (l0
               (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
                 (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
             (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))))): X
ETA: (fun arg1_1900 ->
      (rand_int
        (fun arg2_1903 ->
         (if (arg1_1900 > 0)
           (l0
             (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
               (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
           (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_1903 ->
        (if (arg1_1900 > 0)
          (l0
            (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
              (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
          (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))): unit
ETA: (fun arg2_1903 ->
      (if (arg1_1900 > 0)
        (l0
          (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
            (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
        (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false)))): (int ->
unit)
ETA: (if (arg1_1900 > 0)
       (l0
         (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
           (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))))
       (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false)): unit
ETA: (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
ETA: (fun main_1875 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1900: int
ETA: arg2_1903: int
ETA_AUX: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false): unit
ETA: (l0
       (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
         (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false)))): unit
ETA: (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)))
       (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false)): unit
ETA: (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
ETA: (fun main_1875 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1900: int
ETA: arg2_1903: int
ETA_AUX: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0))): unit
ETA: (k_main_1208 arg2_1903 arg1_1900 (fun main_1875 -> end) (arg2_1903 <= 0)): unit
ETA: (arg2_1903 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
ETA: (fun main_1875 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_1900: int
ETA: arg2_1903: int
ETA_AUX: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_1903:int) ->
            (if (arg1_1900 > 0)
              (l0
                (if (arg2_1903 >= 0)
                  (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                  (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
              (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_1900:int) ->
            (rand_int
              (fun (arg2_1903:int) ->
               (if (arg1_1900 > 0)
                 (l0
                   (if (arg2_1903 >= 0)
                     (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                     (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_1881 ->
          (if (x_1881 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_1881 ->
        (if (x_1881 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_1881 ->
      (if (x_1881 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_1881 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__1933: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__1933): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__1934: int
ETA_AUX: (x_1117 x__1934): ((int -> X) ->
X)
ETA_AUX: x__1935: (int ->
X)
ETA_AUX: x__1936: int
ETA_AUX: (x__1935 x__1936): X
ETA_AUX: (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
           (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_1881 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__1937: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__1937): X
ETA: x_1881: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__1938: int
ETA_AUX: (x_1117 x__1938): ((int -> X) ->
X)
ETA_AUX: x__1939: (int ->
X)
ETA_AUX: x__1940: int
ETA_AUX: (x__1939 x__1940): X
ETA_AUX: (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
           (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_1881:int) ->
            (if (x_1881 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))))
                  x_1881 (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))))
                  m_1118 (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_1916 k_make_array_1917 -> (k_make_array_1917 (n_1019 - i_1916))) -1
         (fun m_1898 -> (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_1916 k_make_array_1917 -> (k_make_array_1917 (n_1019 - i_1916))) -1
       (fun m_1898 -> (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_1898 -> (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__1941: unit
ETA_AUX: (k_main_1201 x__1941): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_1916 k_make_array_1917 -> (k_make_array_1917 (n_1019 - i_1916))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_1917 -> (k_make_array_1917 (n_1019 - i_1916))): ((int -> X) ->
X)
ETA: (k_make_array_1917 (n_1019 - i_1916)): X
ETA: (n_1019 - i_1916): int
ETA_AUX: (k_make_array_1917 (n_1019 - i_1916)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
           (fun (m_1898:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_1900:int) ->
         (rand_int
           (fun (arg2_1903:int) ->
            (if (arg1_1900 > 0)
              (l0
                (if (arg2_1903 >= 0)
                  (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                  (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
              (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_1881:int) ->
           (if (x_1881 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))))
                 x_1881 (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))))
                 m_1118 (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
          (fun (m_1898:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_1900:int) ->
              (rand_int
                (fun (arg2_1903:int) ->
                 (if (arg1_1900 > 0)
                   (l0
                     (if (arg2_1903 >= 0)
                       (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                       (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_1900:int) ->
  (rand_int
    (fun (arg2_1903:int) ->
     (if (arg1_1900 > 0)
       (l0
         (if (arg2_1903 >= 0) (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
           (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
       (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_1900:int) ->
               (rand_int
                 (fun (arg2_1903:int) ->
                  (if (arg1_1900 > 0)
                    (l0
                      (if (arg2_1903 >= 0)
                        (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                        (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_1900:int) ->
                  (rand_int
                    (fun (arg2_1903:int) ->
                     (if (arg1_1900 > 0)
                       (l0
                         (if (arg2_1903 >= 0)
                           (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                           (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))))): X
abstract_term: (fun (arg1_1900:int) ->
                (rand_int
                  (fun (arg2_1903:int) ->
                   (if (arg1_1900 > 0)
                     (l0
                       (if (arg2_1903 >= 0)
                         (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                         (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_1900, int
abst_arg: arg1_1900, int
abstract_term: (rand_int
                 (fun (arg2_1903:int) ->
                  (if (arg1_1900 > 0)
                    (l0
                      (if (arg2_1903 >= 0)
                        (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                        (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))): X
abstract_term: (fun (arg2_1903:int) ->
                (if (arg1_1900 > 0)
                  (l0
                    (if (arg2_1903 >= 0)
                      (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                      (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_1903, int
abst_arg: arg2_1903, int
abstract_term: (if (arg1_1900 > 0)
                 (l0
                   (if (arg2_1903 >= 0)
                     (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                     (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))): X
abstract_term: (arg1_1900 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_1900 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_1903 >= 0)
                   (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                   (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)))): X
abstract_term: (if (arg2_1903 >= 0)
                 (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)))
                 (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false))): X
abstract_term: (arg2_1903 >= 0): x_1:bool[x_1]
cond: (arg1_1900 > 0); true
pbs: 
p:(arg2_1903 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0))): X
abstract_term: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) (arg2_1903 <= 0)): X
abstract_term: (arg2_1903 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
cond: (arg2_1903 >= 0); (arg1_1900 > 0); true
pbs: 
p:((not (arg2_1903 <= 0)) || ((1 <= ((-1 * arg2_1903) + arg1_1900)) && (arg2_1903 <= 0)))
tt:true
ff:false

cond: (arg2_1903 >= 0); (arg1_1900 > 0); true
pbs: 
p:(arg2_1903 <= 0)
tt:false
ff:false

abstract_term: (fun (main_1875:unit) -> end): (unit ->
X)
abst_arg: main_1875, unit
abst_arg: main_1875, unit
abstract_term: end: X
abstract_term: arg1_1900: int
abstract_term: arg2_1903: int
abstract_term: (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
cond: (not (arg2_1903 >= 0)); (arg1_1900 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_1903) + arg1_1900)) && false))
tt:true
ff:false

cond: (not (arg2_1903 >= 0)); (arg1_1900 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_1875:unit) -> end): (unit ->
X)
abst_arg: main_1875, unit
abst_arg: main_1875, unit
abstract_term: end: X
abstract_term: arg1_1900: int
abstract_term: arg2_1903: int
abstract_term: (l1 (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_1903 arg1_1900 (fun (main_1875:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_1903 + arg1_1900 && x_1; x_1]
cond: (not (arg1_1900 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_1903) + arg1_1900)) && false))
tt:true
ff:false

cond: (not (arg1_1900 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_1875:unit) -> end): (unit ->
X)
abst_arg: main_1875, unit
abst_arg: main_1875, unit
abstract_term: end: X
abstract_term: arg1_1900: int
abstract_term: arg2_1903: int
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_1881:int) ->
                     (if (x_1881 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__1938:int) (x__1939:(int -> X)) ->
                            (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                           (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__1934:int) (x__1935:(int -> X)) ->
                            (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                           (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_1881:int) ->
    (if (x_1881 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
          (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
          (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_1881:int) ->
                      (if (x_1881 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__1938:int) (x__1939:(int -> X)) ->
                             (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                            (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__1934:int) (x__1935:(int -> X)) ->
                             (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                            (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_1881:int) ->
                    (if (x_1881 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__1938:int) (x__1939:(int -> X)) ->
                           (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                          (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__1934:int) (x__1935:(int -> X)) ->
                           (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                          (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_1881:int) ->
                  (if (x_1881 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__1938:int) (x__1939:(int -> X)) ->
                         (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                        (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__1934:int) (x__1935:(int -> X)) ->
                         (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                        (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))))))): X
abstract_term: (fun (x_1881:int) ->
                (if (x_1881 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__1938:int) (x__1939:(int -> X)) ->
                       (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                      (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__1934:int) (x__1935:(int -> X)) ->
                       (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                      (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)))))): (int ->
X)
abst_arg: x_1881, int
abst_arg: x_1881, int
abstract_term: (if (x_1881 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__1938:int) (x__1939:(int -> X)) ->
                      (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))) x_1881
                     (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__1934:int) (x__1935:(int -> X)) ->
                      (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))) m_1118
                     (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))))): X
abstract_term: (x_1881 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_1881 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))))
                   x_1881 (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))))
                 x_1881 (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937))): X
abstract_term: (fun (x__1937:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1937)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__1937, int[i_1116 + 1 >= x_1028]
abst_arg: x__1937, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__1937): X
abstract_term: x__1937: int[i_1116 >= x_1028]
cond: (x_1881 > m_1118); (not (i_1116 >= x_1028))
pbs: x__1937 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

abstract_term: x_1881: int
abstract_term: (fun (x__1938:int) (x__1939:(int -> X)) -> (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940)))): (
int -> (int -> X) -> X)
abst_arg: x__1938, int
abst_arg: x__1939, (int ->
X)
abst_arg: x__1938, int
abst_arg: x__1939, (int ->
X)
abstract_term: (x_1117 x__1938 (fun (x__1940:int) -> (x__1939 x__1940))): X
abstract_term: (fun (x__1940:int) -> (x__1939 x__1940)): (int ->
X)
abst_arg: x__1940, int
abst_arg: x__1940, int
abstract_term: (x__1939 x__1940): X
abstract_term: x__1940: int
abstract_term: x__1938: int
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))))
                   m_1118 (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))))
                 m_1118 (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933))): X
abstract_term: (fun (x__1933:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__1933)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__1933, int[i_1116 + 1 >= x_1028]
abst_arg: x__1933, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__1933): X
abstract_term: x__1933: int[i_1116 >= x_1028]
cond: (not (x_1881 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__1933 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

abstract_term: m_1118: int
abstract_term: (fun (x__1934:int) (x__1935:(int -> X)) -> (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936)))): (
int -> (int -> X) -> X)
abst_arg: x__1934, int
abst_arg: x__1935, (int ->
X)
abst_arg: x__1934, int
abst_arg: x__1935, (int ->
X)
abstract_term: (x_1117 x__1934 (fun (x__1936:int) -> (x__1935 x__1936))): X
abstract_term: (fun (x__1936:int) -> (x__1935 x__1936)): (int ->
X)
abst_arg: x__1936, int
abst_arg: x__1936, int
abstract_term: (x__1935 x__1936): X
abstract_term: x__1936: int
abstract_term: x__1934: int
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
abstract_term: i_1116: int
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
                 (fun (m_1898:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
   (fun (m_1898:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_1898) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
                  (fun (m_1898:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
                   (fun (m_1898:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))) -1
                 (fun (m_1898:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))))))): X
abstract_term: (fun (m_1898:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_1898, int[i_1020 >= n_1019]
abst_arg: m_1898, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_1898) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))))): X
abstract_term: (n_1019 <= m_1898): x_1:bool[x_1]
cond: b_1314
pbs: m_1898 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_1898)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
abstract_term: (l1 (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941)))): X
abstract_term: (fail_1341 true (fun (x__1941:unit) -> (k_main_1201 x__1941))): X
abstract_term: (fun (x__1941:unit) -> (k_main_1201 x__1941)): (unit ->
X)
abst_arg: x__1941, unit
abst_arg: x__1941, unit
abstract_term: (k_main_1201 x__1941): X
abstract_term: x__1941: unit
abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_1898)); b_1314
pbs: m_1898 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

abstract_term: -1: int
abstract_term: (fun (i_1916:int) (k_make_array_1917:(int -> X)) -> (k_make_array_1917 (n_1019 - i_1916))): (
int -> (int -> X) -> X)
abst_arg: i_1916, int
abst_arg: k_make_array_1917, (int ->
X)
abst_arg: i_1916, int
abst_arg: k_make_array_1917, (int ->
X)
abstract_term: (k_make_array_1917 (n_1019 - i_1916)): X
abstract_term: (n_1019 - i_1916): int
abstract_term: i_1020: int
abstract_term: n_1019: int
k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__1937 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__1933 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (l0
        (array_max_1011 (fun k_make_array_1917 -> k_make_array_1917)
          (fun m_1898 -> (if rand_bool (l0 k_main_1201) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (if b2_1314 _|_ (l1 k_main_1201)).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_1944 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_1947 k_array_max_1145)))))).
  f_1944 k_array_max_1943 x__1937 -> (k_array_max_1943 false).
  f_1947 k_array_max_1946 x__1933 -> (k_array_max_1946 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_1949 (f_1952 k_main_1201))).
  f_1949 k_make_array_1917 -> k_make_array_1917.
  f_1952 k_main_1951 m_1898 -> (if rand_bool (l0 k_main_1951) (l1 (fail_1341 true k_main_1951))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (if b2_1314 _|_ (l1 k_main_1201)).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_1954 b_1953 =
       (if b_1953
         (l0
           (let f_1956 b_1955 =
            (if b_1955
              (l0 (let f_1958 b_1957 = (k_main_1208 () true b_1957) in (if rand_bool (f_1958 true) (f_1958 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_1956 true) (f_1956 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_1954 true) (f_1954 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_1960 b_1959 =
           (if b_1959 (l0 (array_max_1011 x_1117 (f_1944 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_1947 k_array_max_1145))))
           in (if rand_bool (f_1960 true) (f_1960 false))))).
  f_1944 k_array_max_1943 x__1937 -> (k_array_max_1943 false).
  f_1947 k_array_max_1946 x__1933 -> (k_array_max_1946 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_1949 (f_1952 k_main_1201))).
  f_1949 k_make_array_1917 -> k_make_array_1917.
  f_1952 k_main_1951 m_1898 ->
      (let f_1962 b_1961 = (if b_1961 (l0 k_main_1951) (l1 (fail_1341 true k_main_1951))) in
       (if rand_bool (f_1962 true) (f_1962 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_1964 b_1963 = (if b_1963 _|_ (l1 k_main_1201)) in (f_1964 b2_1314)).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_1954 b_1953 =
       (if b_1953
         (l0
           (let f_1956 b_1955 =
            (if b_1955
              (l0 (let f_1958 b_1957 = (k_main_1208 () true b_1957) in (if rand_bool (f_1958 true) (f_1958 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_1956 true) (f_1956 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_1954 true) (f_1954 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_1960 b_1959 =
           (if b_1959 (l0 (array_max_1011 x_1117 (f_1944 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_1947 k_array_max_1145))))
           in (if rand_bool (f_1960 true) (f_1960 false))))).
  f_1944 k_array_max_1943 x__1937 -> (k_array_max_1943 false).
  f_1947 k_array_max_1946 x__1933 -> (k_array_max_1946 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_1949 (f_1952 k_main_1201))).
  f_1949 k_make_array_1917 -> k_make_array_1917.
  f_1952 k_main_1951 m_1898 ->
      (let f_1962 b_1961 = (if b_1961 (l0 k_main_1951) (l1 (fail_1341 true k_main_1951))) in
       (if rand_bool (f_1962 true) (f_1962 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_1964 b_1963 = (if b_1963 _|_ (l1 k_main_1201)) in (f_1964 b2_1314)).

DONE!

(1-2) Checking HORS ... DONE!

Filter option enabled.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 2)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(2-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_2086 ->
         (rand_int
           (fun arg2_2089 ->
            (if (arg1_2086 > 0)
              (l0
                (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
                  (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
              (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_2067 ->
           (if (x_2067 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_2102 k_make_array_2103 -> (k_make_array_2103 (n_1019 - i_2102))) -1
          (fun m_2084 -> (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_2086 ->
        (rand_int
          (fun arg2_2089 ->
           (if (arg1_2086 > 0)
             (l0
               (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
                 (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
             (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))))): X
ETA: (fun arg1_2086 ->
      (rand_int
        (fun arg2_2089 ->
         (if (arg1_2086 > 0)
           (l0
             (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
               (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
           (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_2089 ->
        (if (arg1_2086 > 0)
          (l0
            (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
              (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
          (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))): unit
ETA: (fun arg2_2089 ->
      (if (arg1_2086 > 0)
        (l0
          (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
            (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
        (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false)))): (int ->
unit)
ETA: (if (arg1_2086 > 0)
       (l0
         (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
           (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))))
       (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false)): unit
ETA: (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
ETA: (fun main_2061 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2086: int
ETA: arg2_2089: int
ETA_AUX: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false): unit
ETA: (l0
       (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
         (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false)))): unit
ETA: (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)))
       (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false)): unit
ETA: (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
ETA: (fun main_2061 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2086: int
ETA: arg2_2089: int
ETA_AUX: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0))): unit
ETA: (k_main_1208 arg2_2089 arg1_2086 (fun main_2061 -> end) (arg2_2089 <= 0)): unit
ETA: (arg2_2089 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
ETA: (fun main_2061 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2086: int
ETA: arg2_2089: int
ETA_AUX: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_2089:int) ->
            (if (arg1_2086 > 0)
              (l0
                (if (arg2_2089 >= 0)
                  (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                  (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_2086:int) ->
            (rand_int
              (fun (arg2_2089:int) ->
               (if (arg1_2086 > 0)
                 (l0
                   (if (arg2_2089 >= 0)
                     (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                     (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_2067 ->
          (if (x_2067 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_2067 ->
        (if (x_2067 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_2067 ->
      (if (x_2067 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_2067 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2119: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2119): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2120: int
ETA_AUX: (x_1117 x__2120): ((int -> X) ->
X)
ETA_AUX: x__2121: (int ->
X)
ETA_AUX: x__2122: int
ETA_AUX: (x__2121 x__2122): X
ETA_AUX: (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
           (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2067 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2123: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2123): X
ETA: x_2067: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2124: int
ETA_AUX: (x_1117 x__2124): ((int -> X) ->
X)
ETA_AUX: x__2125: (int ->
X)
ETA_AUX: x__2126: int
ETA_AUX: (x__2125 x__2126): X
ETA_AUX: (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
           (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_2067:int) ->
            (if (x_2067 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))))
                  x_2067 (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))))
                  m_1118 (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_2102 k_make_array_2103 -> (k_make_array_2103 (n_1019 - i_2102))) -1
         (fun m_2084 -> (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_2102 k_make_array_2103 -> (k_make_array_2103 (n_1019 - i_2102))) -1
       (fun m_2084 -> (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_2084 -> (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__2127: unit
ETA_AUX: (k_main_1201 x__2127): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_2102 k_make_array_2103 -> (k_make_array_2103 (n_1019 - i_2102))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_2103 -> (k_make_array_2103 (n_1019 - i_2102))): ((int -> X) ->
X)
ETA: (k_make_array_2103 (n_1019 - i_2102)): X
ETA: (n_1019 - i_2102): int
ETA_AUX: (k_make_array_2103 (n_1019 - i_2102)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
           (fun (m_2084:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_2086:int) ->
         (rand_int
           (fun (arg2_2089:int) ->
            (if (arg1_2086 > 0)
              (l0
                (if (arg2_2089 >= 0)
                  (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                  (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_2067:int) ->
           (if (x_2067 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))))
                 x_2067 (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))))
                 m_1118 (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
          (fun (m_2084:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_2086:int) ->
              (rand_int
                (fun (arg2_2089:int) ->
                 (if (arg1_2086 > 0)
                   (l0
                     (if (arg2_2089 >= 0)
                       (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                       (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_2086:int) ->
  (rand_int
    (fun (arg2_2089:int) ->
     (if (arg1_2086 > 0)
       (l0
         (if (arg2_2089 >= 0) (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
           (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
       (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_2086:int) ->
               (rand_int
                 (fun (arg2_2089:int) ->
                  (if (arg1_2086 > 0)
                    (l0
                      (if (arg2_2089 >= 0)
                        (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                        (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_2086:int) ->
                  (rand_int
                    (fun (arg2_2089:int) ->
                     (if (arg1_2086 > 0)
                       (l0
                         (if (arg2_2089 >= 0)
                           (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                           (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))))): X
abstract_term: (fun (arg1_2086:int) ->
                (rand_int
                  (fun (arg2_2089:int) ->
                   (if (arg1_2086 > 0)
                     (l0
                       (if (arg2_2089 >= 0)
                         (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                         (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_2086, int
abst_arg: arg1_2086, int
abstract_term: (rand_int
                 (fun (arg2_2089:int) ->
                  (if (arg1_2086 > 0)
                    (l0
                      (if (arg2_2089 >= 0)
                        (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                        (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))): X
abstract_term: (fun (arg2_2089:int) ->
                (if (arg1_2086 > 0)
                  (l0
                    (if (arg2_2089 >= 0)
                      (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                      (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_2089, int
abst_arg: arg2_2089, int
abstract_term: (if (arg1_2086 > 0)
                 (l0
                   (if (arg2_2089 >= 0)
                     (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                     (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))): X
abstract_term: (arg1_2086 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_2086 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_2089 >= 0)
                   (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                   (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)))): X
abstract_term: (if (arg2_2089 >= 0)
                 (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)))
                 (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false))): X
abstract_term: (arg2_2089 >= 0): x_1:bool[x_1]
cond: (arg1_2086 > 0); true
pbs: 
p:(arg2_2089 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0))): X
abstract_term: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) (arg2_2089 <= 0)): X
abstract_term: (arg2_2089 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
cond: (arg2_2089 >= 0); (arg1_2086 > 0); true
pbs: 
p:((not (arg2_2089 <= 0)) || ((1 <= ((-1 * arg2_2089) + arg1_2086)) && (arg2_2089 <= 0)))
tt:true
ff:false

cond: (arg2_2089 >= 0); (arg1_2086 > 0); true
pbs: 
p:(arg2_2089 <= 0)
tt:false
ff:false

abstract_term: (fun (main_2061:unit) -> end): (unit ->
X)
abst_arg: main_2061, unit
abst_arg: main_2061, unit
abstract_term: end: X
abstract_term: arg1_2086: int
abstract_term: arg2_2089: int
filter
cond: (arg2_2089 >= 0); (arg1_2086 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
cond: (not (arg2_2089 >= 0)); (arg1_2086 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2089) + arg1_2086)) && false))
tt:true
ff:false

cond: (not (arg2_2089 >= 0)); (arg1_2086 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2061:unit) -> end): (unit ->
X)
abst_arg: main_2061, unit
abst_arg: main_2061, unit
abstract_term: end: X
abstract_term: arg1_2086: int
abstract_term: arg2_2089: int
filter
cond: (not (arg2_2089 >= 0)); (arg1_2086 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2089 arg1_2086 (fun (main_2061:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2089 + arg1_2086 && x_1; x_1]
cond: (not (arg1_2086 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2089) + arg1_2086)) && false))
tt:true
ff:false

cond: (not (arg1_2086 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2061:unit) -> end): (unit ->
X)
abst_arg: main_2061, unit
abst_arg: main_2061, unit
abstract_term: end: X
abstract_term: arg1_2086: int
abstract_term: arg2_2089: int
filter
cond: (not (arg1_2086 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_2067:int) ->
                     (if (x_2067 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2124:int) (x__2125:(int -> X)) ->
                            (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                           (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2120:int) (x__2121:(int -> X)) ->
                            (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                           (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_2067:int) ->
    (if (x_2067 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
          (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
          (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_2067:int) ->
                      (if (x_2067 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2124:int) (x__2125:(int -> X)) ->
                             (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                            (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2120:int) (x__2121:(int -> X)) ->
                             (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                            (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_2067:int) ->
                    (if (x_2067 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2124:int) (x__2125:(int -> X)) ->
                           (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                          (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2120:int) (x__2121:(int -> X)) ->
                           (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                          (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_2067:int) ->
                  (if (x_2067 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2124:int) (x__2125:(int -> X)) ->
                         (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                        (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2120:int) (x__2121:(int -> X)) ->
                         (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                        (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))))))): X
abstract_term: (fun (x_2067:int) ->
                (if (x_2067 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2124:int) (x__2125:(int -> X)) ->
                       (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                      (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2120:int) (x__2121:(int -> X)) ->
                       (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                      (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)))))): (int ->
X)
abst_arg: x_2067, int
abst_arg: x_2067, int
abstract_term: (if (x_2067 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2124:int) (x__2125:(int -> X)) ->
                      (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))) x_2067
                     (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2120:int) (x__2121:(int -> X)) ->
                      (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))) m_1118
                     (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))))): X
abstract_term: (x_2067 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_2067 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))))
                   x_2067 (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))))
                 x_2067 (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123))): X
abstract_term: (fun (x__2123:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2123)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2123, int[i_1116 + 1 >= x_1028]
abst_arg: x__2123, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2123): X
abstract_term: x__2123: int[i_1116 >= x_1028]
cond: (x_2067 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2123 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_2067 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2123 := ((i_1116 + 1) >= x_1028)

abstract_term: x_2067: int
abstract_term: (fun (x__2124:int) (x__2125:(int -> X)) -> (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126)))): (
int -> (int -> X) -> X)
abst_arg: x__2124, int
abst_arg: x__2125, (int ->
X)
abst_arg: x__2124, int
abst_arg: x__2125, (int ->
X)
abstract_term: (x_1117 x__2124 (fun (x__2126:int) -> (x__2125 x__2126))): X
abstract_term: (fun (x__2126:int) -> (x__2125 x__2126)): (int ->
X)
abst_arg: x__2126, int
abst_arg: x__2126, int
abstract_term: (x__2125 x__2126): X
abstract_term: x__2126: int
filter
cond: (x_2067 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__2124: int
filter
cond: (x_2067 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_2067 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))))
                   m_1118 (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))))
                 m_1118 (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119))): X
abstract_term: (fun (x__2119:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2119)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2119, int[i_1116 + 1 >= x_1028]
abst_arg: x__2119, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2119): X
abstract_term: x__2119: int[i_1116 >= x_1028]
cond: (not (x_2067 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2119 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_2067 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2119 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__2120:int) (x__2121:(int -> X)) -> (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122)))): (
int -> (int -> X) -> X)
abst_arg: x__2120, int
abst_arg: x__2121, (int ->
X)
abst_arg: x__2120, int
abst_arg: x__2121, (int ->
X)
abstract_term: (x_1117 x__2120 (fun (x__2122:int) -> (x__2121 x__2122))): X
abstract_term: (fun (x__2122:int) -> (x__2121 x__2122)): (int ->
X)
abst_arg: x__2122, int
abst_arg: x__2122, int
abstract_term: (x__2121 x__2122): X
abstract_term: x__2122: int
filter
cond: (not (x_2067 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__2120: int
filter
cond: (not (x_2067 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_2067 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
                 (fun (m_2084:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
   (fun (m_2084:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_2084) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
                  (fun (m_2084:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
                   (fun (m_2084:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))) -1
                 (fun (m_2084:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))))))): X
abstract_term: (fun (m_2084:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_2084, int[i_1020 >= n_1019]
abst_arg: m_2084, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_2084) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))))): X
abstract_term: (n_1019 <= m_2084): x_1:bool[x_1]
cond: b_1314
pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_2084)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_2084); b_1314
pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127)))): X
abstract_term: (fail_1341 true (fun (x__2127:unit) -> (k_main_1201 x__2127))): X
abstract_term: (fun (x__2127:unit) -> (k_main_1201 x__2127)): (unit ->
X)
abst_arg: x__2127, unit
abst_arg: x__2127, unit
abstract_term: (k_main_1201 x__2127): X
abstract_term: x__2127: unit
filter
cond: (not (n_1019 <= m_2084)); b_1314
pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_2084)); b_1314
pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_2084)); b_1314
pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2084 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_2102:int) (k_make_array_2103:(int -> X)) -> (k_make_array_2103 (n_1019 - i_2102))): (
int -> (int -> X) -> X)
abst_arg: i_2102, int
abst_arg: k_make_array_2103, (int ->
X)
abst_arg: i_2102, int
abst_arg: k_make_array_2103, (int ->
X)
abstract_term: (k_make_array_2103 (n_1019 - i_2102)): X
abstract_term: (n_1019 - i_2102): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__2123 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__2119 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (l0
        (array_max_1011 (fun k_make_array_2103 -> k_make_array_2103)
          (fun m_2084 -> (if rand_bool (l0 k_main_1201) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (if b2_1314 _|_ (l1 (if (b2_1314 || b2_1314) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_2130 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_2133 k_array_max_1145)))))).
  f_2130 k_array_max_2129 x__2123 -> (k_array_max_2129 false).
  f_2133 k_array_max_2132 x__2119 -> (k_array_max_2132 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_2135 (f_2138 k_main_1201))).
  f_2135 k_make_array_2103 -> k_make_array_2103.
  f_2138 k_main_2137 m_2084 -> (if rand_bool (l0 k_main_2137) (l1 (fail_1341 true k_main_2137))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (if b2_1314 _|_ (l1 (if (b2_1314 || b2_1314) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_2140 b_2139 =
       (if b_2139
         (l0
           (let f_2142 b_2141 =
            (if b_2141
              (l0 (let f_2144 b_2143 = (k_main_1208 () true b_2143) in (if rand_bool (f_2144 true) (f_2144 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2142 true) (f_2142 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2140 true) (f_2140 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2146 b_2145 =
           (if b_2145 (l0 (array_max_1011 x_1117 (f_2130 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2133 k_array_max_1145))))
           in (if rand_bool (f_2146 true) (f_2146 false))))).
  f_2130 k_array_max_2129 x__2123 -> (k_array_max_2129 false).
  f_2133 k_array_max_2132 x__2119 -> (k_array_max_2132 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_2135 (f_2138 k_main_1201))).
  f_2135 k_make_array_2103 -> k_make_array_2103.
  f_2138 k_main_2137 m_2084 ->
      (let f_2148 b_2147 = (if b_2147 (l0 k_main_2137) (l1 (fail_1341 true k_main_2137))) in
       (if rand_bool (f_2148 true) (f_2148 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2150 b_2149 =
       (if b_2149 _|_
         (l1
           (let f_2152 b_2151 = (if b_2151 _|_ k_main_1201) in
            (let f_2154 b_2153 = (if b_2153 (f_2152 true) (f_2152 b2_1314)) in (f_2154 b2_1314)))))
       in (f_2150 b2_1314)).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_2140 b_2139 =
       (if b_2139
         (l0
           (let f_2142 b_2141 =
            (if b_2141
              (l0 (let f_2144 b_2143 = (k_main_1208 () true b_2143) in (if rand_bool (f_2144 true) (f_2144 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2142 true) (f_2142 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2140 true) (f_2140 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2146 b_2145 =
           (if b_2145 (l0 (array_max_1011 x_1117 (f_2130 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2133 k_array_max_1145))))
           in (if rand_bool (f_2146 true) (f_2146 false))))).
  f_2130 k_array_max_2129 x__2123 -> (k_array_max_2129 false).
  f_2133 k_array_max_2132 x__2119 -> (k_array_max_2132 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 -> (l0 (array_max_1011 f_2135 (f_2138 k_main_1201))).
  f_2135 k_make_array_2103 -> k_make_array_2103.
  f_2138 k_main_2137 m_2084 ->
      (let f_2148 b_2147 = (if b_2147 (l0 k_main_2137) (l1 (fail_1341 true k_main_2137))) in
       (if rand_bool (f_2148 true) (f_2148 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2150 b_2149 =
       (if b_2149 _|_
         (l1
           (let f_2152 b_2151 = (if b_2151 _|_ k_main_1201) in
            (let f_2154 b_2153 = (if b_2153 (f_2152 true) (f_2152 b2_1314)) in (f_2154 b2_1314)))))
       in (f_2150 b2_1314)).

DONE!

(2-2) Checking HORS ... DONE!

Negative-predicate option enabled.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 3)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(3-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_2282 ->
         (rand_int
           (fun arg2_2285 ->
            (if (arg1_2282 > 0)
              (l0
                (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
                  (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
              (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_2263 ->
           (if (x_2263 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_2298 k_make_array_2299 -> (k_make_array_2299 (n_1019 - i_2298))) -1
          (fun m_2280 -> (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_2282 ->
        (rand_int
          (fun arg2_2285 ->
           (if (arg1_2282 > 0)
             (l0
               (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
                 (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
             (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))))): X
ETA: (fun arg1_2282 ->
      (rand_int
        (fun arg2_2285 ->
         (if (arg1_2282 > 0)
           (l0
             (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
               (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
           (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_2285 ->
        (if (arg1_2282 > 0)
          (l0
            (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
              (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
          (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))): unit
ETA: (fun arg2_2285 ->
      (if (arg1_2282 > 0)
        (l0
          (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
            (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
        (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false)))): (int ->
unit)
ETA: (if (arg1_2282 > 0)
       (l0
         (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
           (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))))
       (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false)): unit
ETA: (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
ETA: (fun main_2257 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2282: int
ETA: arg2_2285: int
ETA_AUX: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false): unit
ETA: (l0
       (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
         (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false)))): unit
ETA: (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)))
       (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false)): unit
ETA: (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
ETA: (fun main_2257 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2282: int
ETA: arg2_2285: int
ETA_AUX: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0))): unit
ETA: (k_main_1208 arg2_2285 arg1_2282 (fun main_2257 -> end) (arg2_2285 <= 0)): unit
ETA: (arg2_2285 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
ETA: (fun main_2257 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2282: int
ETA: arg2_2285: int
ETA_AUX: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_2285:int) ->
            (if (arg1_2282 > 0)
              (l0
                (if (arg2_2285 >= 0)
                  (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                  (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_2282:int) ->
            (rand_int
              (fun (arg2_2285:int) ->
               (if (arg1_2282 > 0)
                 (l0
                   (if (arg2_2285 >= 0)
                     (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                     (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_2263 ->
          (if (x_2263 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_2263 ->
        (if (x_2263 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_2263 ->
      (if (x_2263 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_2263 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2315: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2315): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2316: int
ETA_AUX: (x_1117 x__2316): ((int -> X) ->
X)
ETA_AUX: x__2317: (int ->
X)
ETA_AUX: x__2318: int
ETA_AUX: (x__2317 x__2318): X
ETA_AUX: (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
           (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2263 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2319: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2319): X
ETA: x_2263: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2320: int
ETA_AUX: (x_1117 x__2320): ((int -> X) ->
X)
ETA_AUX: x__2321: (int ->
X)
ETA_AUX: x__2322: int
ETA_AUX: (x__2321 x__2322): X
ETA_AUX: (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
           (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_2263:int) ->
            (if (x_2263 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))))
                  x_2263 (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))))
                  m_1118 (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_2298 k_make_array_2299 -> (k_make_array_2299 (n_1019 - i_2298))) -1
         (fun m_2280 -> (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_2298 k_make_array_2299 -> (k_make_array_2299 (n_1019 - i_2298))) -1
       (fun m_2280 -> (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_2280 -> (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__2323: unit
ETA_AUX: (k_main_1201 x__2323): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_2298 k_make_array_2299 -> (k_make_array_2299 (n_1019 - i_2298))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_2299 -> (k_make_array_2299 (n_1019 - i_2298))): ((int -> X) ->
X)
ETA: (k_make_array_2299 (n_1019 - i_2298)): X
ETA: (n_1019 - i_2298): int
ETA_AUX: (k_make_array_2299 (n_1019 - i_2298)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
           (fun (m_2280:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_2282:int) ->
         (rand_int
           (fun (arg2_2285:int) ->
            (if (arg1_2282 > 0)
              (l0
                (if (arg2_2285 >= 0)
                  (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                  (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_2263:int) ->
           (if (x_2263 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))))
                 x_2263 (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))))
                 m_1118 (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
          (fun (m_2280:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_2282:int) ->
              (rand_int
                (fun (arg2_2285:int) ->
                 (if (arg1_2282 > 0)
                   (l0
                     (if (arg2_2285 >= 0)
                       (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                       (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_2282:int) ->
  (rand_int
    (fun (arg2_2285:int) ->
     (if (arg1_2282 > 0)
       (l0
         (if (arg2_2285 >= 0) (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
           (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
       (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_2282:int) ->
               (rand_int
                 (fun (arg2_2285:int) ->
                  (if (arg1_2282 > 0)
                    (l0
                      (if (arg2_2285 >= 0)
                        (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                        (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_2282:int) ->
                  (rand_int
                    (fun (arg2_2285:int) ->
                     (if (arg1_2282 > 0)
                       (l0
                         (if (arg2_2285 >= 0)
                           (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                           (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))))): X
abstract_term: (fun (arg1_2282:int) ->
                (rand_int
                  (fun (arg2_2285:int) ->
                   (if (arg1_2282 > 0)
                     (l0
                       (if (arg2_2285 >= 0)
                         (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                         (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_2282, int
abst_arg: arg1_2282, int
abstract_term: (rand_int
                 (fun (arg2_2285:int) ->
                  (if (arg1_2282 > 0)
                    (l0
                      (if (arg2_2285 >= 0)
                        (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                        (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))): X
abstract_term: (fun (arg2_2285:int) ->
                (if (arg1_2282 > 0)
                  (l0
                    (if (arg2_2285 >= 0)
                      (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                      (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_2285, int
abst_arg: arg2_2285, int
abstract_term: (if (arg1_2282 > 0)
                 (l0
                   (if (arg2_2285 >= 0)
                     (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                     (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))): X
abstract_term: (arg1_2282 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_2282 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_2285 >= 0)
                   (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                   (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)))): X
abstract_term: (if (arg2_2285 >= 0)
                 (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)))
                 (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false))): X
abstract_term: (arg2_2285 >= 0): x_1:bool[x_1]
cond: (arg1_2282 > 0); true
pbs: 
p:(arg2_2285 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0))): X
abstract_term: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) (arg2_2285 <= 0)): X
abstract_term: (arg2_2285 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
cond: (arg2_2285 >= 0); (arg1_2282 > 0); true
pbs: 
p:((not (arg2_2285 <= 0)) || ((1 <= ((-1 * arg2_2285) + arg1_2282)) && (arg2_2285 <= 0)))
tt:true
ff:false

cond: (arg2_2285 >= 0); (arg1_2282 > 0); true
pbs: 
p:(arg2_2285 <= 0)
tt:false
ff:false

abstract_term: (fun (main_2257:unit) -> end): (unit ->
X)
abst_arg: main_2257, unit
abst_arg: main_2257, unit
abstract_term: end: X
abstract_term: arg1_2282: int
abstract_term: arg2_2285: int
filter
cond: (arg2_2285 >= 0); (arg1_2282 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
cond: (not (arg2_2285 >= 0)); (arg1_2282 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2285) + arg1_2282)) && false))
tt:true
ff:false

cond: (not (arg2_2285 >= 0)); (arg1_2282 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2257:unit) -> end): (unit ->
X)
abst_arg: main_2257, unit
abst_arg: main_2257, unit
abstract_term: end: X
abstract_term: arg1_2282: int
abstract_term: arg2_2285: int
filter
cond: (not (arg2_2285 >= 0)); (arg1_2282 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2285 arg1_2282 (fun (main_2257:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2285 + arg1_2282 && x_1; x_1]
cond: (not (arg1_2282 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2285) + arg1_2282)) && false))
tt:true
ff:false

cond: (not (arg1_2282 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2257:unit) -> end): (unit ->
X)
abst_arg: main_2257, unit
abst_arg: main_2257, unit
abstract_term: end: X
abstract_term: arg1_2282: int
abstract_term: arg2_2285: int
filter
cond: (not (arg1_2282 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_2263:int) ->
                     (if (x_2263 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2320:int) (x__2321:(int -> X)) ->
                            (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                           (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2316:int) (x__2317:(int -> X)) ->
                            (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                           (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_2263:int) ->
    (if (x_2263 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
          (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
          (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_2263:int) ->
                      (if (x_2263 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2320:int) (x__2321:(int -> X)) ->
                             (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                            (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2316:int) (x__2317:(int -> X)) ->
                             (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                            (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_2263:int) ->
                    (if (x_2263 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2320:int) (x__2321:(int -> X)) ->
                           (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                          (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2316:int) (x__2317:(int -> X)) ->
                           (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                          (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_2263:int) ->
                  (if (x_2263 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2320:int) (x__2321:(int -> X)) ->
                         (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                        (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2316:int) (x__2317:(int -> X)) ->
                         (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                        (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))))))): X
abstract_term: (fun (x_2263:int) ->
                (if (x_2263 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2320:int) (x__2321:(int -> X)) ->
                       (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                      (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2316:int) (x__2317:(int -> X)) ->
                       (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                      (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)))))): (int ->
X)
abst_arg: x_2263, int
abst_arg: x_2263, int
abstract_term: (if (x_2263 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2320:int) (x__2321:(int -> X)) ->
                      (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))) x_2263
                     (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2316:int) (x__2317:(int -> X)) ->
                      (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))) m_1118
                     (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))))): X
abstract_term: (x_2263 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_2263 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))))
                   x_2263 (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))))
                 x_2263 (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319))): X
abstract_term: (fun (x__2319:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2319)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2319, int[i_1116 + 1 >= x_1028]
abst_arg: x__2319, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2319): X
abstract_term: x__2319: int[i_1116 >= x_1028]
cond: (x_2263 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2319 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_2263 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2319 := ((i_1116 + 1) >= x_1028)

abstract_term: x_2263: int
abstract_term: (fun (x__2320:int) (x__2321:(int -> X)) -> (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322)))): (
int -> (int -> X) -> X)
abst_arg: x__2320, int
abst_arg: x__2321, (int ->
X)
abst_arg: x__2320, int
abst_arg: x__2321, (int ->
X)
abstract_term: (x_1117 x__2320 (fun (x__2322:int) -> (x__2321 x__2322))): X
abstract_term: (fun (x__2322:int) -> (x__2321 x__2322)): (int ->
X)
abst_arg: x__2322, int
abst_arg: x__2322, int
abstract_term: (x__2321 x__2322): X
abstract_term: x__2322: int
filter
cond: (x_2263 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__2320: int
filter
cond: (x_2263 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_2263 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))))
                   m_1118 (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))))
                 m_1118 (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315))): X
abstract_term: (fun (x__2315:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2315)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2315, int[i_1116 + 1 >= x_1028]
abst_arg: x__2315, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2315): X
abstract_term: x__2315: int[i_1116 >= x_1028]
cond: (not (x_2263 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2315 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_2263 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2315 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__2316:int) (x__2317:(int -> X)) -> (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318)))): (
int -> (int -> X) -> X)
abst_arg: x__2316, int
abst_arg: x__2317, (int ->
X)
abst_arg: x__2316, int
abst_arg: x__2317, (int ->
X)
abstract_term: (x_1117 x__2316 (fun (x__2318:int) -> (x__2317 x__2318))): X
abstract_term: (fun (x__2318:int) -> (x__2317 x__2318)): (int ->
X)
abst_arg: x__2318, int
abst_arg: x__2318, int
abstract_term: (x__2317 x__2318): X
abstract_term: x__2318: int
filter
cond: (not (x_2263 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__2316: int
filter
cond: (not (x_2263 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_2263 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
                 (fun (m_2280:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
   (fun (m_2280:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_2280) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
                  (fun (m_2280:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
                   (fun (m_2280:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))) -1
                 (fun (m_2280:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))))))): X
abstract_term: (fun (m_2280:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_2280, int[i_1020 >= n_1019]
abst_arg: m_2280, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_2280) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))))): X
abstract_term: (n_1019 <= m_2280): x_1:bool[x_1]
cond: b_1314
pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_2280)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_2280); b_1314
pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323)))): X
abstract_term: (fail_1341 true (fun (x__2323:unit) -> (k_main_1201 x__2323))): X
abstract_term: (fun (x__2323:unit) -> (k_main_1201 x__2323)): (unit ->
X)
abst_arg: x__2323, unit
abst_arg: x__2323, unit
abstract_term: (k_main_1201 x__2323): X
abstract_term: x__2323: unit
filter
cond: (not (n_1019 <= m_2280)); b_1314
pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_2280)); b_1314
pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_2280)); b_1314
pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2280 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_2298:int) (k_make_array_2299:(int -> X)) -> (k_make_array_2299 (n_1019 - i_2298))): (
int -> (int -> X) -> X)
abst_arg: i_2298, int
abst_arg: k_make_array_2299, (int ->
X)
abst_arg: i_2298, int
abst_arg: k_make_array_2299, (int ->
X)
abstract_term: (k_make_array_2299 (n_1019 - i_2298)): X
abstract_term: (n_1019 - i_2298): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__2319 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__2315 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_2299 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_2299))
              (fun m_2280 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_2326 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_2329 k_array_max_1145)))))).
  f_2326 k_array_max_2325 x__2319 -> (k_array_max_2325 false).
  f_2329 k_array_max_2328 x__2315 -> (k_array_max_2328 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_2332 b2_1314) (f_2336 k_main_1201 b2_1314))))).
  f_2332 b2_2331 k_make_array_2299 -> (if ((not b2_2331) || (not b2_2331)) _|_ k_make_array_2299).
  f_2336 k_main_2334 b2_2335 m_2280 ->
      (if rand_bool (l0 (if ((not b2_2335) || (not b2_2335)) _|_ k_main_2334))
        (l1
          (if ((not b2_2335) || (not b2_2335)) _|_
            (fail_1341 true (if ((not b2_2335) || (not b2_2335)) _|_ k_main_2334))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_2338 b_2337 =
       (if b_2337
         (l0
           (let f_2340 b_2339 =
            (if b_2339
              (l0 (let f_2342 b_2341 = (k_main_1208 () true b_2341) in (if rand_bool (f_2342 true) (f_2342 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2340 true) (f_2340 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2338 true) (f_2338 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2344 b_2343 =
           (if b_2343 (l0 (array_max_1011 x_1117 (f_2326 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2329 k_array_max_1145))))
           in (if rand_bool (f_2344 true) (f_2344 false))))).
  f_2326 k_array_max_2325 x__2319 -> (k_array_max_2325 false).
  f_2329 k_array_max_2328 x__2315 -> (k_array_max_2328 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2346 b_2345 =
       (if b_2345 _|_
         (l0
           (let f_2348 b_2347 = (if b_2347 _|_ (array_max_1011 (f_2332 b2_1314) (f_2336 k_main_1201 b2_1314))) in
            (let f_2350 b_2349 =
             (if b_2349 (f_2348 true)
               (let f_2354 b_2353 = (if b_2353 (f_2348 false) (f_2348 true)) in (f_2354 b2_1314)))
             in (let f_2352 b_2351 = (if b_2351 (f_2350 false) (f_2350 true)) in (f_2352 b2_1314))))))
       in (let f_2356 b_2355 = (if b_2355 (f_2346 false) (f_2346 true)) in (f_2356 b2_1314))).
  f_2332 b2_2331 k_make_array_2299 ->
      (let f_2358 b_2357 = (if b_2357 _|_ k_make_array_2299) in
       (let f_2360 b_2359 =
        (if b_2359 (f_2358 true) (let f_2364 b_2363 = (if b_2363 (f_2358 false) (f_2358 true)) in (f_2364 b2_2331))) in
        (let f_2362 b_2361 = (if b_2361 (f_2360 false) (f_2360 true)) in (f_2362 b2_2331)))).
  f_2336 k_main_2334 b2_2335 m_2280 ->
      (let f_2366 b_2365 =
       (if b_2365
         (l0
           (let f_2368 b_2367 = (if b_2367 _|_ k_main_2334) in
            (let f_2370 b_2369 =
             (if b_2369 (f_2368 true)
               (let f_2374 b_2373 = (if b_2373 (f_2368 false) (f_2368 true)) in (f_2374 b2_2335)))
             in (let f_2372 b_2371 = (if b_2371 (f_2370 false) (f_2370 true)) in (f_2372 b2_2335)))))
         (l1
           (let f_2376 b_2375 =
            (if b_2375 _|_
              (fail_1341 true
                (let f_2378 b_2377 = (if b_2377 _|_ k_main_2334) in
                 (let f_2380 b_2379 =
                  (if b_2379 (f_2378 true)
                    (let f_2384 b_2383 = (if b_2383 (f_2378 false) (f_2378 true)) in (f_2384 b2_2335)))
                  in (let f_2382 b_2381 = (if b_2381 (f_2380 false) (f_2380 true)) in (f_2382 b2_2335))))))
            in
            (let f_2386 b_2385 =
             (if b_2385 (f_2376 true)
               (let f_2390 b_2389 = (if b_2389 (f_2376 false) (f_2376 true)) in (f_2390 b2_2335)))
             in (let f_2388 b_2387 = (if b_2387 (f_2386 false) (f_2386 true)) in (f_2388 b2_2335))))))
       in (if rand_bool (f_2366 true) (f_2366 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2392 b_2391 =
       (if b_2391 _|_
         (l1
           (let f_2394 b_2393 = (if b_2393 _|_ k_main_1201) in
            (let f_2396 b_2395 =
             (if b_2395 (f_2394 true)
               (let f_2402 b_2401 =
                (if b_2401 (f_2394 true)
                  (let f_2404 b_2403 = (if b_2403 (f_2394 false) (f_2394 true)) in (f_2404 b1_1314)))
                in (f_2402 b2_1314)))
             in
             (let f_2398 b_2397 =
              (if b_2397 (f_2396 true)
                (let f_2400 b_2399 = (if b_2399 (f_2396 false) (f_2396 true)) in (f_2400 b1_1314)))
              in (f_2398 b2_1314))))))
       in
       (let f_2406 b_2405 =
        (if b_2405 (f_2392 true) (let f_2408 b_2407 = (if b_2407 (f_2392 false) (f_2392 true)) in (f_2408 b1_1314))) in
        (f_2406 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_2338 b_2337 =
       (if b_2337
         (l0
           (let f_2340 b_2339 =
            (if b_2339
              (l0 (let f_2342 b_2341 = (k_main_1208 () true b_2341) in (if rand_bool (f_2342 true) (f_2342 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2340 true) (f_2340 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2338 true) (f_2338 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2344 b_2343 =
           (if b_2343 (l0 (array_max_1011 x_1117 (f_2326 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2329 k_array_max_1145))))
           in (if rand_bool (f_2344 true) (f_2344 false))))).
  f_2326 k_array_max_2325 x__2319 -> (k_array_max_2325 false).
  f_2329 k_array_max_2328 x__2315 -> (k_array_max_2328 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2346 b_2345 =
       (if b_2345 _|_
         (l0
           (let f_2348 b_2347 = (if b_2347 _|_ (array_max_1011 (f_2332 b2_1314) (f_2336 k_main_1201 b2_1314))) in
            (let f_2350 b_2349 =
             (if b_2349 (f_2348 true)
               (let f_2354 b_2353 = (if b_2353 (f_2348 false) (f_2348 true)) in (f_2354 b2_1314)))
             in (let f_2352 b_2351 = (if b_2351 (f_2350 false) (f_2350 true)) in (f_2352 b2_1314))))))
       in (let f_2356 b_2355 = (if b_2355 (f_2346 false) (f_2346 true)) in (f_2356 b2_1314))).
  f_2332 b2_2331 k_make_array_2299 ->
      (let f_2358 b_2357 = (if b_2357 _|_ k_make_array_2299) in
       (let f_2360 b_2359 =
        (if b_2359 (f_2358 true) (let f_2364 b_2363 = (if b_2363 (f_2358 false) (f_2358 true)) in (f_2364 b2_2331))) in
        (let f_2362 b_2361 = (if b_2361 (f_2360 false) (f_2360 true)) in (f_2362 b2_2331)))).
  f_2336 k_main_2334 b2_2335 m_2280 ->
      (let f_2366 b_2365 =
       (if b_2365
         (l0
           (let f_2368 b_2367 = (if b_2367 _|_ k_main_2334) in
            (let f_2370 b_2369 =
             (if b_2369 (f_2368 true)
               (let f_2374 b_2373 = (if b_2373 (f_2368 false) (f_2368 true)) in (f_2374 b2_2335)))
             in (let f_2372 b_2371 = (if b_2371 (f_2370 false) (f_2370 true)) in (f_2372 b2_2335)))))
         (l1
           (let f_2376 b_2375 =
            (if b_2375 _|_
              (fail_1341 true
                (let f_2378 b_2377 = (if b_2377 _|_ k_main_2334) in
                 (let f_2380 b_2379 =
                  (if b_2379 (f_2378 true)
                    (let f_2384 b_2383 = (if b_2383 (f_2378 false) (f_2378 true)) in (f_2384 b2_2335)))
                  in (let f_2382 b_2381 = (if b_2381 (f_2380 false) (f_2380 true)) in (f_2382 b2_2335))))))
            in
            (let f_2386 b_2385 =
             (if b_2385 (f_2376 true)
               (let f_2390 b_2389 = (if b_2389 (f_2376 false) (f_2376 true)) in (f_2390 b2_2335)))
             in (let f_2388 b_2387 = (if b_2387 (f_2386 false) (f_2386 true)) in (f_2388 b2_2335))))))
       in (if rand_bool (f_2366 true) (f_2366 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2392 b_2391 =
       (if b_2391 _|_
         (l1
           (let f_2394 b_2393 = (if b_2393 _|_ k_main_1201) in
            (let f_2396 b_2395 =
             (if b_2395 (f_2394 true)
               (let f_2402 b_2401 =
                (if b_2401 (f_2394 true)
                  (let f_2404 b_2403 = (if b_2403 (f_2394 false) (f_2394 true)) in (f_2404 b1_1314)))
                in (f_2402 b2_1314)))
             in
             (let f_2398 b_2397 =
              (if b_2397 (f_2396 true)
                (let f_2400 b_2399 = (if b_2399 (f_2396 false) (f_2396 true)) in (f_2400 b1_1314)))
              in (f_2398 b2_1314))))))
       in
       (let f_2406 b_2405 =
        (if b_2405 (f_2392 true) (let f_2408 b_2407 = (if b_2407 (f_2392 false) (f_2392 true)) in (f_2408 b1_1314))) in
        (f_2406 b2_1314))).

DONE!

(3-2) Checking HORS ... DONE!

Set wp_max_num to 4.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 4)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(4-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_2620 ->
         (rand_int
           (fun arg2_2623 ->
            (if (arg1_2620 > 0)
              (l0
                (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
                  (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
              (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_2601 ->
           (if (x_2601 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_2636 k_make_array_2637 -> (k_make_array_2637 (n_1019 - i_2636))) -1
          (fun m_2618 -> (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_2620 ->
        (rand_int
          (fun arg2_2623 ->
           (if (arg1_2620 > 0)
             (l0
               (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
                 (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
             (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))))): X
ETA: (fun arg1_2620 ->
      (rand_int
        (fun arg2_2623 ->
         (if (arg1_2620 > 0)
           (l0
             (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
               (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
           (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_2623 ->
        (if (arg1_2620 > 0)
          (l0
            (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
              (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
          (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))): unit
ETA: (fun arg2_2623 ->
      (if (arg1_2620 > 0)
        (l0
          (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
            (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
        (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false)))): (int ->
unit)
ETA: (if (arg1_2620 > 0)
       (l0
         (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
           (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))))
       (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false)): unit
ETA: (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
ETA: (fun main_2595 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2620: int
ETA: arg2_2623: int
ETA_AUX: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false): unit
ETA: (l0
       (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
         (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false)))): unit
ETA: (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)))
       (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false)): unit
ETA: (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
ETA: (fun main_2595 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2620: int
ETA: arg2_2623: int
ETA_AUX: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0))): unit
ETA: (k_main_1208 arg2_2623 arg1_2620 (fun main_2595 -> end) (arg2_2623 <= 0)): unit
ETA: (arg2_2623 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
ETA: (fun main_2595 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2620: int
ETA: arg2_2623: int
ETA_AUX: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_2623:int) ->
            (if (arg1_2620 > 0)
              (l0
                (if (arg2_2623 >= 0)
                  (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                  (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_2620:int) ->
            (rand_int
              (fun (arg2_2623:int) ->
               (if (arg1_2620 > 0)
                 (l0
                   (if (arg2_2623 >= 0)
                     (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                     (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_2601 ->
          (if (x_2601 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_2601 ->
        (if (x_2601 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_2601 ->
      (if (x_2601 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_2601 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2653: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2653): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2654: int
ETA_AUX: (x_1117 x__2654): ((int -> X) ->
X)
ETA_AUX: x__2655: (int ->
X)
ETA_AUX: x__2656: int
ETA_AUX: (x__2655 x__2656): X
ETA_AUX: (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
           (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2601 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2657: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2657): X
ETA: x_2601: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2658: int
ETA_AUX: (x_1117 x__2658): ((int -> X) ->
X)
ETA_AUX: x__2659: (int ->
X)
ETA_AUX: x__2660: int
ETA_AUX: (x__2659 x__2660): X
ETA_AUX: (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
           (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_2601:int) ->
            (if (x_2601 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))))
                  x_2601 (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))))
                  m_1118 (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_2636 k_make_array_2637 -> (k_make_array_2637 (n_1019 - i_2636))) -1
         (fun m_2618 -> (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_2636 k_make_array_2637 -> (k_make_array_2637 (n_1019 - i_2636))) -1
       (fun m_2618 -> (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_2618 -> (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__2661: unit
ETA_AUX: (k_main_1201 x__2661): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_2636 k_make_array_2637 -> (k_make_array_2637 (n_1019 - i_2636))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_2637 -> (k_make_array_2637 (n_1019 - i_2636))): ((int -> X) ->
X)
ETA: (k_make_array_2637 (n_1019 - i_2636)): X
ETA: (n_1019 - i_2636): int
ETA_AUX: (k_make_array_2637 (n_1019 - i_2636)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
           (fun (m_2618:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_2620:int) ->
         (rand_int
           (fun (arg2_2623:int) ->
            (if (arg1_2620 > 0)
              (l0
                (if (arg2_2623 >= 0)
                  (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                  (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_2601:int) ->
           (if (x_2601 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))))
                 x_2601 (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))))
                 m_1118 (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
          (fun (m_2618:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_2620:int) ->
              (rand_int
                (fun (arg2_2623:int) ->
                 (if (arg1_2620 > 0)
                   (l0
                     (if (arg2_2623 >= 0)
                       (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                       (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_2620:int) ->
  (rand_int
    (fun (arg2_2623:int) ->
     (if (arg1_2620 > 0)
       (l0
         (if (arg2_2623 >= 0) (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
           (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
       (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_2620:int) ->
               (rand_int
                 (fun (arg2_2623:int) ->
                  (if (arg1_2620 > 0)
                    (l0
                      (if (arg2_2623 >= 0)
                        (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                        (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_2620:int) ->
                  (rand_int
                    (fun (arg2_2623:int) ->
                     (if (arg1_2620 > 0)
                       (l0
                         (if (arg2_2623 >= 0)
                           (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                           (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))))): X
abstract_term: (fun (arg1_2620:int) ->
                (rand_int
                  (fun (arg2_2623:int) ->
                   (if (arg1_2620 > 0)
                     (l0
                       (if (arg2_2623 >= 0)
                         (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                         (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_2620, int
abst_arg: arg1_2620, int
abstract_term: (rand_int
                 (fun (arg2_2623:int) ->
                  (if (arg1_2620 > 0)
                    (l0
                      (if (arg2_2623 >= 0)
                        (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                        (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))): X
abstract_term: (fun (arg2_2623:int) ->
                (if (arg1_2620 > 0)
                  (l0
                    (if (arg2_2623 >= 0)
                      (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                      (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_2623, int
abst_arg: arg2_2623, int
abstract_term: (if (arg1_2620 > 0)
                 (l0
                   (if (arg2_2623 >= 0)
                     (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                     (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))): X
abstract_term: (arg1_2620 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_2620 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_2623 >= 0)
                   (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                   (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)))): X
abstract_term: (if (arg2_2623 >= 0)
                 (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)))
                 (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false))): X
abstract_term: (arg2_2623 >= 0): x_1:bool[x_1]
cond: (arg1_2620 > 0); true
pbs: 
p:(arg2_2623 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0))): X
abstract_term: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) (arg2_2623 <= 0)): X
abstract_term: (arg2_2623 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
cond: (arg2_2623 >= 0); (arg1_2620 > 0); true
pbs: 
p:((not (arg2_2623 <= 0)) || ((1 <= ((-1 * arg2_2623) + arg1_2620)) && (arg2_2623 <= 0)))
tt:true
ff:false

cond: (arg2_2623 >= 0); (arg1_2620 > 0); true
pbs: 
p:(arg2_2623 <= 0)
tt:false
ff:false

abstract_term: (fun (main_2595:unit) -> end): (unit ->
X)
abst_arg: main_2595, unit
abst_arg: main_2595, unit
abstract_term: end: X
abstract_term: arg1_2620: int
abstract_term: arg2_2623: int
filter
cond: (arg2_2623 >= 0); (arg1_2620 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
cond: (not (arg2_2623 >= 0)); (arg1_2620 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2623) + arg1_2620)) && false))
tt:true
ff:false

cond: (not (arg2_2623 >= 0)); (arg1_2620 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2595:unit) -> end): (unit ->
X)
abst_arg: main_2595, unit
abst_arg: main_2595, unit
abstract_term: end: X
abstract_term: arg1_2620: int
abstract_term: arg2_2623: int
filter
cond: (not (arg2_2623 >= 0)); (arg1_2620 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2623 arg1_2620 (fun (main_2595:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2623 + arg1_2620 && x_1; x_1]
cond: (not (arg1_2620 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2623) + arg1_2620)) && false))
tt:true
ff:false

cond: (not (arg1_2620 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2595:unit) -> end): (unit ->
X)
abst_arg: main_2595, unit
abst_arg: main_2595, unit
abstract_term: end: X
abstract_term: arg1_2620: int
abstract_term: arg2_2623: int
filter
cond: (not (arg1_2620 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_2601:int) ->
                     (if (x_2601 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2658:int) (x__2659:(int -> X)) ->
                            (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                           (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2654:int) (x__2655:(int -> X)) ->
                            (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                           (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_2601:int) ->
    (if (x_2601 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
          (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
          (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_2601:int) ->
                      (if (x_2601 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2658:int) (x__2659:(int -> X)) ->
                             (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                            (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2654:int) (x__2655:(int -> X)) ->
                             (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                            (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_2601:int) ->
                    (if (x_2601 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2658:int) (x__2659:(int -> X)) ->
                           (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                          (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2654:int) (x__2655:(int -> X)) ->
                           (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                          (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_2601:int) ->
                  (if (x_2601 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2658:int) (x__2659:(int -> X)) ->
                         (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                        (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2654:int) (x__2655:(int -> X)) ->
                         (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                        (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))))))): X
abstract_term: (fun (x_2601:int) ->
                (if (x_2601 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2658:int) (x__2659:(int -> X)) ->
                       (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                      (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2654:int) (x__2655:(int -> X)) ->
                       (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                      (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)))))): (int ->
X)
abst_arg: x_2601, int
abst_arg: x_2601, int
abstract_term: (if (x_2601 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2658:int) (x__2659:(int -> X)) ->
                      (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))) x_2601
                     (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2654:int) (x__2655:(int -> X)) ->
                      (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))) m_1118
                     (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))))): X
abstract_term: (x_2601 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_2601 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))))
                   x_2601 (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))))
                 x_2601 (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657))): X
abstract_term: (fun (x__2657:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2657)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2657, int[i_1116 + 1 >= x_1028]
abst_arg: x__2657, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2657): X
abstract_term: x__2657: int[i_1116 >= x_1028]
cond: (x_2601 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2657 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_2601 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2657 := ((i_1116 + 1) >= x_1028)

abstract_term: x_2601: int
abstract_term: (fun (x__2658:int) (x__2659:(int -> X)) -> (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660)))): (
int -> (int -> X) -> X)
abst_arg: x__2658, int
abst_arg: x__2659, (int ->
X)
abst_arg: x__2658, int
abst_arg: x__2659, (int ->
X)
abstract_term: (x_1117 x__2658 (fun (x__2660:int) -> (x__2659 x__2660))): X
abstract_term: (fun (x__2660:int) -> (x__2659 x__2660)): (int ->
X)
abst_arg: x__2660, int
abst_arg: x__2660, int
abstract_term: (x__2659 x__2660): X
abstract_term: x__2660: int
filter
cond: (x_2601 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__2658: int
filter
cond: (x_2601 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_2601 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))))
                   m_1118 (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))))
                 m_1118 (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653))): X
abstract_term: (fun (x__2653:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2653)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2653, int[i_1116 + 1 >= x_1028]
abst_arg: x__2653, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2653): X
abstract_term: x__2653: int[i_1116 >= x_1028]
cond: (not (x_2601 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2653 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_2601 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2653 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__2654:int) (x__2655:(int -> X)) -> (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656)))): (
int -> (int -> X) -> X)
abst_arg: x__2654, int
abst_arg: x__2655, (int ->
X)
abst_arg: x__2654, int
abst_arg: x__2655, (int ->
X)
abstract_term: (x_1117 x__2654 (fun (x__2656:int) -> (x__2655 x__2656))): X
abstract_term: (fun (x__2656:int) -> (x__2655 x__2656)): (int ->
X)
abst_arg: x__2656, int
abst_arg: x__2656, int
abstract_term: (x__2655 x__2656): X
abstract_term: x__2656: int
filter
cond: (not (x_2601 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__2654: int
filter
cond: (not (x_2601 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_2601 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
                 (fun (m_2618:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
   (fun (m_2618:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_2618) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
                  (fun (m_2618:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
                   (fun (m_2618:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))) -1
                 (fun (m_2618:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))))))): X
abstract_term: (fun (m_2618:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_2618, int[i_1020 >= n_1019]
abst_arg: m_2618, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_2618) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))))): X
abstract_term: (n_1019 <= m_2618): x_1:bool[x_1]
cond: b_1314
pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_2618)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_2618); b_1314
pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661)))): X
abstract_term: (fail_1341 true (fun (x__2661:unit) -> (k_main_1201 x__2661))): X
abstract_term: (fun (x__2661:unit) -> (k_main_1201 x__2661)): (unit ->
X)
abst_arg: x__2661, unit
abst_arg: x__2661, unit
abstract_term: (k_main_1201 x__2661): X
abstract_term: x__2661: unit
filter
cond: (not (n_1019 <= m_2618)); b_1314
pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_2618)); b_1314
pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_2618)); b_1314
pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2618 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_2636:int) (k_make_array_2637:(int -> X)) -> (k_make_array_2637 (n_1019 - i_2636))): (
int -> (int -> X) -> X)
abst_arg: i_2636, int
abst_arg: k_make_array_2637, (int ->
X)
abst_arg: i_2636, int
abst_arg: k_make_array_2637, (int ->
X)
abstract_term: (k_make_array_2637 (n_1019 - i_2636)): X
abstract_term: (n_1019 - i_2636): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__2657 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__2653 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_2637 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_2637))
              (fun m_2618 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_2664 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_2667 k_array_max_1145)))))).
  f_2664 k_array_max_2663 x__2657 -> (k_array_max_2663 false).
  f_2667 k_array_max_2666 x__2653 -> (k_array_max_2666 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_2670 b2_1314) (f_2674 k_main_1201 b2_1314))))).
  f_2670 b2_2669 k_make_array_2637 -> (if ((not b2_2669) || (not b2_2669)) _|_ k_make_array_2637).
  f_2674 k_main_2672 b2_2673 m_2618 ->
      (if rand_bool (l0 (if ((not b2_2673) || (not b2_2673)) _|_ k_main_2672))
        (l1
          (if ((not b2_2673) || (not b2_2673)) _|_
            (fail_1341 true (if ((not b2_2673) || (not b2_2673)) _|_ k_main_2672))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_2676 b_2675 =
       (if b_2675
         (l0
           (let f_2678 b_2677 =
            (if b_2677
              (l0 (let f_2680 b_2679 = (k_main_1208 () true b_2679) in (if rand_bool (f_2680 true) (f_2680 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2678 true) (f_2678 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2676 true) (f_2676 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2682 b_2681 =
           (if b_2681 (l0 (array_max_1011 x_1117 (f_2664 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2667 k_array_max_1145))))
           in (if rand_bool (f_2682 true) (f_2682 false))))).
  f_2664 k_array_max_2663 x__2657 -> (k_array_max_2663 false).
  f_2667 k_array_max_2666 x__2653 -> (k_array_max_2666 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2684 b_2683 =
       (if b_2683 _|_
         (l0
           (let f_2686 b_2685 = (if b_2685 _|_ (array_max_1011 (f_2670 b2_1314) (f_2674 k_main_1201 b2_1314))) in
            (let f_2688 b_2687 =
             (if b_2687 (f_2686 true)
               (let f_2692 b_2691 = (if b_2691 (f_2686 false) (f_2686 true)) in (f_2692 b2_1314)))
             in (let f_2690 b_2689 = (if b_2689 (f_2688 false) (f_2688 true)) in (f_2690 b2_1314))))))
       in (let f_2694 b_2693 = (if b_2693 (f_2684 false) (f_2684 true)) in (f_2694 b2_1314))).
  f_2670 b2_2669 k_make_array_2637 ->
      (let f_2696 b_2695 = (if b_2695 _|_ k_make_array_2637) in
       (let f_2698 b_2697 =
        (if b_2697 (f_2696 true) (let f_2702 b_2701 = (if b_2701 (f_2696 false) (f_2696 true)) in (f_2702 b2_2669))) in
        (let f_2700 b_2699 = (if b_2699 (f_2698 false) (f_2698 true)) in (f_2700 b2_2669)))).
  f_2674 k_main_2672 b2_2673 m_2618 ->
      (let f_2704 b_2703 =
       (if b_2703
         (l0
           (let f_2706 b_2705 = (if b_2705 _|_ k_main_2672) in
            (let f_2708 b_2707 =
             (if b_2707 (f_2706 true)
               (let f_2712 b_2711 = (if b_2711 (f_2706 false) (f_2706 true)) in (f_2712 b2_2673)))
             in (let f_2710 b_2709 = (if b_2709 (f_2708 false) (f_2708 true)) in (f_2710 b2_2673)))))
         (l1
           (let f_2714 b_2713 =
            (if b_2713 _|_
              (fail_1341 true
                (let f_2716 b_2715 = (if b_2715 _|_ k_main_2672) in
                 (let f_2718 b_2717 =
                  (if b_2717 (f_2716 true)
                    (let f_2722 b_2721 = (if b_2721 (f_2716 false) (f_2716 true)) in (f_2722 b2_2673)))
                  in (let f_2720 b_2719 = (if b_2719 (f_2718 false) (f_2718 true)) in (f_2720 b2_2673))))))
            in
            (let f_2724 b_2723 =
             (if b_2723 (f_2714 true)
               (let f_2728 b_2727 = (if b_2727 (f_2714 false) (f_2714 true)) in (f_2728 b2_2673)))
             in (let f_2726 b_2725 = (if b_2725 (f_2724 false) (f_2724 true)) in (f_2726 b2_2673))))))
       in (if rand_bool (f_2704 true) (f_2704 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2730 b_2729 =
       (if b_2729 _|_
         (l1
           (let f_2732 b_2731 = (if b_2731 _|_ k_main_1201) in
            (let f_2734 b_2733 =
             (if b_2733 (f_2732 true)
               (let f_2740 b_2739 =
                (if b_2739 (f_2732 true)
                  (let f_2742 b_2741 = (if b_2741 (f_2732 false) (f_2732 true)) in (f_2742 b1_1314)))
                in (f_2740 b2_1314)))
             in
             (let f_2736 b_2735 =
              (if b_2735 (f_2734 true)
                (let f_2738 b_2737 = (if b_2737 (f_2734 false) (f_2734 true)) in (f_2738 b1_1314)))
              in (f_2736 b2_1314))))))
       in
       (let f_2744 b_2743 =
        (if b_2743 (f_2730 true) (let f_2746 b_2745 = (if b_2745 (f_2730 false) (f_2730 true)) in (f_2746 b1_1314))) in
        (f_2744 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_2676 b_2675 =
       (if b_2675
         (l0
           (let f_2678 b_2677 =
            (if b_2677
              (l0 (let f_2680 b_2679 = (k_main_1208 () true b_2679) in (if rand_bool (f_2680 true) (f_2680 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_2678 true) (f_2678 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_2676 true) (f_2676 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_2682 b_2681 =
           (if b_2681 (l0 (array_max_1011 x_1117 (f_2664 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_2667 k_array_max_1145))))
           in (if rand_bool (f_2682 true) (f_2682 false))))).
  f_2664 k_array_max_2663 x__2657 -> (k_array_max_2663 false).
  f_2667 k_array_max_2666 x__2653 -> (k_array_max_2666 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2684 b_2683 =
       (if b_2683 _|_
         (l0
           (let f_2686 b_2685 = (if b_2685 _|_ (array_max_1011 (f_2670 b2_1314) (f_2674 k_main_1201 b2_1314))) in
            (let f_2688 b_2687 =
             (if b_2687 (f_2686 true)
               (let f_2692 b_2691 = (if b_2691 (f_2686 false) (f_2686 true)) in (f_2692 b2_1314)))
             in (let f_2690 b_2689 = (if b_2689 (f_2688 false) (f_2688 true)) in (f_2690 b2_1314))))))
       in (let f_2694 b_2693 = (if b_2693 (f_2684 false) (f_2684 true)) in (f_2694 b2_1314))).
  f_2670 b2_2669 k_make_array_2637 ->
      (let f_2696 b_2695 = (if b_2695 _|_ k_make_array_2637) in
       (let f_2698 b_2697 =
        (if b_2697 (f_2696 true) (let f_2702 b_2701 = (if b_2701 (f_2696 false) (f_2696 true)) in (f_2702 b2_2669))) in
        (let f_2700 b_2699 = (if b_2699 (f_2698 false) (f_2698 true)) in (f_2700 b2_2669)))).
  f_2674 k_main_2672 b2_2673 m_2618 ->
      (let f_2704 b_2703 =
       (if b_2703
         (l0
           (let f_2706 b_2705 = (if b_2705 _|_ k_main_2672) in
            (let f_2708 b_2707 =
             (if b_2707 (f_2706 true)
               (let f_2712 b_2711 = (if b_2711 (f_2706 false) (f_2706 true)) in (f_2712 b2_2673)))
             in (let f_2710 b_2709 = (if b_2709 (f_2708 false) (f_2708 true)) in (f_2710 b2_2673)))))
         (l1
           (let f_2714 b_2713 =
            (if b_2713 _|_
              (fail_1341 true
                (let f_2716 b_2715 = (if b_2715 _|_ k_main_2672) in
                 (let f_2718 b_2717 =
                  (if b_2717 (f_2716 true)
                    (let f_2722 b_2721 = (if b_2721 (f_2716 false) (f_2716 true)) in (f_2722 b2_2673)))
                  in (let f_2720 b_2719 = (if b_2719 (f_2718 false) (f_2718 true)) in (f_2720 b2_2673))))))
            in
            (let f_2724 b_2723 =
             (if b_2723 (f_2714 true)
               (let f_2728 b_2727 = (if b_2727 (f_2714 false) (f_2714 true)) in (f_2728 b2_2673)))
             in (let f_2726 b_2725 = (if b_2725 (f_2724 false) (f_2724 true)) in (f_2726 b2_2673))))))
       in (if rand_bool (f_2704 true) (f_2704 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_2730 b_2729 =
       (if b_2729 _|_
         (l1
           (let f_2732 b_2731 = (if b_2731 _|_ k_main_1201) in
            (let f_2734 b_2733 =
             (if b_2733 (f_2732 true)
               (let f_2740 b_2739 =
                (if b_2739 (f_2732 true)
                  (let f_2742 b_2741 = (if b_2741 (f_2732 false) (f_2732 true)) in (f_2742 b1_1314)))
                in (f_2740 b2_1314)))
             in
             (let f_2736 b_2735 =
              (if b_2735 (f_2734 true)
                (let f_2738 b_2737 = (if b_2737 (f_2734 false) (f_2734 true)) in (f_2738 b1_1314)))
              in (f_2736 b2_1314))))))
       in
       (let f_2744 b_2743 =
        (if b_2743 (f_2730 true) (let f_2746 b_2745 = (if b_2745 (f_2730 false) (f_2730 true)) in (f_2746 b1_1314))) in
        (f_2744 b2_1314))).

DONE!

(4-2) Checking HORS ... DONE!

Set wp_max_num to 5.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 5)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(5-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_2958 ->
         (rand_int
           (fun arg2_2961 ->
            (if (arg1_2958 > 0)
              (l0
                (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
                  (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
              (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_2939 ->
           (if (x_2939 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_2974 k_make_array_2975 -> (k_make_array_2975 (n_1019 - i_2974))) -1
          (fun m_2956 -> (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_2958 ->
        (rand_int
          (fun arg2_2961 ->
           (if (arg1_2958 > 0)
             (l0
               (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
                 (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
             (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))))): X
ETA: (fun arg1_2958 ->
      (rand_int
        (fun arg2_2961 ->
         (if (arg1_2958 > 0)
           (l0
             (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
               (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
           (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_2961 ->
        (if (arg1_2958 > 0)
          (l0
            (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
              (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
          (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))): unit
ETA: (fun arg2_2961 ->
      (if (arg1_2958 > 0)
        (l0
          (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
            (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
        (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false)))): (int ->
unit)
ETA: (if (arg1_2958 > 0)
       (l0
         (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
           (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))))
       (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false)): unit
ETA: (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
ETA: (fun main_2933 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2958: int
ETA: arg2_2961: int
ETA_AUX: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false): unit
ETA: (l0
       (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
         (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false)))): unit
ETA: (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)))
       (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false)): unit
ETA: (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
ETA: (fun main_2933 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2958: int
ETA: arg2_2961: int
ETA_AUX: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0))): unit
ETA: (k_main_1208 arg2_2961 arg1_2958 (fun main_2933 -> end) (arg2_2961 <= 0)): unit
ETA: (arg2_2961 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
ETA: (fun main_2933 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_2958: int
ETA: arg2_2961: int
ETA_AUX: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_2961:int) ->
            (if (arg1_2958 > 0)
              (l0
                (if (arg2_2961 >= 0)
                  (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                  (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_2958:int) ->
            (rand_int
              (fun (arg2_2961:int) ->
               (if (arg1_2958 > 0)
                 (l0
                   (if (arg2_2961 >= 0)
                     (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                     (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_2939 ->
          (if (x_2939 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_2939 ->
        (if (x_2939 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_2939 ->
      (if (x_2939 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_2939 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2991: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2991): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2992: int
ETA_AUX: (x_1117 x__2992): ((int -> X) ->
X)
ETA_AUX: x__2993: (int ->
X)
ETA_AUX: x__2994: int
ETA_AUX: (x__2993 x__2994): X
ETA_AUX: (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
           (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_2939 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__2995: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__2995): X
ETA: x_2939: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__2996: int
ETA_AUX: (x_1117 x__2996): ((int -> X) ->
X)
ETA_AUX: x__2997: (int ->
X)
ETA_AUX: x__2998: int
ETA_AUX: (x__2997 x__2998): X
ETA_AUX: (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
           (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_2939:int) ->
            (if (x_2939 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))))
                  x_2939 (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))))
                  m_1118 (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_2974 k_make_array_2975 -> (k_make_array_2975 (n_1019 - i_2974))) -1
         (fun m_2956 -> (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_2974 k_make_array_2975 -> (k_make_array_2975 (n_1019 - i_2974))) -1
       (fun m_2956 -> (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_2956 -> (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__2999: unit
ETA_AUX: (k_main_1201 x__2999): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_2974 k_make_array_2975 -> (k_make_array_2975 (n_1019 - i_2974))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_2975 -> (k_make_array_2975 (n_1019 - i_2974))): ((int -> X) ->
X)
ETA: (k_make_array_2975 (n_1019 - i_2974)): X
ETA: (n_1019 - i_2974): int
ETA_AUX: (k_make_array_2975 (n_1019 - i_2974)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
           (fun (m_2956:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_2958:int) ->
         (rand_int
           (fun (arg2_2961:int) ->
            (if (arg1_2958 > 0)
              (l0
                (if (arg2_2961 >= 0)
                  (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                  (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
              (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_2939:int) ->
           (if (x_2939 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))))
                 x_2939 (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))))
                 m_1118 (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
          (fun (m_2956:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_2958:int) ->
              (rand_int
                (fun (arg2_2961:int) ->
                 (if (arg1_2958 > 0)
                   (l0
                     (if (arg2_2961 >= 0)
                       (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                       (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_2958:int) ->
  (rand_int
    (fun (arg2_2961:int) ->
     (if (arg1_2958 > 0)
       (l0
         (if (arg2_2961 >= 0) (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
           (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
       (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_2958:int) ->
               (rand_int
                 (fun (arg2_2961:int) ->
                  (if (arg1_2958 > 0)
                    (l0
                      (if (arg2_2961 >= 0)
                        (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                        (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_2958:int) ->
                  (rand_int
                    (fun (arg2_2961:int) ->
                     (if (arg1_2958 > 0)
                       (l0
                         (if (arg2_2961 >= 0)
                           (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                           (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))))): X
abstract_term: (fun (arg1_2958:int) ->
                (rand_int
                  (fun (arg2_2961:int) ->
                   (if (arg1_2958 > 0)
                     (l0
                       (if (arg2_2961 >= 0)
                         (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                         (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_2958, int
abst_arg: arg1_2958, int
abstract_term: (rand_int
                 (fun (arg2_2961:int) ->
                  (if (arg1_2958 > 0)
                    (l0
                      (if (arg2_2961 >= 0)
                        (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                        (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))): X
abstract_term: (fun (arg2_2961:int) ->
                (if (arg1_2958 > 0)
                  (l0
                    (if (arg2_2961 >= 0)
                      (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                      (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_2961, int
abst_arg: arg2_2961, int
abstract_term: (if (arg1_2958 > 0)
                 (l0
                   (if (arg2_2961 >= 0)
                     (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                     (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))): X
abstract_term: (arg1_2958 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_2958 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_2961 >= 0)
                   (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                   (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)))): X
abstract_term: (if (arg2_2961 >= 0)
                 (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)))
                 (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false))): X
abstract_term: (arg2_2961 >= 0): x_1:bool[x_1]
cond: (arg1_2958 > 0); true
pbs: 
p:(arg2_2961 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0))): X
abstract_term: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) (arg2_2961 <= 0)): X
abstract_term: (arg2_2961 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
cond: (arg2_2961 >= 0); (arg1_2958 > 0); true
pbs: 
p:((not (arg2_2961 <= 0)) || ((1 <= ((-1 * arg2_2961) + arg1_2958)) && (arg2_2961 <= 0)))
tt:true
ff:false

cond: (arg2_2961 >= 0); (arg1_2958 > 0); true
pbs: 
p:(arg2_2961 <= 0)
tt:false
ff:false

abstract_term: (fun (main_2933:unit) -> end): (unit ->
X)
abst_arg: main_2933, unit
abst_arg: main_2933, unit
abstract_term: end: X
abstract_term: arg1_2958: int
abstract_term: arg2_2961: int
filter
cond: (arg2_2961 >= 0); (arg1_2958 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
cond: (not (arg2_2961 >= 0)); (arg1_2958 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2961) + arg1_2958)) && false))
tt:true
ff:false

cond: (not (arg2_2961 >= 0)); (arg1_2958 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2933:unit) -> end): (unit ->
X)
abst_arg: main_2933, unit
abst_arg: main_2933, unit
abstract_term: end: X
abstract_term: arg1_2958: int
abstract_term: arg2_2961: int
filter
cond: (not (arg2_2961 >= 0)); (arg1_2958 > 0); true
abstract_term: (l1 (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_2961 arg1_2958 (fun (main_2933:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_2961 + arg1_2958 && x_1; x_1]
cond: (not (arg1_2958 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_2961) + arg1_2958)) && false))
tt:true
ff:false

cond: (not (arg1_2958 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_2933:unit) -> end): (unit ->
X)
abst_arg: main_2933, unit
abst_arg: main_2933, unit
abstract_term: end: X
abstract_term: arg1_2958: int
abstract_term: arg2_2961: int
filter
cond: (not (arg1_2958 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_2939:int) ->
                     (if (x_2939 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2996:int) (x__2997:(int -> X)) ->
                            (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                           (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__2992:int) (x__2993:(int -> X)) ->
                            (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                           (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_2939:int) ->
    (if (x_2939 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
          (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
          (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_2939:int) ->
                      (if (x_2939 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2996:int) (x__2997:(int -> X)) ->
                             (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                            (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__2992:int) (x__2993:(int -> X)) ->
                             (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                            (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_2939:int) ->
                    (if (x_2939 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2996:int) (x__2997:(int -> X)) ->
                           (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                          (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__2992:int) (x__2993:(int -> X)) ->
                           (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                          (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_2939:int) ->
                  (if (x_2939 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2996:int) (x__2997:(int -> X)) ->
                         (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                        (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__2992:int) (x__2993:(int -> X)) ->
                         (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                        (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))))))): X
abstract_term: (fun (x_2939:int) ->
                (if (x_2939 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2996:int) (x__2997:(int -> X)) ->
                       (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                      (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__2992:int) (x__2993:(int -> X)) ->
                       (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                      (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)))))): (int ->
X)
abst_arg: x_2939, int
abst_arg: x_2939, int
abstract_term: (if (x_2939 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2996:int) (x__2997:(int -> X)) ->
                      (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))) x_2939
                     (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__2992:int) (x__2993:(int -> X)) ->
                      (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))) m_1118
                     (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))))): X
abstract_term: (x_2939 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_2939 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))))
                   x_2939 (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))))
                 x_2939 (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995))): X
abstract_term: (fun (x__2995:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2995)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2995, int[i_1116 + 1 >= x_1028]
abst_arg: x__2995, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2995): X
abstract_term: x__2995: int[i_1116 >= x_1028]
cond: (x_2939 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2995 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_2939 > m_1118); (not (i_1116 >= x_1028))
pbs: x__2995 := ((i_1116 + 1) >= x_1028)

abstract_term: x_2939: int
abstract_term: (fun (x__2996:int) (x__2997:(int -> X)) -> (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998)))): (
int -> (int -> X) -> X)
abst_arg: x__2996, int
abst_arg: x__2997, (int ->
X)
abst_arg: x__2996, int
abst_arg: x__2997, (int ->
X)
abstract_term: (x_1117 x__2996 (fun (x__2998:int) -> (x__2997 x__2998))): X
abstract_term: (fun (x__2998:int) -> (x__2997 x__2998)): (int ->
X)
abst_arg: x__2998, int
abst_arg: x__2998, int
abstract_term: (x__2997 x__2998): X
abstract_term: x__2998: int
filter
cond: (x_2939 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__2996: int
filter
cond: (x_2939 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_2939 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))))
                   m_1118 (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))))
                 m_1118 (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991))): X
abstract_term: (fun (x__2991:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__2991)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__2991, int[i_1116 + 1 >= x_1028]
abst_arg: x__2991, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__2991): X
abstract_term: x__2991: int[i_1116 >= x_1028]
cond: (not (x_2939 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2991 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_2939 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__2991 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__2992:int) (x__2993:(int -> X)) -> (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994)))): (
int -> (int -> X) -> X)
abst_arg: x__2992, int
abst_arg: x__2993, (int ->
X)
abst_arg: x__2992, int
abst_arg: x__2993, (int ->
X)
abstract_term: (x_1117 x__2992 (fun (x__2994:int) -> (x__2993 x__2994))): X
abstract_term: (fun (x__2994:int) -> (x__2993 x__2994)): (int ->
X)
abst_arg: x__2994, int
abst_arg: x__2994, int
abstract_term: (x__2993 x__2994): X
abstract_term: x__2994: int
filter
cond: (not (x_2939 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__2992: int
filter
cond: (not (x_2939 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_2939 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
                 (fun (m_2956:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
   (fun (m_2956:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_2956) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
                  (fun (m_2956:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
                   (fun (m_2956:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))) -1
                 (fun (m_2956:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))))))): X
abstract_term: (fun (m_2956:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_2956, int[i_1020 >= n_1019]
abst_arg: m_2956, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_2956) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))))): X
abstract_term: (n_1019 <= m_2956): x_1:bool[x_1]
cond: b_1314
pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_2956)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_2956); b_1314
pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999)))): X
abstract_term: (fail_1341 true (fun (x__2999:unit) -> (k_main_1201 x__2999))): X
abstract_term: (fun (x__2999:unit) -> (k_main_1201 x__2999)): (unit ->
X)
abst_arg: x__2999, unit
abst_arg: x__2999, unit
abstract_term: (k_main_1201 x__2999): X
abstract_term: x__2999: unit
filter
cond: (not (n_1019 <= m_2956)); b_1314
pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_2956)); b_1314
pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_2956)); b_1314
pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_2956 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_2974:int) (k_make_array_2975:(int -> X)) -> (k_make_array_2975 (n_1019 - i_2974))): (
int -> (int -> X) -> X)
abst_arg: i_2974, int
abst_arg: k_make_array_2975, (int ->
X)
abst_arg: i_2974, int
abst_arg: k_make_array_2975, (int ->
X)
abstract_term: (k_make_array_2975 (n_1019 - i_2974)): X
abstract_term: (n_1019 - i_2974): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__2995 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__2991 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_2975 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_2975))
              (fun m_2956 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_3002 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_3005 k_array_max_1145)))))).
  f_3002 k_array_max_3001 x__2995 -> (k_array_max_3001 false).
  f_3005 k_array_max_3004 x__2991 -> (k_array_max_3004 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_3008 b2_1314) (f_3012 k_main_1201 b2_1314))))).
  f_3008 b2_3007 k_make_array_2975 -> (if ((not b2_3007) || (not b2_3007)) _|_ k_make_array_2975).
  f_3012 k_main_3010 b2_3011 m_2956 ->
      (if rand_bool (l0 (if ((not b2_3011) || (not b2_3011)) _|_ k_main_3010))
        (l1
          (if ((not b2_3011) || (not b2_3011)) _|_
            (fail_1341 true (if ((not b2_3011) || (not b2_3011)) _|_ k_main_3010))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_3014 b_3013 =
       (if b_3013
         (l0
           (let f_3016 b_3015 =
            (if b_3015
              (l0 (let f_3018 b_3017 = (k_main_1208 () true b_3017) in (if rand_bool (f_3018 true) (f_3018 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3016 true) (f_3016 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3014 true) (f_3014 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3020 b_3019 =
           (if b_3019 (l0 (array_max_1011 x_1117 (f_3002 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3005 k_array_max_1145))))
           in (if rand_bool (f_3020 true) (f_3020 false))))).
  f_3002 k_array_max_3001 x__2995 -> (k_array_max_3001 false).
  f_3005 k_array_max_3004 x__2991 -> (k_array_max_3004 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3022 b_3021 =
       (if b_3021 _|_
         (l0
           (let f_3024 b_3023 = (if b_3023 _|_ (array_max_1011 (f_3008 b2_1314) (f_3012 k_main_1201 b2_1314))) in
            (let f_3026 b_3025 =
             (if b_3025 (f_3024 true)
               (let f_3030 b_3029 = (if b_3029 (f_3024 false) (f_3024 true)) in (f_3030 b2_1314)))
             in (let f_3028 b_3027 = (if b_3027 (f_3026 false) (f_3026 true)) in (f_3028 b2_1314))))))
       in (let f_3032 b_3031 = (if b_3031 (f_3022 false) (f_3022 true)) in (f_3032 b2_1314))).
  f_3008 b2_3007 k_make_array_2975 ->
      (let f_3034 b_3033 = (if b_3033 _|_ k_make_array_2975) in
       (let f_3036 b_3035 =
        (if b_3035 (f_3034 true) (let f_3040 b_3039 = (if b_3039 (f_3034 false) (f_3034 true)) in (f_3040 b2_3007))) in
        (let f_3038 b_3037 = (if b_3037 (f_3036 false) (f_3036 true)) in (f_3038 b2_3007)))).
  f_3012 k_main_3010 b2_3011 m_2956 ->
      (let f_3042 b_3041 =
       (if b_3041
         (l0
           (let f_3044 b_3043 = (if b_3043 _|_ k_main_3010) in
            (let f_3046 b_3045 =
             (if b_3045 (f_3044 true)
               (let f_3050 b_3049 = (if b_3049 (f_3044 false) (f_3044 true)) in (f_3050 b2_3011)))
             in (let f_3048 b_3047 = (if b_3047 (f_3046 false) (f_3046 true)) in (f_3048 b2_3011)))))
         (l1
           (let f_3052 b_3051 =
            (if b_3051 _|_
              (fail_1341 true
                (let f_3054 b_3053 = (if b_3053 _|_ k_main_3010) in
                 (let f_3056 b_3055 =
                  (if b_3055 (f_3054 true)
                    (let f_3060 b_3059 = (if b_3059 (f_3054 false) (f_3054 true)) in (f_3060 b2_3011)))
                  in (let f_3058 b_3057 = (if b_3057 (f_3056 false) (f_3056 true)) in (f_3058 b2_3011))))))
            in
            (let f_3062 b_3061 =
             (if b_3061 (f_3052 true)
               (let f_3066 b_3065 = (if b_3065 (f_3052 false) (f_3052 true)) in (f_3066 b2_3011)))
             in (let f_3064 b_3063 = (if b_3063 (f_3062 false) (f_3062 true)) in (f_3064 b2_3011))))))
       in (if rand_bool (f_3042 true) (f_3042 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3068 b_3067 =
       (if b_3067 _|_
         (l1
           (let f_3070 b_3069 = (if b_3069 _|_ k_main_1201) in
            (let f_3072 b_3071 =
             (if b_3071 (f_3070 true)
               (let f_3078 b_3077 =
                (if b_3077 (f_3070 true)
                  (let f_3080 b_3079 = (if b_3079 (f_3070 false) (f_3070 true)) in (f_3080 b1_1314)))
                in (f_3078 b2_1314)))
             in
             (let f_3074 b_3073 =
              (if b_3073 (f_3072 true)
                (let f_3076 b_3075 = (if b_3075 (f_3072 false) (f_3072 true)) in (f_3076 b1_1314)))
              in (f_3074 b2_1314))))))
       in
       (let f_3082 b_3081 =
        (if b_3081 (f_3068 true) (let f_3084 b_3083 = (if b_3083 (f_3068 false) (f_3068 true)) in (f_3084 b1_1314))) in
        (f_3082 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_3014 b_3013 =
       (if b_3013
         (l0
           (let f_3016 b_3015 =
            (if b_3015
              (l0 (let f_3018 b_3017 = (k_main_1208 () true b_3017) in (if rand_bool (f_3018 true) (f_3018 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3016 true) (f_3016 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3014 true) (f_3014 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3020 b_3019 =
           (if b_3019 (l0 (array_max_1011 x_1117 (f_3002 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3005 k_array_max_1145))))
           in (if rand_bool (f_3020 true) (f_3020 false))))).
  f_3002 k_array_max_3001 x__2995 -> (k_array_max_3001 false).
  f_3005 k_array_max_3004 x__2991 -> (k_array_max_3004 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3022 b_3021 =
       (if b_3021 _|_
         (l0
           (let f_3024 b_3023 = (if b_3023 _|_ (array_max_1011 (f_3008 b2_1314) (f_3012 k_main_1201 b2_1314))) in
            (let f_3026 b_3025 =
             (if b_3025 (f_3024 true)
               (let f_3030 b_3029 = (if b_3029 (f_3024 false) (f_3024 true)) in (f_3030 b2_1314)))
             in (let f_3028 b_3027 = (if b_3027 (f_3026 false) (f_3026 true)) in (f_3028 b2_1314))))))
       in (let f_3032 b_3031 = (if b_3031 (f_3022 false) (f_3022 true)) in (f_3032 b2_1314))).
  f_3008 b2_3007 k_make_array_2975 ->
      (let f_3034 b_3033 = (if b_3033 _|_ k_make_array_2975) in
       (let f_3036 b_3035 =
        (if b_3035 (f_3034 true) (let f_3040 b_3039 = (if b_3039 (f_3034 false) (f_3034 true)) in (f_3040 b2_3007))) in
        (let f_3038 b_3037 = (if b_3037 (f_3036 false) (f_3036 true)) in (f_3038 b2_3007)))).
  f_3012 k_main_3010 b2_3011 m_2956 ->
      (let f_3042 b_3041 =
       (if b_3041
         (l0
           (let f_3044 b_3043 = (if b_3043 _|_ k_main_3010) in
            (let f_3046 b_3045 =
             (if b_3045 (f_3044 true)
               (let f_3050 b_3049 = (if b_3049 (f_3044 false) (f_3044 true)) in (f_3050 b2_3011)))
             in (let f_3048 b_3047 = (if b_3047 (f_3046 false) (f_3046 true)) in (f_3048 b2_3011)))))
         (l1
           (let f_3052 b_3051 =
            (if b_3051 _|_
              (fail_1341 true
                (let f_3054 b_3053 = (if b_3053 _|_ k_main_3010) in
                 (let f_3056 b_3055 =
                  (if b_3055 (f_3054 true)
                    (let f_3060 b_3059 = (if b_3059 (f_3054 false) (f_3054 true)) in (f_3060 b2_3011)))
                  in (let f_3058 b_3057 = (if b_3057 (f_3056 false) (f_3056 true)) in (f_3058 b2_3011))))))
            in
            (let f_3062 b_3061 =
             (if b_3061 (f_3052 true)
               (let f_3066 b_3065 = (if b_3065 (f_3052 false) (f_3052 true)) in (f_3066 b2_3011)))
             in (let f_3064 b_3063 = (if b_3063 (f_3062 false) (f_3062 true)) in (f_3064 b2_3011))))))
       in (if rand_bool (f_3042 true) (f_3042 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3068 b_3067 =
       (if b_3067 _|_
         (l1
           (let f_3070 b_3069 = (if b_3069 _|_ k_main_1201) in
            (let f_3072 b_3071 =
             (if b_3071 (f_3070 true)
               (let f_3078 b_3077 =
                (if b_3077 (f_3070 true)
                  (let f_3080 b_3079 = (if b_3079 (f_3070 false) (f_3070 true)) in (f_3080 b1_1314)))
                in (f_3078 b2_1314)))
             in
             (let f_3074 b_3073 =
              (if b_3073 (f_3072 true)
                (let f_3076 b_3075 = (if b_3075 (f_3072 false) (f_3072 true)) in (f_3076 b1_1314)))
              in (f_3074 b2_1314))))))
       in
       (let f_3082 b_3081 =
        (if b_3081 (f_3068 true) (let f_3084 b_3083 = (if b_3083 (f_3068 false) (f_3068 true)) in (f_3084 b1_1314))) in
        (f_3082 b2_1314))).

DONE!

(5-2) Checking HORS ... DONE!

Set wp_max_num to 6.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 6)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(6-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_3296 ->
         (rand_int
           (fun arg2_3299 ->
            (if (arg1_3296 > 0)
              (l0
                (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
                  (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
              (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_3277 ->
           (if (x_3277 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_3312 k_make_array_3313 -> (k_make_array_3313 (n_1019 - i_3312))) -1
          (fun m_3294 -> (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_3296 ->
        (rand_int
          (fun arg2_3299 ->
           (if (arg1_3296 > 0)
             (l0
               (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
                 (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
             (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))))): X
ETA: (fun arg1_3296 ->
      (rand_int
        (fun arg2_3299 ->
         (if (arg1_3296 > 0)
           (l0
             (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
               (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
           (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_3299 ->
        (if (arg1_3296 > 0)
          (l0
            (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
              (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
          (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))): unit
ETA: (fun arg2_3299 ->
      (if (arg1_3296 > 0)
        (l0
          (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
            (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
        (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false)))): (int ->
unit)
ETA: (if (arg1_3296 > 0)
       (l0
         (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
           (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))))
       (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false)): unit
ETA: (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
ETA: (fun main_3271 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3296: int
ETA: arg2_3299: int
ETA_AUX: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false): unit
ETA: (l0
       (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
         (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false)))): unit
ETA: (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)))
       (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false)): unit
ETA: (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
ETA: (fun main_3271 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3296: int
ETA: arg2_3299: int
ETA_AUX: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0))): unit
ETA: (k_main_1208 arg2_3299 arg1_3296 (fun main_3271 -> end) (arg2_3299 <= 0)): unit
ETA: (arg2_3299 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
ETA: (fun main_3271 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3296: int
ETA: arg2_3299: int
ETA_AUX: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_3299:int) ->
            (if (arg1_3296 > 0)
              (l0
                (if (arg2_3299 >= 0)
                  (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                  (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_3296:int) ->
            (rand_int
              (fun (arg2_3299:int) ->
               (if (arg1_3296 > 0)
                 (l0
                   (if (arg2_3299 >= 0)
                     (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                     (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_3277 ->
          (if (x_3277 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_3277 ->
        (if (x_3277 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_3277 ->
      (if (x_3277 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_3277 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__3329: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__3329): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__3330: int
ETA_AUX: (x_1117 x__3330): ((int -> X) ->
X)
ETA_AUX: x__3331: (int ->
X)
ETA_AUX: x__3332: int
ETA_AUX: (x__3331 x__3332): X
ETA_AUX: (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
           (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3277 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__3333: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__3333): X
ETA: x_3277: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__3334: int
ETA_AUX: (x_1117 x__3334): ((int -> X) ->
X)
ETA_AUX: x__3335: (int ->
X)
ETA_AUX: x__3336: int
ETA_AUX: (x__3335 x__3336): X
ETA_AUX: (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
           (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_3277:int) ->
            (if (x_3277 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))))
                  x_3277 (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))))
                  m_1118 (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_3312 k_make_array_3313 -> (k_make_array_3313 (n_1019 - i_3312))) -1
         (fun m_3294 -> (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_3312 k_make_array_3313 -> (k_make_array_3313 (n_1019 - i_3312))) -1
       (fun m_3294 -> (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_3294 -> (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__3337: unit
ETA_AUX: (k_main_1201 x__3337): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_3312 k_make_array_3313 -> (k_make_array_3313 (n_1019 - i_3312))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_3313 -> (k_make_array_3313 (n_1019 - i_3312))): ((int -> X) ->
X)
ETA: (k_make_array_3313 (n_1019 - i_3312)): X
ETA: (n_1019 - i_3312): int
ETA_AUX: (k_make_array_3313 (n_1019 - i_3312)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
           (fun (m_3294:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_3296:int) ->
         (rand_int
           (fun (arg2_3299:int) ->
            (if (arg1_3296 > 0)
              (l0
                (if (arg2_3299 >= 0)
                  (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                  (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_3277:int) ->
           (if (x_3277 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))))
                 x_3277 (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))))
                 m_1118 (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
          (fun (m_3294:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_3296:int) ->
              (rand_int
                (fun (arg2_3299:int) ->
                 (if (arg1_3296 > 0)
                   (l0
                     (if (arg2_3299 >= 0)
                       (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                       (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_3296:int) ->
  (rand_int
    (fun (arg2_3299:int) ->
     (if (arg1_3296 > 0)
       (l0
         (if (arg2_3299 >= 0) (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
           (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
       (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_3296:int) ->
               (rand_int
                 (fun (arg2_3299:int) ->
                  (if (arg1_3296 > 0)
                    (l0
                      (if (arg2_3299 >= 0)
                        (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                        (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_3296:int) ->
                  (rand_int
                    (fun (arg2_3299:int) ->
                     (if (arg1_3296 > 0)
                       (l0
                         (if (arg2_3299 >= 0)
                           (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                           (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))))): X
abstract_term: (fun (arg1_3296:int) ->
                (rand_int
                  (fun (arg2_3299:int) ->
                   (if (arg1_3296 > 0)
                     (l0
                       (if (arg2_3299 >= 0)
                         (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                         (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_3296, int
abst_arg: arg1_3296, int
abstract_term: (rand_int
                 (fun (arg2_3299:int) ->
                  (if (arg1_3296 > 0)
                    (l0
                      (if (arg2_3299 >= 0)
                        (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                        (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))): X
abstract_term: (fun (arg2_3299:int) ->
                (if (arg1_3296 > 0)
                  (l0
                    (if (arg2_3299 >= 0)
                      (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                      (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_3299, int
abst_arg: arg2_3299, int
abstract_term: (if (arg1_3296 > 0)
                 (l0
                   (if (arg2_3299 >= 0)
                     (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                     (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))): X
abstract_term: (arg1_3296 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_3296 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_3299 >= 0)
                   (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                   (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)))): X
abstract_term: (if (arg2_3299 >= 0)
                 (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)))
                 (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false))): X
abstract_term: (arg2_3299 >= 0): x_1:bool[x_1]
cond: (arg1_3296 > 0); true
pbs: 
p:(arg2_3299 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0))): X
abstract_term: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) (arg2_3299 <= 0)): X
abstract_term: (arg2_3299 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
cond: (arg2_3299 >= 0); (arg1_3296 > 0); true
pbs: 
p:((not (arg2_3299 <= 0)) || ((1 <= ((-1 * arg2_3299) + arg1_3296)) && (arg2_3299 <= 0)))
tt:true
ff:false

cond: (arg2_3299 >= 0); (arg1_3296 > 0); true
pbs: 
p:(arg2_3299 <= 0)
tt:false
ff:false

abstract_term: (fun (main_3271:unit) -> end): (unit ->
X)
abst_arg: main_3271, unit
abst_arg: main_3271, unit
abstract_term: end: X
abstract_term: arg1_3296: int
abstract_term: arg2_3299: int
filter
cond: (arg2_3299 >= 0); (arg1_3296 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
cond: (not (arg2_3299 >= 0)); (arg1_3296 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3299) + arg1_3296)) && false))
tt:true
ff:false

cond: (not (arg2_3299 >= 0)); (arg1_3296 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3271:unit) -> end): (unit ->
X)
abst_arg: main_3271, unit
abst_arg: main_3271, unit
abstract_term: end: X
abstract_term: arg1_3296: int
abstract_term: arg2_3299: int
filter
cond: (not (arg2_3299 >= 0)); (arg1_3296 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3299 arg1_3296 (fun (main_3271:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3299 + arg1_3296 && x_1; x_1]
cond: (not (arg1_3296 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3299) + arg1_3296)) && false))
tt:true
ff:false

cond: (not (arg1_3296 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3271:unit) -> end): (unit ->
X)
abst_arg: main_3271, unit
abst_arg: main_3271, unit
abstract_term: end: X
abstract_term: arg1_3296: int
abstract_term: arg2_3299: int
filter
cond: (not (arg1_3296 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_3277:int) ->
                     (if (x_3277 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__3334:int) (x__3335:(int -> X)) ->
                            (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                           (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__3330:int) (x__3331:(int -> X)) ->
                            (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                           (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_3277:int) ->
    (if (x_3277 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
          (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
          (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_3277:int) ->
                      (if (x_3277 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__3334:int) (x__3335:(int -> X)) ->
                             (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                            (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__3330:int) (x__3331:(int -> X)) ->
                             (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                            (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_3277:int) ->
                    (if (x_3277 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__3334:int) (x__3335:(int -> X)) ->
                           (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                          (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__3330:int) (x__3331:(int -> X)) ->
                           (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                          (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_3277:int) ->
                  (if (x_3277 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__3334:int) (x__3335:(int -> X)) ->
                         (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                        (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__3330:int) (x__3331:(int -> X)) ->
                         (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                        (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))))))): X
abstract_term: (fun (x_3277:int) ->
                (if (x_3277 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__3334:int) (x__3335:(int -> X)) ->
                       (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                      (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__3330:int) (x__3331:(int -> X)) ->
                       (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                      (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)))))): (int ->
X)
abst_arg: x_3277, int
abst_arg: x_3277, int
abstract_term: (if (x_3277 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__3334:int) (x__3335:(int -> X)) ->
                      (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))) x_3277
                     (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__3330:int) (x__3331:(int -> X)) ->
                      (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))) m_1118
                     (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))))): X
abstract_term: (x_3277 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_3277 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))))
                   x_3277 (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))))
                 x_3277 (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333))): X
abstract_term: (fun (x__3333:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3333)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__3333, int[i_1116 + 1 >= x_1028]
abst_arg: x__3333, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__3333): X
abstract_term: x__3333: int[i_1116 >= x_1028]
cond: (x_3277 > m_1118); (not (i_1116 >= x_1028))
pbs: x__3333 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_3277 > m_1118); (not (i_1116 >= x_1028))
pbs: x__3333 := ((i_1116 + 1) >= x_1028)

abstract_term: x_3277: int
abstract_term: (fun (x__3334:int) (x__3335:(int -> X)) -> (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336)))): (
int -> (int -> X) -> X)
abst_arg: x__3334, int
abst_arg: x__3335, (int ->
X)
abst_arg: x__3334, int
abst_arg: x__3335, (int ->
X)
abstract_term: (x_1117 x__3334 (fun (x__3336:int) -> (x__3335 x__3336))): X
abstract_term: (fun (x__3336:int) -> (x__3335 x__3336)): (int ->
X)
abst_arg: x__3336, int
abst_arg: x__3336, int
abstract_term: (x__3335 x__3336): X
abstract_term: x__3336: int
filter
cond: (x_3277 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__3334: int
filter
cond: (x_3277 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_3277 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))))
                   m_1118 (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))))
                 m_1118 (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329))): X
abstract_term: (fun (x__3329:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3329)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__3329, int[i_1116 + 1 >= x_1028]
abst_arg: x__3329, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__3329): X
abstract_term: x__3329: int[i_1116 >= x_1028]
cond: (not (x_3277 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__3329 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_3277 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__3329 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__3330:int) (x__3331:(int -> X)) -> (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332)))): (
int -> (int -> X) -> X)
abst_arg: x__3330, int
abst_arg: x__3331, (int ->
X)
abst_arg: x__3330, int
abst_arg: x__3331, (int ->
X)
abstract_term: (x_1117 x__3330 (fun (x__3332:int) -> (x__3331 x__3332))): X
abstract_term: (fun (x__3332:int) -> (x__3331 x__3332)): (int ->
X)
abst_arg: x__3332, int
abst_arg: x__3332, int
abstract_term: (x__3331 x__3332): X
abstract_term: x__3332: int
filter
cond: (not (x_3277 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__3330: int
filter
cond: (not (x_3277 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_3277 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
                 (fun (m_3294:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
   (fun (m_3294:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_3294) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
                  (fun (m_3294:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
                   (fun (m_3294:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))) -1
                 (fun (m_3294:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))))))): X
abstract_term: (fun (m_3294:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_3294, int[i_1020 >= n_1019]
abst_arg: m_3294, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_3294) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))))): X
abstract_term: (n_1019 <= m_3294): x_1:bool[x_1]
cond: b_1314
pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_3294)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_3294); b_1314
pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337)))): X
abstract_term: (fail_1341 true (fun (x__3337:unit) -> (k_main_1201 x__3337))): X
abstract_term: (fun (x__3337:unit) -> (k_main_1201 x__3337)): (unit ->
X)
abst_arg: x__3337, unit
abst_arg: x__3337, unit
abstract_term: (k_main_1201 x__3337): X
abstract_term: x__3337: unit
filter
cond: (not (n_1019 <= m_3294)); b_1314
pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_3294)); b_1314
pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_3294)); b_1314
pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3294 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_3312:int) (k_make_array_3313:(int -> X)) -> (k_make_array_3313 (n_1019 - i_3312))): (
int -> (int -> X) -> X)
abst_arg: i_3312, int
abst_arg: k_make_array_3313, (int ->
X)
abst_arg: i_3312, int
abst_arg: k_make_array_3313, (int ->
X)
abstract_term: (k_make_array_3313 (n_1019 - i_3312)): X
abstract_term: (n_1019 - i_3312): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__3333 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__3329 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_3313 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_3313))
              (fun m_3294 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_3340 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_3343 k_array_max_1145)))))).
  f_3340 k_array_max_3339 x__3333 -> (k_array_max_3339 false).
  f_3343 k_array_max_3342 x__3329 -> (k_array_max_3342 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_3346 b2_1314) (f_3350 k_main_1201 b2_1314))))).
  f_3346 b2_3345 k_make_array_3313 -> (if ((not b2_3345) || (not b2_3345)) _|_ k_make_array_3313).
  f_3350 k_main_3348 b2_3349 m_3294 ->
      (if rand_bool (l0 (if ((not b2_3349) || (not b2_3349)) _|_ k_main_3348))
        (l1
          (if ((not b2_3349) || (not b2_3349)) _|_
            (fail_1341 true (if ((not b2_3349) || (not b2_3349)) _|_ k_main_3348))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_3352 b_3351 =
       (if b_3351
         (l0
           (let f_3354 b_3353 =
            (if b_3353
              (l0 (let f_3356 b_3355 = (k_main_1208 () true b_3355) in (if rand_bool (f_3356 true) (f_3356 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3354 true) (f_3354 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3352 true) (f_3352 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3358 b_3357 =
           (if b_3357 (l0 (array_max_1011 x_1117 (f_3340 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3343 k_array_max_1145))))
           in (if rand_bool (f_3358 true) (f_3358 false))))).
  f_3340 k_array_max_3339 x__3333 -> (k_array_max_3339 false).
  f_3343 k_array_max_3342 x__3329 -> (k_array_max_3342 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3360 b_3359 =
       (if b_3359 _|_
         (l0
           (let f_3362 b_3361 = (if b_3361 _|_ (array_max_1011 (f_3346 b2_1314) (f_3350 k_main_1201 b2_1314))) in
            (let f_3364 b_3363 =
             (if b_3363 (f_3362 true)
               (let f_3368 b_3367 = (if b_3367 (f_3362 false) (f_3362 true)) in (f_3368 b2_1314)))
             in (let f_3366 b_3365 = (if b_3365 (f_3364 false) (f_3364 true)) in (f_3366 b2_1314))))))
       in (let f_3370 b_3369 = (if b_3369 (f_3360 false) (f_3360 true)) in (f_3370 b2_1314))).
  f_3346 b2_3345 k_make_array_3313 ->
      (let f_3372 b_3371 = (if b_3371 _|_ k_make_array_3313) in
       (let f_3374 b_3373 =
        (if b_3373 (f_3372 true) (let f_3378 b_3377 = (if b_3377 (f_3372 false) (f_3372 true)) in (f_3378 b2_3345))) in
        (let f_3376 b_3375 = (if b_3375 (f_3374 false) (f_3374 true)) in (f_3376 b2_3345)))).
  f_3350 k_main_3348 b2_3349 m_3294 ->
      (let f_3380 b_3379 =
       (if b_3379
         (l0
           (let f_3382 b_3381 = (if b_3381 _|_ k_main_3348) in
            (let f_3384 b_3383 =
             (if b_3383 (f_3382 true)
               (let f_3388 b_3387 = (if b_3387 (f_3382 false) (f_3382 true)) in (f_3388 b2_3349)))
             in (let f_3386 b_3385 = (if b_3385 (f_3384 false) (f_3384 true)) in (f_3386 b2_3349)))))
         (l1
           (let f_3390 b_3389 =
            (if b_3389 _|_
              (fail_1341 true
                (let f_3392 b_3391 = (if b_3391 _|_ k_main_3348) in
                 (let f_3394 b_3393 =
                  (if b_3393 (f_3392 true)
                    (let f_3398 b_3397 = (if b_3397 (f_3392 false) (f_3392 true)) in (f_3398 b2_3349)))
                  in (let f_3396 b_3395 = (if b_3395 (f_3394 false) (f_3394 true)) in (f_3396 b2_3349))))))
            in
            (let f_3400 b_3399 =
             (if b_3399 (f_3390 true)
               (let f_3404 b_3403 = (if b_3403 (f_3390 false) (f_3390 true)) in (f_3404 b2_3349)))
             in (let f_3402 b_3401 = (if b_3401 (f_3400 false) (f_3400 true)) in (f_3402 b2_3349))))))
       in (if rand_bool (f_3380 true) (f_3380 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3406 b_3405 =
       (if b_3405 _|_
         (l1
           (let f_3408 b_3407 = (if b_3407 _|_ k_main_1201) in
            (let f_3410 b_3409 =
             (if b_3409 (f_3408 true)
               (let f_3416 b_3415 =
                (if b_3415 (f_3408 true)
                  (let f_3418 b_3417 = (if b_3417 (f_3408 false) (f_3408 true)) in (f_3418 b1_1314)))
                in (f_3416 b2_1314)))
             in
             (let f_3412 b_3411 =
              (if b_3411 (f_3410 true)
                (let f_3414 b_3413 = (if b_3413 (f_3410 false) (f_3410 true)) in (f_3414 b1_1314)))
              in (f_3412 b2_1314))))))
       in
       (let f_3420 b_3419 =
        (if b_3419 (f_3406 true) (let f_3422 b_3421 = (if b_3421 (f_3406 false) (f_3406 true)) in (f_3422 b1_1314))) in
        (f_3420 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_3352 b_3351 =
       (if b_3351
         (l0
           (let f_3354 b_3353 =
            (if b_3353
              (l0 (let f_3356 b_3355 = (k_main_1208 () true b_3355) in (if rand_bool (f_3356 true) (f_3356 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3354 true) (f_3354 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3352 true) (f_3352 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3358 b_3357 =
           (if b_3357 (l0 (array_max_1011 x_1117 (f_3340 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3343 k_array_max_1145))))
           in (if rand_bool (f_3358 true) (f_3358 false))))).
  f_3340 k_array_max_3339 x__3333 -> (k_array_max_3339 false).
  f_3343 k_array_max_3342 x__3329 -> (k_array_max_3342 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3360 b_3359 =
       (if b_3359 _|_
         (l0
           (let f_3362 b_3361 = (if b_3361 _|_ (array_max_1011 (f_3346 b2_1314) (f_3350 k_main_1201 b2_1314))) in
            (let f_3364 b_3363 =
             (if b_3363 (f_3362 true)
               (let f_3368 b_3367 = (if b_3367 (f_3362 false) (f_3362 true)) in (f_3368 b2_1314)))
             in (let f_3366 b_3365 = (if b_3365 (f_3364 false) (f_3364 true)) in (f_3366 b2_1314))))))
       in (let f_3370 b_3369 = (if b_3369 (f_3360 false) (f_3360 true)) in (f_3370 b2_1314))).
  f_3346 b2_3345 k_make_array_3313 ->
      (let f_3372 b_3371 = (if b_3371 _|_ k_make_array_3313) in
       (let f_3374 b_3373 =
        (if b_3373 (f_3372 true) (let f_3378 b_3377 = (if b_3377 (f_3372 false) (f_3372 true)) in (f_3378 b2_3345))) in
        (let f_3376 b_3375 = (if b_3375 (f_3374 false) (f_3374 true)) in (f_3376 b2_3345)))).
  f_3350 k_main_3348 b2_3349 m_3294 ->
      (let f_3380 b_3379 =
       (if b_3379
         (l0
           (let f_3382 b_3381 = (if b_3381 _|_ k_main_3348) in
            (let f_3384 b_3383 =
             (if b_3383 (f_3382 true)
               (let f_3388 b_3387 = (if b_3387 (f_3382 false) (f_3382 true)) in (f_3388 b2_3349)))
             in (let f_3386 b_3385 = (if b_3385 (f_3384 false) (f_3384 true)) in (f_3386 b2_3349)))))
         (l1
           (let f_3390 b_3389 =
            (if b_3389 _|_
              (fail_1341 true
                (let f_3392 b_3391 = (if b_3391 _|_ k_main_3348) in
                 (let f_3394 b_3393 =
                  (if b_3393 (f_3392 true)
                    (let f_3398 b_3397 = (if b_3397 (f_3392 false) (f_3392 true)) in (f_3398 b2_3349)))
                  in (let f_3396 b_3395 = (if b_3395 (f_3394 false) (f_3394 true)) in (f_3396 b2_3349))))))
            in
            (let f_3400 b_3399 =
             (if b_3399 (f_3390 true)
               (let f_3404 b_3403 = (if b_3403 (f_3390 false) (f_3390 true)) in (f_3404 b2_3349)))
             in (let f_3402 b_3401 = (if b_3401 (f_3400 false) (f_3400 true)) in (f_3402 b2_3349))))))
       in (if rand_bool (f_3380 true) (f_3380 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3406 b_3405 =
       (if b_3405 _|_
         (l1
           (let f_3408 b_3407 = (if b_3407 _|_ k_main_1201) in
            (let f_3410 b_3409 =
             (if b_3409 (f_3408 true)
               (let f_3416 b_3415 =
                (if b_3415 (f_3408 true)
                  (let f_3418 b_3417 = (if b_3417 (f_3408 false) (f_3408 true)) in (f_3418 b1_1314)))
                in (f_3416 b2_1314)))
             in
             (let f_3412 b_3411 =
              (if b_3411 (f_3410 true)
                (let f_3414 b_3413 = (if b_3413 (f_3410 false) (f_3410 true)) in (f_3414 b1_1314)))
              in (f_3412 b2_1314))))))
       in
       (let f_3420 b_3419 =
        (if b_3419 (f_3406 true) (let f_3422 b_3421 = (if b_3421 (f_3406 false) (f_3406 true)) in (f_3422 b1_1314))) in
        (f_3420 b2_1314))).

DONE!

(6-2) Checking HORS ... DONE!

Set wp_max_num to 7.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 7)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(7-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_3634 ->
         (rand_int
           (fun arg2_3637 ->
            (if (arg1_3634 > 0)
              (l0
                (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
                  (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
              (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_3615 ->
           (if (x_3615 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_3650 k_make_array_3651 -> (k_make_array_3651 (n_1019 - i_3650))) -1
          (fun m_3632 -> (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_3634 ->
        (rand_int
          (fun arg2_3637 ->
           (if (arg1_3634 > 0)
             (l0
               (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
                 (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
             (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))))): X
ETA: (fun arg1_3634 ->
      (rand_int
        (fun arg2_3637 ->
         (if (arg1_3634 > 0)
           (l0
             (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
               (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
           (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_3637 ->
        (if (arg1_3634 > 0)
          (l0
            (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
              (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
          (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))): unit
ETA: (fun arg2_3637 ->
      (if (arg1_3634 > 0)
        (l0
          (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
            (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
        (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false)))): (int ->
unit)
ETA: (if (arg1_3634 > 0)
       (l0
         (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
           (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))))
       (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false)): unit
ETA: (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
ETA: (fun main_3609 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3634: int
ETA: arg2_3637: int
ETA_AUX: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false): unit
ETA: (l0
       (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
         (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false)))): unit
ETA: (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)))
       (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false)): unit
ETA: (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
ETA: (fun main_3609 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3634: int
ETA: arg2_3637: int
ETA_AUX: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0))): unit
ETA: (k_main_1208 arg2_3637 arg1_3634 (fun main_3609 -> end) (arg2_3637 <= 0)): unit
ETA: (arg2_3637 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
ETA: (fun main_3609 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3634: int
ETA: arg2_3637: int
ETA_AUX: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_3637:int) ->
            (if (arg1_3634 > 0)
              (l0
                (if (arg2_3637 >= 0)
                  (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                  (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_3634:int) ->
            (rand_int
              (fun (arg2_3637:int) ->
               (if (arg1_3634 > 0)
                 (l0
                   (if (arg2_3637 >= 0)
                     (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                     (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_3615 ->
          (if (x_3615 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_3615 ->
        (if (x_3615 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_3615 ->
      (if (x_3615 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_3615 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__3667: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__3667): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__3668: int
ETA_AUX: (x_1117 x__3668): ((int -> X) ->
X)
ETA_AUX: x__3669: (int ->
X)
ETA_AUX: x__3670: int
ETA_AUX: (x__3669 x__3670): X
ETA_AUX: (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
           (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3615 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__3671: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__3671): X
ETA: x_3615: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__3672: int
ETA_AUX: (x_1117 x__3672): ((int -> X) ->
X)
ETA_AUX: x__3673: (int ->
X)
ETA_AUX: x__3674: int
ETA_AUX: (x__3673 x__3674): X
ETA_AUX: (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
           (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_3615:int) ->
            (if (x_3615 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))))
                  x_3615 (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))))
                  m_1118 (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_3650 k_make_array_3651 -> (k_make_array_3651 (n_1019 - i_3650))) -1
         (fun m_3632 -> (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_3650 k_make_array_3651 -> (k_make_array_3651 (n_1019 - i_3650))) -1
       (fun m_3632 -> (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_3632 -> (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__3675: unit
ETA_AUX: (k_main_1201 x__3675): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_3650 k_make_array_3651 -> (k_make_array_3651 (n_1019 - i_3650))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_3651 -> (k_make_array_3651 (n_1019 - i_3650))): ((int -> X) ->
X)
ETA: (k_make_array_3651 (n_1019 - i_3650)): X
ETA: (n_1019 - i_3650): int
ETA_AUX: (k_make_array_3651 (n_1019 - i_3650)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
           (fun (m_3632:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_3634:int) ->
         (rand_int
           (fun (arg2_3637:int) ->
            (if (arg1_3634 > 0)
              (l0
                (if (arg2_3637 >= 0)
                  (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                  (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_3615:int) ->
           (if (x_3615 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))))
                 x_3615 (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))))
                 m_1118 (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
          (fun (m_3632:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_3634:int) ->
              (rand_int
                (fun (arg2_3637:int) ->
                 (if (arg1_3634 > 0)
                   (l0
                     (if (arg2_3637 >= 0)
                       (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                       (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_3634:int) ->
  (rand_int
    (fun (arg2_3637:int) ->
     (if (arg1_3634 > 0)
       (l0
         (if (arg2_3637 >= 0) (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
           (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
       (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_3634:int) ->
               (rand_int
                 (fun (arg2_3637:int) ->
                  (if (arg1_3634 > 0)
                    (l0
                      (if (arg2_3637 >= 0)
                        (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                        (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_3634:int) ->
                  (rand_int
                    (fun (arg2_3637:int) ->
                     (if (arg1_3634 > 0)
                       (l0
                         (if (arg2_3637 >= 0)
                           (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                           (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))))): X
abstract_term: (fun (arg1_3634:int) ->
                (rand_int
                  (fun (arg2_3637:int) ->
                   (if (arg1_3634 > 0)
                     (l0
                       (if (arg2_3637 >= 0)
                         (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                         (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_3634, int
abst_arg: arg1_3634, int
abstract_term: (rand_int
                 (fun (arg2_3637:int) ->
                  (if (arg1_3634 > 0)
                    (l0
                      (if (arg2_3637 >= 0)
                        (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                        (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))): X
abstract_term: (fun (arg2_3637:int) ->
                (if (arg1_3634 > 0)
                  (l0
                    (if (arg2_3637 >= 0)
                      (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                      (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_3637, int
abst_arg: arg2_3637, int
abstract_term: (if (arg1_3634 > 0)
                 (l0
                   (if (arg2_3637 >= 0)
                     (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                     (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))): X
abstract_term: (arg1_3634 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_3634 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_3637 >= 0)
                   (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                   (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)))): X
abstract_term: (if (arg2_3637 >= 0)
                 (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)))
                 (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false))): X
abstract_term: (arg2_3637 >= 0): x_1:bool[x_1]
cond: (arg1_3634 > 0); true
pbs: 
p:(arg2_3637 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0))): X
abstract_term: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) (arg2_3637 <= 0)): X
abstract_term: (arg2_3637 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
cond: (arg2_3637 >= 0); (arg1_3634 > 0); true
pbs: 
p:((not (arg2_3637 <= 0)) || ((1 <= ((-1 * arg2_3637) + arg1_3634)) && (arg2_3637 <= 0)))
tt:true
ff:false

cond: (arg2_3637 >= 0); (arg1_3634 > 0); true
pbs: 
p:(arg2_3637 <= 0)
tt:false
ff:false

abstract_term: (fun (main_3609:unit) -> end): (unit ->
X)
abst_arg: main_3609, unit
abst_arg: main_3609, unit
abstract_term: end: X
abstract_term: arg1_3634: int
abstract_term: arg2_3637: int
filter
cond: (arg2_3637 >= 0); (arg1_3634 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
cond: (not (arg2_3637 >= 0)); (arg1_3634 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3637) + arg1_3634)) && false))
tt:true
ff:false

cond: (not (arg2_3637 >= 0)); (arg1_3634 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3609:unit) -> end): (unit ->
X)
abst_arg: main_3609, unit
abst_arg: main_3609, unit
abstract_term: end: X
abstract_term: arg1_3634: int
abstract_term: arg2_3637: int
filter
cond: (not (arg2_3637 >= 0)); (arg1_3634 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3637 arg1_3634 (fun (main_3609:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3637 + arg1_3634 && x_1; x_1]
cond: (not (arg1_3634 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3637) + arg1_3634)) && false))
tt:true
ff:false

cond: (not (arg1_3634 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3609:unit) -> end): (unit ->
X)
abst_arg: main_3609, unit
abst_arg: main_3609, unit
abstract_term: end: X
abstract_term: arg1_3634: int
abstract_term: arg2_3637: int
filter
cond: (not (arg1_3634 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_3615:int) ->
                     (if (x_3615 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__3672:int) (x__3673:(int -> X)) ->
                            (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                           (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__3668:int) (x__3669:(int -> X)) ->
                            (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                           (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_3615:int) ->
    (if (x_3615 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
          (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
          (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_3615:int) ->
                      (if (x_3615 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__3672:int) (x__3673:(int -> X)) ->
                             (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                            (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__3668:int) (x__3669:(int -> X)) ->
                             (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                            (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_3615:int) ->
                    (if (x_3615 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__3672:int) (x__3673:(int -> X)) ->
                           (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                          (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__3668:int) (x__3669:(int -> X)) ->
                           (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                          (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_3615:int) ->
                  (if (x_3615 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__3672:int) (x__3673:(int -> X)) ->
                         (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                        (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__3668:int) (x__3669:(int -> X)) ->
                         (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                        (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))))))): X
abstract_term: (fun (x_3615:int) ->
                (if (x_3615 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__3672:int) (x__3673:(int -> X)) ->
                       (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                      (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__3668:int) (x__3669:(int -> X)) ->
                       (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                      (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)))))): (int ->
X)
abst_arg: x_3615, int
abst_arg: x_3615, int
abstract_term: (if (x_3615 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__3672:int) (x__3673:(int -> X)) ->
                      (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))) x_3615
                     (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__3668:int) (x__3669:(int -> X)) ->
                      (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))) m_1118
                     (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))))): X
abstract_term: (x_3615 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_3615 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))))
                   x_3615 (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))))
                 x_3615 (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671))): X
abstract_term: (fun (x__3671:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3671)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__3671, int[i_1116 + 1 >= x_1028]
abst_arg: x__3671, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__3671): X
abstract_term: x__3671: int[i_1116 >= x_1028]
cond: (x_3615 > m_1118); (not (i_1116 >= x_1028))
pbs: x__3671 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_3615 > m_1118); (not (i_1116 >= x_1028))
pbs: x__3671 := ((i_1116 + 1) >= x_1028)

abstract_term: x_3615: int
abstract_term: (fun (x__3672:int) (x__3673:(int -> X)) -> (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674)))): (
int -> (int -> X) -> X)
abst_arg: x__3672, int
abst_arg: x__3673, (int ->
X)
abst_arg: x__3672, int
abst_arg: x__3673, (int ->
X)
abstract_term: (x_1117 x__3672 (fun (x__3674:int) -> (x__3673 x__3674))): X
abstract_term: (fun (x__3674:int) -> (x__3673 x__3674)): (int ->
X)
abst_arg: x__3674, int
abst_arg: x__3674, int
abstract_term: (x__3673 x__3674): X
abstract_term: x__3674: int
filter
cond: (x_3615 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__3672: int
filter
cond: (x_3615 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_3615 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))))
                   m_1118 (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))))
                 m_1118 (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667))): X
abstract_term: (fun (x__3667:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__3667)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__3667, int[i_1116 + 1 >= x_1028]
abst_arg: x__3667, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__3667): X
abstract_term: x__3667: int[i_1116 >= x_1028]
cond: (not (x_3615 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__3667 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_3615 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__3667 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__3668:int) (x__3669:(int -> X)) -> (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670)))): (
int -> (int -> X) -> X)
abst_arg: x__3668, int
abst_arg: x__3669, (int ->
X)
abst_arg: x__3668, int
abst_arg: x__3669, (int ->
X)
abstract_term: (x_1117 x__3668 (fun (x__3670:int) -> (x__3669 x__3670))): X
abstract_term: (fun (x__3670:int) -> (x__3669 x__3670)): (int ->
X)
abst_arg: x__3670, int
abst_arg: x__3670, int
abstract_term: (x__3669 x__3670): X
abstract_term: x__3670: int
filter
cond: (not (x_3615 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__3668: int
filter
cond: (not (x_3615 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_3615 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
                 (fun (m_3632:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
   (fun (m_3632:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_3632) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
                  (fun (m_3632:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
                   (fun (m_3632:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))) -1
                 (fun (m_3632:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))))))): X
abstract_term: (fun (m_3632:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_3632, int[i_1020 >= n_1019]
abst_arg: m_3632, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_3632) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))))): X
abstract_term: (n_1019 <= m_3632): x_1:bool[x_1]
cond: b_1314
pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_3632)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_3632); b_1314
pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675)))): X
abstract_term: (fail_1341 true (fun (x__3675:unit) -> (k_main_1201 x__3675))): X
abstract_term: (fun (x__3675:unit) -> (k_main_1201 x__3675)): (unit ->
X)
abst_arg: x__3675, unit
abst_arg: x__3675, unit
abstract_term: (k_main_1201 x__3675): X
abstract_term: x__3675: unit
filter
cond: (not (n_1019 <= m_3632)); b_1314
pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_3632)); b_1314
pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_3632)); b_1314
pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3632 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_3650:int) (k_make_array_3651:(int -> X)) -> (k_make_array_3651 (n_1019 - i_3650))): (
int -> (int -> X) -> X)
abst_arg: i_3650, int
abst_arg: k_make_array_3651, (int ->
X)
abst_arg: i_3650, int
abst_arg: k_make_array_3651, (int ->
X)
abstract_term: (k_make_array_3651 (n_1019 - i_3650)): X
abstract_term: (n_1019 - i_3650): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__3671 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__3667 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_3651 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_3651))
              (fun m_3632 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_3678 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_3681 k_array_max_1145)))))).
  f_3678 k_array_max_3677 x__3671 -> (k_array_max_3677 false).
  f_3681 k_array_max_3680 x__3667 -> (k_array_max_3680 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_3684 b2_1314) (f_3688 k_main_1201 b2_1314))))).
  f_3684 b2_3683 k_make_array_3651 -> (if ((not b2_3683) || (not b2_3683)) _|_ k_make_array_3651).
  f_3688 k_main_3686 b2_3687 m_3632 ->
      (if rand_bool (l0 (if ((not b2_3687) || (not b2_3687)) _|_ k_main_3686))
        (l1
          (if ((not b2_3687) || (not b2_3687)) _|_
            (fail_1341 true (if ((not b2_3687) || (not b2_3687)) _|_ k_main_3686))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_3690 b_3689 =
       (if b_3689
         (l0
           (let f_3692 b_3691 =
            (if b_3691
              (l0 (let f_3694 b_3693 = (k_main_1208 () true b_3693) in (if rand_bool (f_3694 true) (f_3694 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3692 true) (f_3692 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3690 true) (f_3690 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3696 b_3695 =
           (if b_3695 (l0 (array_max_1011 x_1117 (f_3678 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3681 k_array_max_1145))))
           in (if rand_bool (f_3696 true) (f_3696 false))))).
  f_3678 k_array_max_3677 x__3671 -> (k_array_max_3677 false).
  f_3681 k_array_max_3680 x__3667 -> (k_array_max_3680 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3698 b_3697 =
       (if b_3697 _|_
         (l0
           (let f_3700 b_3699 = (if b_3699 _|_ (array_max_1011 (f_3684 b2_1314) (f_3688 k_main_1201 b2_1314))) in
            (let f_3702 b_3701 =
             (if b_3701 (f_3700 true)
               (let f_3706 b_3705 = (if b_3705 (f_3700 false) (f_3700 true)) in (f_3706 b2_1314)))
             in (let f_3704 b_3703 = (if b_3703 (f_3702 false) (f_3702 true)) in (f_3704 b2_1314))))))
       in (let f_3708 b_3707 = (if b_3707 (f_3698 false) (f_3698 true)) in (f_3708 b2_1314))).
  f_3684 b2_3683 k_make_array_3651 ->
      (let f_3710 b_3709 = (if b_3709 _|_ k_make_array_3651) in
       (let f_3712 b_3711 =
        (if b_3711 (f_3710 true) (let f_3716 b_3715 = (if b_3715 (f_3710 false) (f_3710 true)) in (f_3716 b2_3683))) in
        (let f_3714 b_3713 = (if b_3713 (f_3712 false) (f_3712 true)) in (f_3714 b2_3683)))).
  f_3688 k_main_3686 b2_3687 m_3632 ->
      (let f_3718 b_3717 =
       (if b_3717
         (l0
           (let f_3720 b_3719 = (if b_3719 _|_ k_main_3686) in
            (let f_3722 b_3721 =
             (if b_3721 (f_3720 true)
               (let f_3726 b_3725 = (if b_3725 (f_3720 false) (f_3720 true)) in (f_3726 b2_3687)))
             in (let f_3724 b_3723 = (if b_3723 (f_3722 false) (f_3722 true)) in (f_3724 b2_3687)))))
         (l1
           (let f_3728 b_3727 =
            (if b_3727 _|_
              (fail_1341 true
                (let f_3730 b_3729 = (if b_3729 _|_ k_main_3686) in
                 (let f_3732 b_3731 =
                  (if b_3731 (f_3730 true)
                    (let f_3736 b_3735 = (if b_3735 (f_3730 false) (f_3730 true)) in (f_3736 b2_3687)))
                  in (let f_3734 b_3733 = (if b_3733 (f_3732 false) (f_3732 true)) in (f_3734 b2_3687))))))
            in
            (let f_3738 b_3737 =
             (if b_3737 (f_3728 true)
               (let f_3742 b_3741 = (if b_3741 (f_3728 false) (f_3728 true)) in (f_3742 b2_3687)))
             in (let f_3740 b_3739 = (if b_3739 (f_3738 false) (f_3738 true)) in (f_3740 b2_3687))))))
       in (if rand_bool (f_3718 true) (f_3718 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3744 b_3743 =
       (if b_3743 _|_
         (l1
           (let f_3746 b_3745 = (if b_3745 _|_ k_main_1201) in
            (let f_3748 b_3747 =
             (if b_3747 (f_3746 true)
               (let f_3754 b_3753 =
                (if b_3753 (f_3746 true)
                  (let f_3756 b_3755 = (if b_3755 (f_3746 false) (f_3746 true)) in (f_3756 b1_1314)))
                in (f_3754 b2_1314)))
             in
             (let f_3750 b_3749 =
              (if b_3749 (f_3748 true)
                (let f_3752 b_3751 = (if b_3751 (f_3748 false) (f_3748 true)) in (f_3752 b1_1314)))
              in (f_3750 b2_1314))))))
       in
       (let f_3758 b_3757 =
        (if b_3757 (f_3744 true) (let f_3760 b_3759 = (if b_3759 (f_3744 false) (f_3744 true)) in (f_3760 b1_1314))) in
        (f_3758 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_3690 b_3689 =
       (if b_3689
         (l0
           (let f_3692 b_3691 =
            (if b_3691
              (l0 (let f_3694 b_3693 = (k_main_1208 () true b_3693) in (if rand_bool (f_3694 true) (f_3694 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_3692 true) (f_3692 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_3690 true) (f_3690 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_3696 b_3695 =
           (if b_3695 (l0 (array_max_1011 x_1117 (f_3678 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_3681 k_array_max_1145))))
           in (if rand_bool (f_3696 true) (f_3696 false))))).
  f_3678 k_array_max_3677 x__3671 -> (k_array_max_3677 false).
  f_3681 k_array_max_3680 x__3667 -> (k_array_max_3680 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3698 b_3697 =
       (if b_3697 _|_
         (l0
           (let f_3700 b_3699 = (if b_3699 _|_ (array_max_1011 (f_3684 b2_1314) (f_3688 k_main_1201 b2_1314))) in
            (let f_3702 b_3701 =
             (if b_3701 (f_3700 true)
               (let f_3706 b_3705 = (if b_3705 (f_3700 false) (f_3700 true)) in (f_3706 b2_1314)))
             in (let f_3704 b_3703 = (if b_3703 (f_3702 false) (f_3702 true)) in (f_3704 b2_1314))))))
       in (let f_3708 b_3707 = (if b_3707 (f_3698 false) (f_3698 true)) in (f_3708 b2_1314))).
  f_3684 b2_3683 k_make_array_3651 ->
      (let f_3710 b_3709 = (if b_3709 _|_ k_make_array_3651) in
       (let f_3712 b_3711 =
        (if b_3711 (f_3710 true) (let f_3716 b_3715 = (if b_3715 (f_3710 false) (f_3710 true)) in (f_3716 b2_3683))) in
        (let f_3714 b_3713 = (if b_3713 (f_3712 false) (f_3712 true)) in (f_3714 b2_3683)))).
  f_3688 k_main_3686 b2_3687 m_3632 ->
      (let f_3718 b_3717 =
       (if b_3717
         (l0
           (let f_3720 b_3719 = (if b_3719 _|_ k_main_3686) in
            (let f_3722 b_3721 =
             (if b_3721 (f_3720 true)
               (let f_3726 b_3725 = (if b_3725 (f_3720 false) (f_3720 true)) in (f_3726 b2_3687)))
             in (let f_3724 b_3723 = (if b_3723 (f_3722 false) (f_3722 true)) in (f_3724 b2_3687)))))
         (l1
           (let f_3728 b_3727 =
            (if b_3727 _|_
              (fail_1341 true
                (let f_3730 b_3729 = (if b_3729 _|_ k_main_3686) in
                 (let f_3732 b_3731 =
                  (if b_3731 (f_3730 true)
                    (let f_3736 b_3735 = (if b_3735 (f_3730 false) (f_3730 true)) in (f_3736 b2_3687)))
                  in (let f_3734 b_3733 = (if b_3733 (f_3732 false) (f_3732 true)) in (f_3734 b2_3687))))))
            in
            (let f_3738 b_3737 =
             (if b_3737 (f_3728 true)
               (let f_3742 b_3741 = (if b_3741 (f_3728 false) (f_3728 true)) in (f_3742 b2_3687)))
             in (let f_3740 b_3739 = (if b_3739 (f_3738 false) (f_3738 true)) in (f_3740 b2_3687))))))
       in (if rand_bool (f_3718 true) (f_3718 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_3744 b_3743 =
       (if b_3743 _|_
         (l1
           (let f_3746 b_3745 = (if b_3745 _|_ k_main_1201) in
            (let f_3748 b_3747 =
             (if b_3747 (f_3746 true)
               (let f_3754 b_3753 =
                (if b_3753 (f_3746 true)
                  (let f_3756 b_3755 = (if b_3755 (f_3746 false) (f_3746 true)) in (f_3756 b1_1314)))
                in (f_3754 b2_1314)))
             in
             (let f_3750 b_3749 =
              (if b_3749 (f_3748 true)
                (let f_3752 b_3751 = (if b_3751 (f_3748 false) (f_3748 true)) in (f_3752 b1_1314)))
              in (f_3750 b2_1314))))))
       in
       (let f_3758 b_3757 =
        (if b_3757 (f_3744 true) (let f_3760 b_3759 = (if b_3759 (f_3744 false) (f_3744 true)) in (f_3760 b1_1314))) in
        (f_3758 b2_1314))).

DONE!

(7-2) Checking HORS ... DONE!

Set wp_max_num to 8.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 8)::
Main: main_1332
  main_1332 -> (main_1089 f_1338).
  arg1_1085 k_main_arg1_1260 -> (rand_int k_main_arg1_1260).
  arg2_1087 arg1_1290 k_main_arg2_1272 -> (rand_int k_main_arg2_1272).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) -> (k_array_max_1145 m_1118).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (x_1119 i_1116 m_1118 x_1028 x_1117 (f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when b_1340 -> (k_main_1208 i_1020 n_1019 k_main_1201 (i_1020 <= 0)).
  br_main_1339 b_1340 n_1019 i_1020 k_main_1201 when (not b_1340) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  f_1338 main_1291 -> end.
  f_array_max_1333 i_1116 m_1118 x_1028 k_array_max_1145 x_1117 x_1184 ->
      (z_1120 i_1116 m_1118 x_1028 x_1184 (f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117)).
  f_array_max_1334 i_1116 m_1118 x_1028 x_1184 k_array_max_1145 x_1117 z_1183 ->
      (array_max_1011 x_1028 (i_1116 + 1) x_1117 z_1183 k_array_max_1145).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (m_1320 >= n_1019) -> (k_main_1201 ()).
  f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201 m_1320 when (not (m_1320 >= n_1019)) -> (fail_1341 true k_main_1201).
  f_main_1336 k_main_1255 arg1_1290 -> (arg2_1087 arg1_1290 (f_main_1337 arg1_1290 k_main_1255)).
  f_main_1337 arg1_1290 k_main_1255 arg2_1289 -> (main_1018 arg1_1290 arg2_1289 k_main_1255).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (m_1316 b_1314 i_1020 n_1019 (f_k_main_1335 b_1314 i_1020 n_1019 k_main_1201)).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (k_main_1201 ()).
  m_1316 b_1314 i_1020 n_1019 k_main_m_1317 ->
      (array_max_1011 n_1019 i_1020 (make_array_1008 n_1019) -1 k_main_m_1317).
  main_1018 n_1019 i_1020 k_main_1201 when (n_1019 > 0) -> (br_main_1339 (i_1020 >= 0) n_1019 i_1020 k_main_1201).
  main_1018 n_1019 i_1020 k_main_1201 when (not (n_1019 > 0)) -> (k_main_1208 i_1020 n_1019 k_main_1201 false).
  main_1089 k_main_1255 -> (arg1_1085 (f_main_1336 k_main_1255)).
  make_array_1008 n_1009 i_1010 k_make_array_1134 -> (k_make_array_1134 (n_1009 - i_1010)).
  x_1119 i_1116 m_1118 x_1028 x_1117 k_array_max_x_1152 -> (x_1117 i_1116 k_array_max_x_1152).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (x_1184 > m_1118) -> (k_array_max_z_1161 x_1184).
  z_1120 i_1116 m_1118 x_1028 x_1184 k_array_max_z_1161 when (not (x_1184 > m_1118)) -> (k_array_max_z_1161 m_1118).
Types:
  main_1332 : X
  array_max_1011 : (x_1:int -> x_2:int -> (int -> (int -> X) -> X) -> int -> (int[x_2 >= x_1] -> X) -> X)
  fail_1341 : (x_1:bool[x_1] -> (unit -> X) -> X)
  k_main_1208 : (x_1:int -> x_2:int -> (unit -> X) -> x_6:bool[(not x_6) || 1 <= -x_1 + x_2 && x_6; x_6] -> X)

(8-1) Abstracting ... EXPAND_NONREC:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun arg1_3972 ->
         (rand_int
           (fun arg2_3975 ->
            (if (arg1_3972 > 0)
              (l0
                (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
                  (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
              (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun x_3953 ->
           (if (x_3953 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145))
             (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020 (fun i_3988 k_make_array_3989 -> (k_make_array_3989 (n_1019 - i_3988))) -1
          (fun m_3970 -> (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

ETA: (rand_int
       (fun arg1_3972 ->
        (rand_int
          (fun arg2_3975 ->
           (if (arg1_3972 > 0)
             (l0
               (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
                 (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
             (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))))): X
ETA: (fun arg1_3972 ->
      (rand_int
        (fun arg2_3975 ->
         (if (arg1_3972 > 0)
           (l0
             (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
               (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
           (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false)))))): (int ->
unit)
ETA: (rand_int
       (fun arg2_3975 ->
        (if (arg1_3972 > 0)
          (l0
            (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
              (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
          (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))): unit
ETA: (fun arg2_3975 ->
      (if (arg1_3972 > 0)
        (l0
          (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
            (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
        (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false)))): (int ->
unit)
ETA: (if (arg1_3972 > 0)
       (l0
         (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
           (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))))
       (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false)): unit
ETA: (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
ETA: (fun main_3947 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3972: int
ETA: arg2_3975: int
ETA_AUX: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false): unit
ETA: (l0
       (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
         (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false)))): unit
ETA: (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)))
       (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false))): unit
ETA: (l1 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false)): unit
ETA: (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) false): unit
ETA: false: x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
ETA: (fun main_3947 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3972: int
ETA: arg2_3975: int
ETA_AUX: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false): unit
ETA: (l0 (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0))): unit
ETA: (k_main_1208 arg2_3975 arg1_3972 (fun main_3947 -> end) (arg2_3975 <= 0)): unit
ETA: (arg2_3975 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
ETA: (fun main_3947 -> end): (unit ->
X)
ETA: end: X
ETA: arg1_3972: int
ETA: arg2_3975: int
ETA_AUX: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)): unit
ETA_AUX: (rand_int
           (fun (arg2_3975:int) ->
            (if (arg1_3972 > 0)
              (l0
                (if (arg2_3975 >= 0)
                  (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                  (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))): unit
ETA_AUX: (rand_int
           (fun (arg1_3972:int) ->
            (rand_int
              (fun (arg2_3975:int) ->
               (if (arg1_3972 > 0)
                 (l0
                   (if (arg2_3975 >= 0)
                     (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                     (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))))): X
ETA: (l0 (k_array_max_1145 m_1118)): X
ETA: (k_array_max_1145 m_1118): X
ETA: m_1118: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 m_1118): X
ETA: (l1
       (x_1117 i_1116
         (fun x_3953 ->
          (if (x_3953 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145))
            (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))))): X
ETA: (x_1117 i_1116
       (fun x_3953 ->
        (if (x_3953 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145))
          (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))))): X
ETA: (fun x_3953 ->
      (if (x_3953 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145))
        (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)))): (int ->
X)
ETA: (if (x_3953 > m_1118) (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145))
       (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145))): X
ETA: (l1 (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 m_1118 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__4005: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__4005): X
ETA: m_1118: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__4006: int
ETA_AUX: (x_1117 x__4006): ((int -> X) ->
X)
ETA_AUX: x__4007: (int ->
X)
ETA_AUX: x__4008: int
ETA_AUX: (x__4007 x__4008): X
ETA_AUX: (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
           (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))): X
ETA: (l0 (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145)): X
ETA: (array_max_1011 x_1028 (i_1116 + 1) x_1117 x_3953 k_array_max_1145): X
ETA: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: k_array_max_1145: (int[i_1116 + 1 >= x_1028] ->
X)
ETA_AUX: x__4009: int[i_1116 >= x_1028]
ETA_AUX: (k_array_max_1145 x__4009): X
ETA: x_3953: int
ETA: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x_1117: (int -> (int -> X) -> X)
ETA_AUX: x__4010: int
ETA_AUX: (x_1117 x__4010): ((int -> X) ->
X)
ETA_AUX: x__4011: (int ->
X)
ETA_AUX: x__4012: int
ETA_AUX: (x__4011 x__4012): X
ETA_AUX: (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))): X
ETA: (i_1116 + 1): int
ETA: x_1028: int
ETA_AUX: (array_max_1011 x_1028 (i_1116 + 1)
           (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
           (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))): X
ETA: i_1116: int
ETA_AUX: (x_1117 i_1116
           (fun (x_3953:int) ->
            (if (x_3953 > m_1118)
              (l0
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))))
                  x_3953 (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
              (l1
                (array_max_1011 x_1028 (i_1116 + 1)
                  (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))))
                  m_1118 (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))))))): X
ETA: (k ()): X
ETA: (): unit
ETA_AUX: (k ()): X
ETA: (l0
       (array_max_1011 n_1019 i_1020 (fun i_3988 k_make_array_3989 -> (k_make_array_3989 (n_1019 - i_3988))) -1
         (fun m_3970 -> (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))))): X
ETA: (array_max_1011 n_1019 i_1020 (fun i_3988 k_make_array_3989 -> (k_make_array_3989 (n_1019 - i_3988))) -1
       (fun m_3970 -> (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))))): X
ETA: (fun m_3970 -> (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201)))): (int[
i_1020 >= n_1019] ->
X)
ETA: (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true k_main_1201))): X
ETA: (l1 (fail_1341 true k_main_1201)): X
ETA: (fail_1341 true k_main_1201): X
ETA: k_main_1201: (unit ->
X)
ETA_AUX: k_main_1201: (unit ->
X)
ETA_AUX: x__4013: unit
ETA_AUX: (k_main_1201 x__4013): X
ETA: true: x_1:bool[x_1]
ETA_AUX: (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))): X
ETA: (l0 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA: -1: int
ETA: (fun i_3988 k_make_array_3989 -> (k_make_array_3989 (n_1019 - i_3988))): (
int -> (int -> X) -> X)
ETA: (fun k_make_array_3989 -> (k_make_array_3989 (n_1019 - i_3988))): ((int -> X) ->
X)
ETA: (k_make_array_3989 (n_1019 - i_3988)): X
ETA: (n_1019 - i_3988): int
ETA_AUX: (k_make_array_3989 (n_1019 - i_3988)): X
ETA: i_1020: int
ETA: n_1019: int
ETA_AUX: (array_max_1011 n_1019 i_1020
           (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
           (fun (m_3970:int[i_1020 >= n_1019]) ->
            (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
              (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))))))): X
ETA: (l1 (k_main_1201 ())): X
ETA: (k_main_1201 ()): X
ETA: (): unit
ETA_AUX: (k_main_1201 ()): X
ETA_EXPAND:
Main: main_1332
  main_1332 ->
      (rand_int
        (fun (arg1_3972:int) ->
         (rand_int
           (fun (arg2_3975:int) ->
            (if (arg1_3972 > 0)
              (l0
                (if (arg2_3975 >= 0)
                  (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                  (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
              (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))))).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (i_1116 >= x_1028) ->
      (l0 (k_array_max_1145 m_1118)).
  array_max_1011 x_1028 i_1116 x_1117 m_1118 k_array_max_1145 when (not (i_1116 >= x_1028)) ->
      (l1
        (x_1117 i_1116
          (fun (x_3953:int) ->
           (if (x_3953 > m_1118)
             (l0
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))))
                 x_3953 (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
             (l1
               (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))))
                 m_1118 (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)))))))).
  fail_1341 b k -> {fail} => (k ()).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when b_1314 ->
      (l0
        (array_max_1011 n_1019 i_1020
          (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
          (fun (m_3970:int[i_1020 >= n_1019]) ->
           (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
             (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013)))))))).
  k_main_1208 i_1020 n_1019 k_main_1201 b_1314 when (not b_1314) -> (l1 (k_main_1201 ())).

main_1332: ENV: 

main_1332: (rand_int
             (fun (arg1_3972:int) ->
              (rand_int
                (fun (arg2_3975:int) ->
                 (if (arg1_3972 > 0)
                   (l0
                     (if (arg2_3975 >= 0)
                       (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                       (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                   (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))))) ===> (
rand_int
 (fun (arg1_3972:int) ->
  (rand_int
    (fun (arg2_3975:int) ->
     (if (arg1_3972 > 0)
       (l0
         (if (arg2_3975 >= 0) (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
           (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
       (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)))))))
main_1332:: (rand_int
              (fun (arg1_3972:int) ->
               (rand_int
                 (fun (arg2_3975:int) ->
                  (if (arg1_3972 > 0)
                    (l0
                      (if (arg2_3975 >= 0)
                        (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                        (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)))))))
abstract_term: (rand_int
                 (fun (arg1_3972:int) ->
                  (rand_int
                    (fun (arg2_3975:int) ->
                     (if (arg1_3972 > 0)
                       (l0
                         (if (arg2_3975 >= 0)
                           (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                           (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                       (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))))): X
abstract_term: (fun (arg1_3972:int) ->
                (rand_int
                  (fun (arg2_3975:int) ->
                   (if (arg1_3972 > 0)
                     (l0
                       (if (arg2_3975 >= 0)
                         (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                         (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                     (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)))))): (int ->
X)
abst_arg: arg1_3972, int
abst_arg: arg1_3972, int
abstract_term: (rand_int
                 (fun (arg2_3975:int) ->
                  (if (arg1_3972 > 0)
                    (l0
                      (if (arg2_3975 >= 0)
                        (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                        (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                    (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))): X
abstract_term: (fun (arg2_3975:int) ->
                (if (arg1_3972 > 0)
                  (l0
                    (if (arg2_3975 >= 0)
                      (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                      (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                  (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)))): (int ->
X)
abst_arg: arg2_3975, int
abst_arg: arg2_3975, int
abstract_term: (if (arg1_3972 > 0)
                 (l0
                   (if (arg2_3975 >= 0)
                     (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                     (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))))
                 (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))): X
abstract_term: (arg1_3972 > 0): x_1:bool[x_1]
cond: true
pbs: 
p:(arg1_3972 > 0)
tt:false
ff:false

abstract_term: (l0
                 (if (arg2_3975 >= 0)
                   (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                   (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)))): X
abstract_term: (if (arg2_3975 >= 0)
                 (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)))
                 (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false))): X
abstract_term: (arg2_3975 >= 0): x_1:bool[x_1]
cond: (arg1_3972 > 0); true
pbs: 
p:(arg2_3975 >= 0)
tt:false
ff:false

abstract_term: (l0 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0))): X
abstract_term: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) (arg2_3975 <= 0)): X
abstract_term: (arg2_3975 <= 0): x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
cond: (arg2_3975 >= 0); (arg1_3972 > 0); true
pbs: 
p:((not (arg2_3975 <= 0)) || ((1 <= ((-1 * arg2_3975) + arg1_3972)) && (arg2_3975 <= 0)))
tt:true
ff:false

cond: (arg2_3975 >= 0); (arg1_3972 > 0); true
pbs: 
p:(arg2_3975 <= 0)
tt:false
ff:false

abstract_term: (fun (main_3947:unit) -> end): (unit ->
X)
abst_arg: main_3947, unit
abst_arg: main_3947, unit
abstract_term: end: X
abstract_term: arg1_3972: int
abstract_term: arg2_3975: int
filter
cond: (arg2_3975 >= 0); (arg1_3972 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
cond: (not (arg2_3975 >= 0)); (arg1_3972 > 0); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3975) + arg1_3972)) && false))
tt:true
ff:false

cond: (not (arg2_3975 >= 0)); (arg1_3972 > 0); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3947:unit) -> end): (unit ->
X)
abst_arg: main_3947, unit
abst_arg: main_3947, unit
abstract_term: end: X
abstract_term: arg1_3972: int
abstract_term: arg2_3975: int
filter
cond: (not (arg2_3975 >= 0)); (arg1_3972 > 0); true
abstract_term: (l1 (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false)): X
abstract_term: (k_main_1208 arg2_3975 arg1_3972 (fun (main_3947:unit) -> end) false): X
abstract_term: false: x_1:bool[(not x_1) || 1 <= -arg2_3975 + arg1_3972 && x_1; x_1]
cond: (not (arg1_3972 > 0)); true
pbs: 
p:((not false) || ((1 <= ((-1 * arg2_3975) + arg1_3972)) && false))
tt:true
ff:false

cond: (not (arg1_3972 > 0)); true
pbs: 
p:false
tt:false
ff:true

abstract_term: (fun (main_3947:unit) -> end): (unit ->
X)
abst_arg: main_3947, unit
abst_arg: main_3947, unit
abstract_term: end: X
abstract_term: arg1_3972: int
abstract_term: arg2_3975: int
filter
cond: (not (arg1_3972 > 0)); true
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l0 (k_array_max_1145 m_1118)) ===> (l0 (k_array_max_1145 m_1118))
array_max_1011:: (l0 (k_array_max_1145 m_1118))
abstract_term: (l0 (k_array_max_1145 m_1118)): X
abstract_term: (k_array_max_1145 m_1118): X
abstract_term: m_1118: int[i_1116 >= x_1028]
cond: (i_1116 >= x_1028)
pbs: 
p:(i_1116 >= x_1028)
tt:true
ff:false

filter
cond: (i_1116 >= x_1028)
array_max_1011: ENV: x_1028:int, i_1116:int, x_1117:(int -> (int -> X) -> X), m_1118:int,
k_array_max_1145:(int[i_1116 >= x_1028] -> X),


abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
abst_arg: x_1028, int
abst_arg: i_1116, int
abst_arg: x_1117, (int -> (int -> X) -> X)
abst_arg: m_1118, int
abst_arg: k_array_max_1145, (int[i_1116 >= x_1028] ->
X)
array_max_1011: (l1
                  (x_1117 i_1116
                    (fun (x_3953:int) ->
                     (if (x_3953 > m_1118)
                       (l0
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__4010:int) (x__4011:(int -> X)) ->
                            (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                           (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                       (l1
                         (array_max_1011 x_1028 (i_1116 + 1)
                           (fun (x__4006:int) (x__4007:(int -> X)) ->
                            (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                           (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)))))))) ===> (
l1
 (x_1117 i_1116
   (fun (x_3953:int) ->
    (if (x_3953 > m_1118)
      (l0
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
          (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
      (l1
        (array_max_1011 x_1028 (i_1116 + 1)
          (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
          (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))))))))
array_max_1011:: (l1
                   (x_1117 i_1116
                     (fun (x_3953:int) ->
                      (if (x_3953 > m_1118)
                        (l0
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__4010:int) (x__4011:(int -> X)) ->
                             (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                            (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                        (l1
                          (array_max_1011 x_1028 (i_1116 + 1)
                            (fun (x__4006:int) (x__4007:(int -> X)) ->
                             (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                            (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))))))))
abstract_term: (l1
                 (x_1117 i_1116
                   (fun (x_3953:int) ->
                    (if (x_3953 > m_1118)
                      (l0
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__4010:int) (x__4011:(int -> X)) ->
                           (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                          (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                      (l1
                        (array_max_1011 x_1028 (i_1116 + 1)
                          (fun (x__4006:int) (x__4007:(int -> X)) ->
                           (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                          (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)))))))): X
abstract_term: (x_1117 i_1116
                 (fun (x_3953:int) ->
                  (if (x_3953 > m_1118)
                    (l0
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__4010:int) (x__4011:(int -> X)) ->
                         (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                        (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                    (l1
                      (array_max_1011 x_1028 (i_1116 + 1)
                        (fun (x__4006:int) (x__4007:(int -> X)) ->
                         (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                        (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))))))): X
abstract_term: (fun (x_3953:int) ->
                (if (x_3953 > m_1118)
                  (l0
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__4010:int) (x__4011:(int -> X)) ->
                       (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                      (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                  (l1
                    (array_max_1011 x_1028 (i_1116 + 1)
                      (fun (x__4006:int) (x__4007:(int -> X)) ->
                       (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                      (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)))))): (int ->
X)
abst_arg: x_3953, int
abst_arg: x_3953, int
abstract_term: (if (x_3953 > m_1118)
                 (l0
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__4010:int) (x__4011:(int -> X)) ->
                      (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))) x_3953
                     (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))))
                 (l1
                   (array_max_1011 x_1028 (i_1116 + 1)
                     (fun (x__4006:int) (x__4007:(int -> X)) ->
                      (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))) m_1118
                     (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))))): X
abstract_term: (x_3953 > m_1118): x_1:bool[x_1]
cond: (not (i_1116 >= x_1028))
pbs: 
p:(x_3953 > m_1118)
tt:false
ff:false

abstract_term: (l0
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))))
                   x_3953 (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))))
                 x_3953 (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009))): X
abstract_term: (fun (x__4009:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4009)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__4009, int[i_1116 + 1 >= x_1028]
abst_arg: x__4009, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__4009): X
abstract_term: x__4009: int[i_1116 >= x_1028]
cond: (x_3953 > m_1118); (not (i_1116 >= x_1028))
pbs: x__4009 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (x_3953 > m_1118); (not (i_1116 >= x_1028))
pbs: x__4009 := ((i_1116 + 1) >= x_1028)

abstract_term: x_3953: int
abstract_term: (fun (x__4010:int) (x__4011:(int -> X)) -> (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012)))): (
int -> (int -> X) -> X)
abst_arg: x__4010, int
abst_arg: x__4011, (int ->
X)
abst_arg: x__4010, int
abst_arg: x__4011, (int ->
X)
abstract_term: (x_1117 x__4010 (fun (x__4012:int) -> (x__4011 x__4012))): X
abstract_term: (fun (x__4012:int) -> (x__4011 x__4012)): (int ->
X)
abst_arg: x__4012, int
abst_arg: x__4012, int
abstract_term: (x__4011 x__4012): X
abstract_term: x__4012: int
filter
cond: (x_3953 > m_1118); (not (i_1116 >= x_1028))
abstract_term: x__4010: int
filter
cond: (x_3953 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (x_3953 > m_1118); (not (i_1116 >= x_1028))
abstract_term: (l1
                 (array_max_1011 x_1028 (i_1116 + 1)
                   (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))))
                   m_1118 (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)))): X
abstract_term: (array_max_1011 x_1028 (i_1116 + 1)
                 (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))))
                 m_1118 (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005))): X
abstract_term: (fun (x__4005:int[i_1116 + 1 >= x_1028]) -> (k_array_max_1145 x__4005)): (int[
i_1116 + 1 >= x_1028] ->
X)
abst_arg: x__4005, int[i_1116 + 1 >= x_1028]
abst_arg: x__4005, int[i_1116 + 1 >= x_1028]
abstract_term: (k_array_max_1145 x__4005): X
abstract_term: x__4005: int[i_1116 >= x_1028]
cond: (not (x_3953 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__4005 := ((i_1116 + 1) >= x_1028)
p:(i_1116 >= x_1028)
tt:false
ff:true

filter
cond: (not (x_3953 > m_1118)); (not (i_1116 >= x_1028))
pbs: x__4005 := ((i_1116 + 1) >= x_1028)

abstract_term: m_1118: int
abstract_term: (fun (x__4006:int) (x__4007:(int -> X)) -> (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008)))): (
int -> (int -> X) -> X)
abst_arg: x__4006, int
abst_arg: x__4007, (int ->
X)
abst_arg: x__4006, int
abst_arg: x__4007, (int ->
X)
abstract_term: (x_1117 x__4006 (fun (x__4008:int) -> (x__4007 x__4008))): X
abstract_term: (fun (x__4008:int) -> (x__4007 x__4008)): (int ->
X)
abst_arg: x__4008, int
abst_arg: x__4008, int
abstract_term: (x__4007 x__4008): X
abstract_term: x__4008: int
filter
cond: (not (x_3953 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: x__4006: int
filter
cond: (not (x_3953 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: (i_1116 + 1): int
abstract_term: x_1028: int
filter
cond: (not (x_3953 > m_1118)); (not (i_1116 >= x_1028))
abstract_term: i_1116: int
filter
cond: (not (i_1116 >= x_1028))
fail_1341: ENV: b:x_1:bool[x_1], k:(unit -> X),


abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
abst_arg: b, x_1:bool[x_1]
abst_arg: k, (unit ->
X)
fail_1341: (k ()) ===> (k ())
fail_1341:: (k ())
abstract_term: (k ()): X
abstract_term: (): unit
filter
cond: true
pbs: b := b

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l0
               (array_max_1011 n_1019 i_1020
                 (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
                 (fun (m_3970:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013)))))))) ===> (
l0
 (array_max_1011 n_1019 i_1020
   (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
   (fun (m_3970:int[i_1020 >= n_1019]) ->
    (if (n_1019 <= m_3970) (l0 (k_main_1201 ())) (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))))))))
k_main_1208:: (l0
                (array_max_1011 n_1019 i_1020
                  (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
                  (fun (m_3970:int[i_1020 >= n_1019]) ->
                   (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                     (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))))))))
abstract_term: (l0
                 (array_max_1011 n_1019 i_1020
                   (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
                   (fun (m_3970:int[i_1020 >= n_1019]) ->
                    (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                      (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013)))))))): X
abstract_term: (array_max_1011 n_1019 i_1020
                 (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))) -1
                 (fun (m_3970:int[i_1020 >= n_1019]) ->
                  (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                    (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))))))): X
abstract_term: (fun (m_3970:int[i_1020 >= n_1019]) ->
                (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                  (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013)))))): (int[
i_1020 >= n_1019] ->
X)
abst_arg: m_3970, int[i_1020 >= n_1019]
abst_arg: m_3970, int[i_1020 >= n_1019]
abstract_term: (if (n_1019 <= m_3970) (l0 (k_main_1201 ()))
                 (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))))): X
abstract_term: (n_1019 <= m_3970): x_1:bool[x_1]
cond: b_1314
pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:(n_1019 <= m_3970)
tt:false
ff:false

abstract_term: (l0 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (n_1019 <= m_3970); b_1314
pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: (l1 (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013)))): X
abstract_term: (fail_1341 true (fun (x__4013:unit) -> (k_main_1201 x__4013))): X
abstract_term: (fun (x__4013:unit) -> (k_main_1201 x__4013)): (unit ->
X)
abst_arg: x__4013, unit
abst_arg: x__4013, unit
abstract_term: (k_main_1201 x__4013): X
abstract_term: x__4013: unit
filter
cond: (not (n_1019 <= m_3970)); b_1314
pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: true: x_1:bool[x_1]
cond: (not (n_1019 <= m_3970)); b_1314
pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314
p:true
tt:true
ff:false

filter
cond: (not (n_1019 <= m_3970)); b_1314
pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: m_3970 := (i_1020 >= n_1019);
     b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: -1: int
abstract_term: (fun (i_3988:int) (k_make_array_3989:(int -> X)) -> (k_make_array_3989 (n_1019 - i_3988))): (
int -> (int -> X) -> X)
abst_arg: i_3988, int
abst_arg: k_make_array_3989, (int ->
X)
abst_arg: i_3988, int
abst_arg: k_make_array_3989, (int ->
X)
abstract_term: (k_make_array_3989 (n_1019 - i_3988)): X
abstract_term: (n_1019 - i_3988): int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

abstract_term: i_1020: int
abstract_term: n_1019: int
filter
cond: b_1314
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314));
     b2_1314 := b_1314

k_main_1208: ENV: i_1020:int, n_1019:int, k_main_1201:(unit -> X),
b_1314:x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1],


abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
abst_arg: i_1020, int
abst_arg: n_1019, int
abst_arg: k_main_1201, (unit ->
X)
abst_arg: b_1314, x_1:bool[(not x_1) || 1 <= -i_1020 + n_1019 && x_1; x_1]
k_main_1208: (l1 (k_main_1201 ())) ===> (l1 (k_main_1201 ()))
k_main_1208:: (l1 (k_main_1201 ()))
abstract_term: (l1 (k_main_1201 ())): X
abstract_term: (k_main_1201 ()): X
abstract_term: (): unit
filter
cond: (not b_1314)
pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

pbs: b1_1314 := ((not b_1314) || ((1 <= ((-1 * i_1020) + n_1019)) && b_1314))

ABST:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (fun x__4009 -> (k_array_max_1145 false))))
            (l1 (array_max_1011 x_1117 (fun x__4005 -> (k_array_max_1145 false))))))).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0
          (if ((not b2_1314) || (not b2_1314)) _|_
            (array_max_1011 (fun k_make_array_3989 -> (if ((not b2_1314) || (not b2_1314)) _|_ k_make_array_3989))
              (fun m_3970 ->
               (if rand_bool (l0 (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201))
                 (l1
                   (if ((not b2_1314) || (not b2_1314)) _|_
                     (fail_1341 true (if ((not b2_1314) || (not b2_1314)) _|_ k_main_1201)))))))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).
Types:
  arg1_1085 : (unit -> unit)
  arg2_1087 : (unit -> unit)
  br_main_1339 : (bool -> unit -> unit)
  f_1338 : unit
  f_array_max_1333 : (unit -> (unit -> unit) -> unit)
  f_array_max_1334 : (unit -> (unit -> unit) -> unit)
  f_k_main_1335 : (bool -> unit -> unit)
  f_main_1336 : (unit -> unit)
  f_main_1337 : (unit -> unit)
  m_1316 : (bool -> unit -> unit)
  main_1018 : (unit -> unit)
  main_1089 : (unit -> unit)
  make_array_1008 : (unit -> unit)
  x_1119 : ((unit -> unit) -> unit -> unit)
  z_1120 : (unit -> unit)

LIFT:
Main: main_1332
  main_1332 ->
      (if rand_bool (l0 (if rand_bool (l0 (k_main_1208 () true rand_bool)) (l1 (k_main_1208 () true false))))
        (l1 (k_main_1208 () true false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (if rand_bool (l0 (array_max_1011 x_1117 (f_4016 k_array_max_1145)))
            (l1 (array_max_1011 x_1117 (f_4019 k_array_max_1145)))))).
  f_4016 k_array_max_4015 x__4009 -> (k_array_max_4015 false).
  f_4019 k_array_max_4018 x__4005 -> (k_array_max_4018 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (not b2_1314) _|_
        (l0 (if ((not b2_1314) || (not b2_1314)) _|_ (array_max_1011 (f_4022 b2_1314) (f_4026 k_main_1201 b2_1314))))).
  f_4022 b2_4021 k_make_array_3989 -> (if ((not b2_4021) || (not b2_4021)) _|_ k_make_array_3989).
  f_4026 k_main_4024 b2_4025 m_3970 ->
      (if rand_bool (l0 (if ((not b2_4025) || (not b2_4025)) _|_ k_main_4024))
        (l1
          (if ((not b2_4025) || (not b2_4025)) _|_
            (fail_1341 true (if ((not b2_4025) || (not b2_4025)) _|_ k_main_4024))))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (if (b2_1314 || (not b1_1314)) _|_
        (l1 (if ((b2_1314 || (not b1_1314)) || (b2_1314 || (not b1_1314))) _|_ k_main_1201))).

TRANS_EAGER:
Main: main_1332
  main_1332 ->
      (let f_4028 b_4027 =
       (if b_4027
         (l0
           (let f_4030 b_4029 =
            (if b_4029
              (l0 (let f_4032 b_4031 = (k_main_1208 () true b_4031) in (if rand_bool (f_4032 true) (f_4032 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_4030 true) (f_4030 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_4028 true) (f_4028 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_4034 b_4033 =
           (if b_4033 (l0 (array_max_1011 x_1117 (f_4016 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_4019 k_array_max_1145))))
           in (if rand_bool (f_4034 true) (f_4034 false))))).
  f_4016 k_array_max_4015 x__4009 -> (k_array_max_4015 false).
  f_4019 k_array_max_4018 x__4005 -> (k_array_max_4018 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_4036 b_4035 =
       (if b_4035 _|_
         (l0
           (let f_4038 b_4037 = (if b_4037 _|_ (array_max_1011 (f_4022 b2_1314) (f_4026 k_main_1201 b2_1314))) in
            (let f_4040 b_4039 =
             (if b_4039 (f_4038 true)
               (let f_4044 b_4043 = (if b_4043 (f_4038 false) (f_4038 true)) in (f_4044 b2_1314)))
             in (let f_4042 b_4041 = (if b_4041 (f_4040 false) (f_4040 true)) in (f_4042 b2_1314))))))
       in (let f_4046 b_4045 = (if b_4045 (f_4036 false) (f_4036 true)) in (f_4046 b2_1314))).
  f_4022 b2_4021 k_make_array_3989 ->
      (let f_4048 b_4047 = (if b_4047 _|_ k_make_array_3989) in
       (let f_4050 b_4049 =
        (if b_4049 (f_4048 true) (let f_4054 b_4053 = (if b_4053 (f_4048 false) (f_4048 true)) in (f_4054 b2_4021))) in
        (let f_4052 b_4051 = (if b_4051 (f_4050 false) (f_4050 true)) in (f_4052 b2_4021)))).
  f_4026 k_main_4024 b2_4025 m_3970 ->
      (let f_4056 b_4055 =
       (if b_4055
         (l0
           (let f_4058 b_4057 = (if b_4057 _|_ k_main_4024) in
            (let f_4060 b_4059 =
             (if b_4059 (f_4058 true)
               (let f_4064 b_4063 = (if b_4063 (f_4058 false) (f_4058 true)) in (f_4064 b2_4025)))
             in (let f_4062 b_4061 = (if b_4061 (f_4060 false) (f_4060 true)) in (f_4062 b2_4025)))))
         (l1
           (let f_4066 b_4065 =
            (if b_4065 _|_
              (fail_1341 true
                (let f_4068 b_4067 = (if b_4067 _|_ k_main_4024) in
                 (let f_4070 b_4069 =
                  (if b_4069 (f_4068 true)
                    (let f_4074 b_4073 = (if b_4073 (f_4068 false) (f_4068 true)) in (f_4074 b2_4025)))
                  in (let f_4072 b_4071 = (if b_4071 (f_4070 false) (f_4070 true)) in (f_4072 b2_4025))))))
            in
            (let f_4076 b_4075 =
             (if b_4075 (f_4066 true)
               (let f_4080 b_4079 = (if b_4079 (f_4066 false) (f_4066 true)) in (f_4080 b2_4025)))
             in (let f_4078 b_4077 = (if b_4077 (f_4076 false) (f_4076 true)) in (f_4078 b2_4025))))))
       in (if rand_bool (f_4056 true) (f_4056 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_4082 b_4081 =
       (if b_4081 _|_
         (l1
           (let f_4084 b_4083 = (if b_4083 _|_ k_main_1201) in
            (let f_4086 b_4085 =
             (if b_4085 (f_4084 true)
               (let f_4092 b_4091 =
                (if b_4091 (f_4084 true)
                  (let f_4094 b_4093 = (if b_4093 (f_4084 false) (f_4084 true)) in (f_4094 b1_1314)))
                in (f_4092 b2_1314)))
             in
             (let f_4088 b_4087 =
              (if b_4087 (f_4086 true)
                (let f_4090 b_4089 = (if b_4089 (f_4086 false) (f_4086 true)) in (f_4090 b1_1314)))
              in (f_4088 b2_1314))))))
       in
       (let f_4096 b_4095 =
        (if b_4095 (f_4082 true) (let f_4098 b_4097 = (if b_4097 (f_4082 false) (f_4082 true)) in (f_4098 b1_1314))) in
        (f_4096 b2_1314))).

PUT_INTO_IF:
Main: main_1332
  main_1332 ->
      (let f_4028 b_4027 =
       (if b_4027
         (l0
           (let f_4030 b_4029 =
            (if b_4029
              (l0 (let f_4032 b_4031 = (k_main_1208 () true b_4031) in (if rand_bool (f_4032 true) (f_4032 false))))
              (l1 (k_main_1208 () true false)))
            in (if rand_bool (f_4030 true) (f_4030 false)))) (l1 (k_main_1208 () true false)))
       in (if rand_bool (f_4028 true) (f_4028 false))).
  array_max_1011 x_1117 k_array_max_1145 -> (l0 (k_array_max_1145 true)).
  array_max_1011 x_1117 k_array_max_1145 ->
      (l1
        (x_1117
          (let f_4034 b_4033 =
           (if b_4033 (l0 (array_max_1011 x_1117 (f_4016 k_array_max_1145)))
             (l1 (array_max_1011 x_1117 (f_4019 k_array_max_1145))))
           in (if rand_bool (f_4034 true) (f_4034 false))))).
  f_4016 k_array_max_4015 x__4009 -> (k_array_max_4015 false).
  f_4019 k_array_max_4018 x__4005 -> (k_array_max_4018 false).
  fail_1341 b k -> {fail} => k.
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_4036 b_4035 =
       (if b_4035 _|_
         (l0
           (let f_4038 b_4037 = (if b_4037 _|_ (array_max_1011 (f_4022 b2_1314) (f_4026 k_main_1201 b2_1314))) in
            (let f_4040 b_4039 =
             (if b_4039 (f_4038 true)
               (let f_4044 b_4043 = (if b_4043 (f_4038 false) (f_4038 true)) in (f_4044 b2_1314)))
             in (let f_4042 b_4041 = (if b_4041 (f_4040 false) (f_4040 true)) in (f_4042 b2_1314))))))
       in (let f_4046 b_4045 = (if b_4045 (f_4036 false) (f_4036 true)) in (f_4046 b2_1314))).
  f_4022 b2_4021 k_make_array_3989 ->
      (let f_4048 b_4047 = (if b_4047 _|_ k_make_array_3989) in
       (let f_4050 b_4049 =
        (if b_4049 (f_4048 true) (let f_4054 b_4053 = (if b_4053 (f_4048 false) (f_4048 true)) in (f_4054 b2_4021))) in
        (let f_4052 b_4051 = (if b_4051 (f_4050 false) (f_4050 true)) in (f_4052 b2_4021)))).
  f_4026 k_main_4024 b2_4025 m_3970 ->
      (let f_4056 b_4055 =
       (if b_4055
         (l0
           (let f_4058 b_4057 = (if b_4057 _|_ k_main_4024) in
            (let f_4060 b_4059 =
             (if b_4059 (f_4058 true)
               (let f_4064 b_4063 = (if b_4063 (f_4058 false) (f_4058 true)) in (f_4064 b2_4025)))
             in (let f_4062 b_4061 = (if b_4061 (f_4060 false) (f_4060 true)) in (f_4062 b2_4025)))))
         (l1
           (let f_4066 b_4065 =
            (if b_4065 _|_
              (fail_1341 true
                (let f_4068 b_4067 = (if b_4067 _|_ k_main_4024) in
                 (let f_4070 b_4069 =
                  (if b_4069 (f_4068 true)
                    (let f_4074 b_4073 = (if b_4073 (f_4068 false) (f_4068 true)) in (f_4074 b2_4025)))
                  in (let f_4072 b_4071 = (if b_4071 (f_4070 false) (f_4070 true)) in (f_4072 b2_4025))))))
            in
            (let f_4076 b_4075 =
             (if b_4075 (f_4066 true)
               (let f_4080 b_4079 = (if b_4079 (f_4066 false) (f_4066 true)) in (f_4080 b2_4025)))
             in (let f_4078 b_4077 = (if b_4077 (f_4076 false) (f_4076 true)) in (f_4078 b2_4025))))))
       in (if rand_bool (f_4056 true) (f_4056 false))).
  k_main_1208 k_main_1201 b1_1314 b2_1314 ->
      (let f_4082 b_4081 =
       (if b_4081 _|_
         (l1
           (let f_4084 b_4083 = (if b_4083 _|_ k_main_1201) in
            (let f_4086 b_4085 =
             (if b_4085 (f_4084 true)
               (let f_4092 b_4091 =
                (if b_4091 (f_4084 true)
                  (let f_4094 b_4093 = (if b_4093 (f_4084 false) (f_4084 true)) in (f_4094 b1_1314)))
                in (f_4092 b2_1314)))
             in
             (let f_4088 b_4087 =
              (if b_4087 (f_4086 true)
                (let f_4090 b_4089 = (if b_4089 (f_4086 false) (f_4086 true)) in (f_4090 b1_1314)))
              in (f_4088 b2_1314))))))
       in
       (let f_4096 b_4095 =
        (if b_4095 (f_4082 true) (let f_4098 b_4097 = (if b_4097 (f_4082 false) (f_4082 true)) in (f_4098 b1_1314))) in
        (f_4096 b2_1314))).

DONE!

(8-2) Checking HORS ... DONE!

Error trace::
  main_1332 ... --> 
  main_1089 ... --> 
  arg1_1085 ... --> 
  f_main_1336 ... --> 
  arg2_1087 ... --> 
  f_main_1337 ... --> 
  main_1018 [1/2] ... --> 
  br_main_1339 [1/2] ... --> 
  k_main_1208 [1/2] ... --> 
  m_1316 ... --> 
  array_max_1011 [1/2] ... --> 
  f_k_main_1335 [2/2] ... --> 
  fail_1341 ... --> fail -->
  ERROR!

Verification failed (new error path not found)
