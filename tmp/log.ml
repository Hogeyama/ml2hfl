MoCHi: Model Checker for Higher-Order Programs
  Build: _a021351 (after 2014-07-24 14:36:51 +0900)
  FPAT version: 3c21822
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt tmp/test.ml -tupling -debug-module CEGAR_abst_CPS -disable-rc -color -tupling -list-option 
           -abs-remove-false -fpat -hccs 1 -bool-init-empty

parsed:
 let rec append_1008 xs__ys_1009 =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun x_1036 -> (match x_1036 with
                                     | _ -> (true, 0)))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1050 = {fail} () in
                 _|_)
   | _ -> let u_1051 = {fail} () in
          _|_
 in
 let mynot_1012 b_1013 = if b_1013 then
                           false
                         else
                           true in
 let main_1017_1014 x_1056 = match x_1056 with
                             | () -> append_1008 (fun b_1015 -> (mynot_1012 b_1015, 0)) in
 ()

set_target:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun (x_1036:bool) -> (match x_1036 with
                                            | _ -> (true, 0)))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1050 = {fail} () in
                 _|_)
   | _ -> let u_1051 = {fail} () in
          _|_
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = match x_1056 with
                                    | () -> append_1008 (fun (b_1015:bool) -> (mynot_1012 b_1015, 0)) in
 let main_1072 = let arg1_1070 = () in
                 main_1017_1014 arg1_1070 in
 ()

abst_recdata:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   match xs__ys_1009 true with
   | (b_1010, _) ->
       if b_1010 then
         append_1008 (fun (x_1075:bool) -> (true, 0))
       else
         (match xs__ys_1009 false with
          | (_, r2_1011) -> if r2_1011 = 0 then
                              ()
                            else
                              {fail} ()
          | _ -> let u_1074 = {fail} () in
                 _|_)
   | _ -> let u_1076 = {fail} () in
          _|_
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = let arg1_1070 = () in
                 main_1017_1014 arg1_1070 in
 ()

encode_list:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   let xs_1102 = xs__ys_1009 true in
   let b_1010 = fst xs_1102 in
   if b_1010 then
     append_1008 (fun (x_1100:bool) -> (true, 0))
   else
     let xs_1103 = xs__ys_1009 false in
     let r2_1011 = snd xs_1103 in
     if r2_1011 = 0 then
       ()
     else
       {fail} ()
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = main_1017_1014 () in
 ()

inlined:
 let rec append_1008 (xs__ys_1009:(bool -> (bool * int))) =
   let xs_1102 = xs__ys_1009 true in
   if fst xs_1102 then
     append_1008 (fun (x_1100:bool) -> (true, 0))
   else
     let xs_1103 = xs__ys_1009 false in
     if snd xs_1103 = 0 then
       ()
     else
       {fail} ()
 in
 let mynot_1012 (b_1013:bool) = if b_1013 then
                                  false
                                else
                                  true in
 let main_1017_1014 (x_1056:unit) = append_1008 (fun (b_1077:bool) -> (mynot_1012 b_1077, 0)) in
 let main_1072 = main_1017_1014 () in
 ()

CPS:
 let rec append_1008 (xs__ys_1009:(bool -> ((bool * int) -> X) -> X)) (k_append_1119:(unit -> X)) =
   let xs_1102 (k_append_xs_1126:((bool * int) -> X)) = xs__ys_1009 true k_append_xs_1126 in
   xs_1102
     (fun (xs_1187:(bool * int)) ->
        (if fst xs_1187 then
           append_1008 (fun (x_1279:bool) -> fun (k_append_1280:((bool * int) -> X)) -> k_append_1280 (true, 0))
             k_append_1119
         else
           let xs_1103 (k_append_xs_1165:((bool * int) -> X)) = xs__ys_1009 false k_append_xs_1165 in
           xs_1103
             (fun (xs_1182:(bool * int)) ->
                (if snd xs_1182 = 0 then
                   k_append_1119 ()
                 else
                   {|fail|} () k_append_1119))))
 in
 let mynot_1012 (b_1013:bool) (k_mynot_1197:(bool -> X)) = if b_1013 then
                                                             k_mynot_1197 false
                                                           else
                                                             k_mynot_1197 true in
 let main_1017_1014 (x_1056:unit) (k_main_1017_1208:(unit -> X)) =
   append_1008
     (fun (b_1284:bool) ->
        fun (k_main_1017_1285:((bool * int) -> X)) ->
          mynot_1012 b_1284 (fun (x_1292:bool) -> k_main_1017_1285 (x_1292, 0))) k_main_1017_1208
 in
 let main_1072 (k_main_1255:(unit -> X)) = main_1017_1014 () k_main_1255 in
 main_1072 (fun (main_1261:unit) -> {end})

remove_pair:
 let rec append_1008 (xs__ys_1009:(bool -> (bool -> int -> X) -> X)) (k_append_1119:(unit -> X)) =
   let xs_1102 (k_append_xs_1126:(bool -> int -> X)) = xs__ys_1009 true k_append_xs_1126 in
   xs_1102
     (fun (xs0_1187:bool) ->
        fun (xs1_1187:int) ->
          (if xs0_1187 then
             append_1008 (fun (x_1279:bool) -> fun (k_append_1280:(bool -> int -> X)) -> k_append_1280 true 0)
               k_append_1119
           else
             let xs_1103 (k_append_xs_1165:(bool -> int -> X)) = xs__ys_1009 false k_append_xs_1165 in
             xs_1103
               (fun (xs0_1182:bool) ->
                  fun (xs1_1182:int) -> (if xs1_1182 = 0 then
                                           k_append_1119 ()
                                         else
                                           {|fail|} () k_append_1119))))
 in
 let mynot_1012 (b_1013:bool) (k_mynot_1197:(bool -> X)) = if b_1013 then
                                                             k_mynot_1197 false
                                                           else
                                                             k_mynot_1197 true in
 let main_1017_1014 (x_1056:unit) (k_main_1017_1208:(unit -> X)) =
   append_1008
     (fun (b_1284:bool) ->
        fun (k_main_1017_1285:(bool -> int -> X)) -> mynot_1012 b_1284 (fun (x_1292:bool) -> k_main_1017_1285 x_1292 0))
     k_main_1017_1208
 in
 let main_1072 (k_main_1255:(unit -> X)) = main_1017_1014 () k_main_1255 in
 main_1072 (fun (main_1261:unit) -> {end})

Program with abstraction types (CEGAR-cycle 0)::
Main: main_1294
  main_1294 -> (main_1072 f_1300);;
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009));;
  f_1300 main_1261 -> end;;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119);;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119));;
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0);;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ());;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119);;
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285));;
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0);;
  fail_1301 b k -> {fail} => (k ());;
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208);;
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255);;
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false);;
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true);;
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126);;
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165);;
Types:
  main_1294 : X
  append_1008 : ((bool -> (bool -> int -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(0-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_1369 k_main_1017_1370 -> (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
        (fun main_1345 -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1352 xs1_1353 ->
         (if xs0_1352 (l0 (append_1008 (fun x_1356 k_append_1357 -> (k_append_1357 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1366 xs1_1367 ->
                (if (xs1_1367 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))));;
  fail_1301 b k -> {fail} => (k ());;

ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
         (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))) (
        fun (main_1345:unit) -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1352:bool) (xs1_1353:int) ->
         (if xs0_1352
           (l0
             (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
               (fun (x__1389:unit) -> (k_append_1119 x__1389))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1366:bool) (xs1_1367:int) ->
                (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))));;
  fail_1301 b k -> {fail} => (k ());;

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
              (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0)))) (
             fun (main_1345:unit) -> end)) ===> (append_1008
                                                  (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
                                                   (if b_1369 (l0 (k_main_1017_1370 false 0))
                                                     (l1 (k_main_1017_1370 true 0)))) (
                                                  fun (main_1345:unit) -> end))
main_1294:: (append_1008
              (fun (b_1369:bool) (k_main_1017_1370:(bool -> int -> X)) ->
               (if b_1369 (l0 (k_main_1017_1370 false 0)) (l1 (k_main_1017_1370 true 0))))
              (fun (main_1345:unit) -> end))
abst_arg: main_1345, unit;;
abst_arg: main_1345, unit;;
abst_arg: b_1369, bool;;
abst_arg: k_main_1017_1370, (bool -> int -> X);;
abst_arg: b_1369, bool;;
abst_arg: k_main_1017_1370, (bool -> int -> X);;
append_1008: ENV: xs__ys_1009:(bool -> (bool -> int -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (bool -> (bool -> int -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
abst_arg: xs__ys_1009, (bool -> (bool -> int -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
append_1008: (xs__ys_1009 true
               (fun (xs0_1352:bool) (xs1_1353:int) ->
                (if xs0_1352
                  (l0
                    (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                      (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1366:bool) (xs1_1367:int) ->
                       (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1352:bool) (xs1_1353:int) ->
  (if xs0_1352
    (l0
      (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
        (fun (x__1389:unit) -> (k_append_1119 x__1389))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1366:bool) (xs1_1367:int) ->
         (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1352:bool) (xs1_1353:int) ->
                 (if xs0_1352
                   (l0
                     (append_1008 (fun (x_1356:bool) (k_append_1357:(bool -> int -> X)) -> (k_append_1357 true 0))
                       (fun (x__1389:unit) -> (k_append_1119 x__1389))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1366:bool) (xs1_1367:int) ->
                        (if (xs1_1367 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__1388:unit) -> (k_append_1119 x__1388)))))))))))
abst_arg: xs0_1352, bool;;
abst_arg: xs1_1353, int;;
abst_arg: xs0_1352, bool;;
abst_arg: xs1_1353, int;;
abst_arg: x__1389, unit;;
abst_arg: x__1389, unit;;
abst_arg: x_1356, bool;;
abst_arg: k_append_1357, (bool -> int -> X);;
abst_arg: x_1356, bool;;
abst_arg: k_append_1357, (bool -> int -> X);;
abst_arg: xs0_1366, bool;;
abst_arg: xs1_1367, int;;
abst_arg: xs0_1366, bool;;
abst_arg: xs1_1367, int;;
abst_arg: x__1388, unit;;
abst_arg: x__1388, unit;;
fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
ABST:
Main: main_1294
  main_1294 -> (append_1008 (fun k_main_1017_1370 -> (if rand_bool (l0 k_main_1017_1370) (l1 k_main_1017_1370))) ());;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (if rand_bool (l0 (append_1008 (fun k_append_1357 -> k_append_1357) k_append_1119))
          (l1 (xs__ys_1009 (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119)))))));;
  fail_1301 k -> {fail} => k;;
Types:
  main_1294 : unit
  append_1008 : ((unit -> unit) -> unit -> unit)
  fail_1301 : (unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ());;
  f_1391 k_main_1017_1370 -> (if rand_bool (l0 k_main_1017_1370) (l1 k_main_1017_1370));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (if rand_bool (l0 (append_1008 f_1393 k_append_1119))
          (l1 (xs__ys_1009 (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119)))))));;
  f_1393 k_append_1357 -> k_append_1357;;
  fail_1301 k -> {fail} => k;;

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ());;
  f_1391 k_main_1017_1370 ->
      (let f_1395 b_1394 = (if b_1394 (l0 k_main_1017_1370) (l1 k_main_1017_1370)) in
       (if rand_bool (f_1395 true) (f_1395 false)));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (let f_1397 b_1396 =
         (if b_1396 (l0 (append_1008 f_1393 k_append_1119))
           (l1
             (xs__ys_1009
               (let f_1399 b_1398 = (if b_1398 (l0 k_append_1119) (l1 (fail_1301 k_append_1119))) in
                (if rand_bool (f_1399 true) (f_1399 false))))))
         in (if rand_bool (f_1397 true) (f_1397 false))));;
  f_1393 k_append_1357 -> k_append_1357;;
  fail_1301 k -> {fail} => k;;

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_1391 ());;
  f_1391 k_main_1017_1370 ->
      (let f_1395 b_1394 = (if b_1394 (l0 k_main_1017_1370) (l1 k_main_1017_1370)) in
       (if rand_bool (f_1395 true) (f_1395 false)));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (let f_1397 b_1396 =
         (if b_1396 (l0 (append_1008 f_1393 k_append_1119))
           (l1
             (xs__ys_1009
               (let f_1399 b_1398 = (if b_1398 (l0 k_append_1119) (l1 (fail_1301 k_append_1119))) in
                (if rand_bool (f_1399 true) (f_1399 false))))))
         in (if rand_bool (f_1397 true) (f_1397 false))));;
  f_1393 k_append_1357 -> k_append_1357;;
  fail_1301 k -> {fail} => k;;

DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_1294 ... --> 
  main_1072 ... --> 
  main_1017_1014 ... --> 
  append_1008 ... --> 
  xs_1102 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [1/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1295 [2/2] ... --> 
  xs_1103 ... --> 
  f_main_1017_1298 ... --> 
  mynot_1012 [1/2] ... --> 
  f_main_1017_1299 ... --> 
  f_append_1297 [2/2] ... --> 
  fail_1301 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0

Program with abstraction types (CEGAR-cycle 1)::
Main: main_1294
  main_1294 -> (main_1072 f_1300);;
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009));;
  f_1300 main_1261 -> end;;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119);;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119));;
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0);;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ());;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119);;
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285));;
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0);;
  fail_1301 b k -> {fail} => (k ());;
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208);;
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255);;
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false);;
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true);;
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126);;
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165);;
Types:
  main_1294 : X
  append_1008 : ((x_2:bool -> (bool -> int[x_2] -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(1-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_1669 k_main_1017_1670 -> (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
        (fun main_1645 -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1652 xs1_1653 ->
         (if xs0_1652 (l0 (append_1008 (fun x_1656 k_append_1657 -> (k_append_1657 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1666 xs1_1667 ->
                (if (xs1_1667 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))));;
  fail_1301 b k -> {fail} => (k ());;

ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
         (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))) (
        fun (main_1645:unit) -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
         (if xs0_1652
           (l0
             (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
               (fun (x__1689:unit) -> (k_append_1119 x__1689))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))));;
  fail_1301 b k -> {fail} => (k ());;

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
              (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0)))) (
             fun (main_1645:unit) -> end)) ===> (append_1008
                                                  (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
                                                   (if b_1669 (l0 (k_main_1017_1670 false 0))
                                                     (l1 (k_main_1017_1670 true 0)))) (
                                                  fun (main_1645:unit) -> end))
main_1294:: (append_1008
              (fun (b_1669:bool) (k_main_1017_1670:(bool -> int[b_1669] -> X)) ->
               (if b_1669 (l0 (k_main_1017_1670 false 0)) (l1 (k_main_1017_1670 true 0))))
              (fun (main_1645:unit) -> end))
abst_arg: main_1645, unit;;
abst_arg: main_1645, unit;;
abst_arg: b_1669, bool;;
abst_arg: k_main_1017_1670, (bool -> int[b_1669] -> X);;
abst_arg: b_1669, bool;;
abst_arg: k_main_1017_1670, (bool -> int[b_1669] -> X);;
append_1008: ENV: xs__ys_1009:(x_1:bool -> (bool -> int[x_1] -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
append_1008: (xs__ys_1009 true
               (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                (if xs0_1652
                  (l0
                    (append_1008
                      (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                      (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                       (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
  (if xs0_1652
    (l0
      (append_1008 (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
        (fun (x__1689:unit) -> (k_append_1119 x__1689))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
         (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1652:bool) (xs1_1653:int[true]) ->
                 (if xs0_1652
                   (l0
                     (append_1008
                       (fun (x_1656:bool) (k_append_1657:(bool -> int[x_1656] -> X)) -> (k_append_1657 true 0))
                       (fun (x__1689:unit) -> (k_append_1119 x__1689))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1666:bool) (xs1_1667:int[false]) ->
                        (if (xs1_1667 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__1688:unit) -> (k_append_1119 x__1688)))))))))))
abst_arg: xs0_1652, bool;;
abst_arg: xs1_1653, int[true];;
abst_arg: xs0_1652, bool;;
abst_arg: xs1_1653, int[true];;
abst_arg: x__1689, unit;;
abst_arg: x__1689, unit;;
abst_arg: x_1656, bool;;
abst_arg: k_append_1657, (bool -> int[x_1656] -> X);;
abst_arg: x_1656, bool;;
abst_arg: k_append_1657, (bool -> int[x_1656] -> X);;
abst_arg: xs0_1666, bool;;
abst_arg: xs1_1667, int[false];;
abst_arg: xs0_1666, bool;;
abst_arg: xs1_1667, int[false];;
abst_arg: x__1688, unit;;
abst_arg: x__1688, unit;;
fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
ABST:
Main: main_1294
  main_1294 ->
      (append_1008 (fun k_main_1017_1670 -> (if rand_bool (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))))
        ());;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (fun xs1_1653 ->
         (if rand_bool (l0 (append_1008 (fun k_append_1657 -> (k_append_1657 rand_bool)) k_append_1119))
           (l1 (xs__ys_1009 (fun xs1_1667 -> (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119)))))))));;
  fail_1301 k -> {fail} => k;;
Types:
  main_1294 : unit
  append_1008 : (((bool -> unit) -> unit) -> unit -> unit)
  fail_1301 : (unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ());;
  f_1691 k_main_1017_1670 -> (if rand_bool (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119));;
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (if rand_bool (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694))));;
  f_1697 k_append_1657 -> (k_append_1657 rand_bool);;
  f_1700 k_append_1699 xs1_1667 -> (if rand_bool (l0 k_append_1699) (l1 (fail_1301 k_append_1699)));;
  fail_1301 k -> {fail} => k;;

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ());;
  f_1691 k_main_1017_1670 ->
      (let f_1702 b_1701 = (if b_1701 (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))) in
       (if rand_bool (f_1702 true) (f_1702 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119));;
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (let f_1704 b_1703 =
       (if b_1703 (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694)))) in
       (if rand_bool (f_1704 true) (f_1704 false)));;
  f_1697 k_append_1657 -> (if rand_bool (k_append_1657 true) (k_append_1657 false));;
  f_1700 k_append_1699 xs1_1667 ->
      (let f_1708 b_1707 = (if b_1707 (l0 k_append_1699) (l1 (fail_1301 k_append_1699))) in
       (if rand_bool (f_1708 true) (f_1708 false)));;
  fail_1301 k -> {fail} => k;;

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_1691 ());;
  f_1691 k_main_1017_1670 ->
      (let f_1702 b_1701 = (if b_1701 (l0 (k_main_1017_1670 true)) (l1 (k_main_1017_1670 false))) in
       (if rand_bool (f_1702 true) (f_1702 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1695 xs__ys_1009 k_append_1119));;
  f_1695 xs__ys_1693 k_append_1694 xs1_1653 ->
      (let f_1704 b_1703 =
       (if b_1703 (l0 (append_1008 f_1697 k_append_1694)) (l1 (xs__ys_1693 (f_1700 k_append_1694)))) in
       (if rand_bool (f_1704 true) (f_1704 false)));;
  f_1697 k_append_1657 -> (if rand_bool (k_append_1657 true) (k_append_1657 false));;
  f_1700 k_append_1699 xs1_1667 ->
      (let f_1708 b_1707 = (if b_1707 (l0 k_append_1699) (l1 (fail_1301 k_append_1699))) in
       (if rand_bool (f_1708 true) (f_1708 false)));;
  fail_1301 k -> {fail} => k;;

DONE!

(1-2) Checking HORS ... DONE!

Filter option enabled.
Restart CEGAR-loop.
Program with abstraction types (CEGAR-cycle 2)::
Main: main_1294
  main_1294 -> (main_1072 f_1300);;
  append_1008 xs__ys_1009 k_append_1119 -> (xs_1102 xs__ys_1009 (f_append_1295 k_append_1119 xs__ys_1009));;
  f_1300 main_1261 -> end;;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when xs0_1187 ->
      (append_1008 (f_append_1296 xs0_1187 xs1_1187) k_append_1119);;
  f_append_1295 k_append_1119 xs__ys_1009 xs0_1187 xs1_1187 when (not xs0_1187) ->
      (xs_1103 xs0_1187 xs1_1187 xs__ys_1009 (f_append_1297 xs0_1187 xs1_1187 k_append_1119));;
  f_append_1296 xs0_1187 xs1_1187 x_1279 k_append_1280 -> (k_append_1280 true 0);;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (xs1_1182 = 0) -> (k_append_1119 ());;
  f_append_1297 xs0_1187 xs1_1187 k_append_1119 xs0_1182 xs1_1182 when (
      not (xs1_1182 = 0)) -> (fail_1301 true k_append_1119);;
  f_main_1017_1298 x_1056 b_1284 k_main_1017_1285 ->
      (mynot_1012 b_1284 (f_main_1017_1299 b_1284 x_1056 k_main_1017_1285));;
  f_main_1017_1299 b_1284 x_1056 k_main_1017_1285 x_1292 -> (k_main_1017_1285 x_1292 0);;
  fail_1301 b k -> {fail} => (k ());;
  main_1017_1014 x_1056 k_main_1017_1208 -> (append_1008 (f_main_1017_1298 x_1056) k_main_1017_1208);;
  main_1072 k_main_1255 -> (main_1017_1014 () k_main_1255);;
  mynot_1012 b_1013 k_mynot_1197 when b_1013 -> (k_mynot_1197 false);;
  mynot_1012 b_1013 k_mynot_1197 when (not b_1013) -> (k_mynot_1197 true);;
  xs_1102 xs__ys_1009 k_append_xs_1126 -> (xs__ys_1009 true k_append_xs_1126);;
  xs_1103 xs0_1187 xs1_1187 xs__ys_1009 k_append_xs_1165 -> (xs__ys_1009 false k_append_xs_1165);;
Types:
  main_1294 : X
  append_1008 : ((x_2:bool -> (bool -> int[x_2] -> X) -> X) -> (unit -> X) -> X)
  fail_1301 : (bool -> (unit -> X) -> X)

(2-1) Abstracting ... EXPAND_NONREC:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun b_1783 k_main_1017_1784 -> (if b_1783 (l0 (k_main_1017_1784 false 0)) (l1 (k_main_1017_1784 true 0))))
        (fun main_1759 -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun xs0_1766 xs1_1767 ->
         (if xs0_1766 (l0 (append_1008 (fun x_1770 k_append_1771 -> (k_append_1771 true 0)) k_append_1119))
           (l1
             (xs__ys_1009 false
               (fun xs0_1780 xs1_1781 ->
                (if (xs1_1781 = 0) (l0 (k_append_1119 ())) (l1 (fail_1301 true k_append_1119)))))))));;
  fail_1301 b k -> {fail} => (k ());;

ETA_EXPAND:
Main: main_1294
  main_1294 ->
      (append_1008
        (fun (b_1783:bool) (k_main_1017_1784:(bool -> int[b_1783] -> X)) ->
         (if b_1783 (l0 (k_main_1017_1784 false 0)) (l1 (k_main_1017_1784 true 0)))) (
        fun (main_1759:unit) -> end));;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009 true
        (fun (xs0_1766:bool) (xs1_1767:int[true]) ->
         (if xs0_1766
           (l0
             (append_1008 (fun (x_1770:bool) (k_append_1771:(bool -> int[x_1770] -> X)) -> (k_append_1771 true 0))
               (fun (x__1803:unit) -> (k_append_1119 x__1803))))
           (l1
             (xs__ys_1009 false
               (fun (xs0_1780:bool) (xs1_1781:int[false]) ->
                (if (xs1_1781 = 0) (l0 (k_append_1119 ()))
                  (l1 (fail_1301 true (fun (x__1802:unit) -> (k_append_1119 x__1802)))))))))));;
  fail_1301 b k -> {fail} => (k ());;

main_1294: ENV: 

main_1294: (append_1008
             (fun (b_1783:bool) (k_main_1017_1784:(bool -> int[b_1783] -> X)) ->
              (if b_1783 (l0 (k_main_1017_1784 false 0)) (l1 (k_main_1017_1784 true 0)))) (
             fun (main_1759:unit) -> end)) ===> (append_1008
                                                  (fun (b_1783:bool) (k_main_1017_1784:(bool -> int[b_1783] -> X)) ->
                                                   (if b_1783 (l0 (k_main_1017_1784 false 0))
                                                     (l1 (k_main_1017_1784 true 0)))) (
                                                  fun (main_1759:unit) -> end))
main_1294:: (append_1008
              (fun (b_1783:bool) (k_main_1017_1784:(bool -> int[b_1783] -> X)) ->
               (if b_1783 (l0 (k_main_1017_1784 false 0)) (l1 (k_main_1017_1784 true 0))))
              (fun (main_1759:unit) -> end))
abst_arg: main_1759, unit;;
abst_arg: main_1759, unit;;
abst_arg: b_1783, bool;;
abst_arg: k_main_1017_1784, (bool -> int[b_1783] -> X);;
abst_arg: b_1783, bool;;
abst_arg: k_main_1017_1784, (bool -> int[b_1783] -> X);;
append_1008: ENV: xs__ys_1009:(x_1:bool -> (bool -> int[x_1] -> X) -> X), k_append_1119:(unit -> X),


abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
abst_arg: xs__ys_1009, (x_1:bool -> (bool -> int[x_1] -> X) -> X);;
abst_arg: k_append_1119, (unit ->
X);;
append_1008: (xs__ys_1009 true
               (fun (xs0_1766:bool) (xs1_1767:int[true]) ->
                (if xs0_1766
                  (l0
                    (append_1008
                      (fun (x_1770:bool) (k_append_1771:(bool -> int[x_1770] -> X)) -> (k_append_1771 true 0))
                      (fun (x__1803:unit) -> (k_append_1119 x__1803))))
                  (l1
                    (xs__ys_1009 false
                      (fun (xs0_1780:bool) (xs1_1781:int[false]) ->
                       (if (xs1_1781 = 0) (l0 (k_append_1119 ()))
                         (l1 (fail_1301 true (fun (x__1802:unit) -> (k_append_1119 x__1802))))))))))) ===> (
xs__ys_1009 true
 (fun (xs0_1766:bool) (xs1_1767:int[true]) ->
  (if xs0_1766
    (l0
      (append_1008 (fun (x_1770:bool) (k_append_1771:(bool -> int[x_1770] -> X)) -> (k_append_1771 true 0))
        (fun (x__1803:unit) -> (k_append_1119 x__1803))))
    (l1
      (xs__ys_1009 false
        (fun (xs0_1780:bool) (xs1_1781:int[false]) ->
         (if (xs1_1781 = 0) (l0 (k_append_1119 ()))
           (l1 (fail_1301 true (fun (x__1802:unit) -> (k_append_1119 x__1802)))))))))))
append_1008:: (xs__ys_1009 true
                (fun (xs0_1766:bool) (xs1_1767:int[true]) ->
                 (if xs0_1766
                   (l0
                     (append_1008
                       (fun (x_1770:bool) (k_append_1771:(bool -> int[x_1770] -> X)) -> (k_append_1771 true 0))
                       (fun (x__1803:unit) -> (k_append_1119 x__1803))))
                   (l1
                     (xs__ys_1009 false
                       (fun (xs0_1780:bool) (xs1_1781:int[false]) ->
                        (if (xs1_1781 = 0) (l0 (k_append_1119 ()))
                          (l1 (fail_1301 true (fun (x__1802:unit) -> (k_append_1119 x__1802)))))))))))
abst_arg: xs0_1766, bool;;
abst_arg: xs1_1767, int[true];;
abst_arg: xs0_1766, bool;;
abst_arg: xs1_1767, int[true];;
abst_arg: x__1803, unit;;
abst_arg: x__1803, unit;;
abst_arg: x_1770, bool;;
abst_arg: k_append_1771, (bool -> int[x_1770] -> X);;
abst_arg: x_1770, bool;;
abst_arg: k_append_1771, (bool -> int[x_1770] -> X);;
abst_arg: xs0_1780, bool;;
abst_arg: xs1_1781, int[false];;
abst_arg: xs0_1780, bool;;
abst_arg: xs1_1781, int[false];;
abst_arg: x__1802, unit;;
abst_arg: x__1802, unit;;
fail_1301: ENV: b:bool, k:(unit -> X),


abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
abst_arg: b, bool;;
abst_arg: k, (unit ->
X);;
fail_1301: (k ()) ===> (k ())
fail_1301:: (k ())
ABST:
Main: main_1294
  main_1294 ->
      (append_1008 (fun k_main_1017_1784 -> (if rand_bool (l0 (k_main_1017_1784 true)) (l1 (k_main_1017_1784 false))))
        ());;
  append_1008 xs__ys_1009 k_append_1119 ->
      (xs__ys_1009
        (fun xs1_1767 ->
         (if rand_bool (l0 (append_1008 (fun k_append_1771 -> (k_append_1771 rand_bool)) k_append_1119))
           (l1 (xs__ys_1009 (fun xs1_1781 -> (if rand_bool (l0 k_append_1119) (l1 (fail_1301 k_append_1119)))))))));;
  fail_1301 k -> {fail} => k;;
Types:
  main_1294 : unit
  append_1008 : (((bool -> unit) -> unit) -> unit -> unit)
  fail_1301 : (unit -> unit)

LIFT:
Main: main_1294
  main_1294 -> (append_1008 f_1805 ());;
  f_1805 k_main_1017_1784 -> (if rand_bool (l0 (k_main_1017_1784 true)) (l1 (k_main_1017_1784 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1809 xs__ys_1009 k_append_1119));;
  f_1809 xs__ys_1807 k_append_1808 xs1_1767 ->
      (if rand_bool (l0 (append_1008 f_1811 k_append_1808)) (l1 (xs__ys_1807 (f_1814 k_append_1808))));;
  f_1811 k_append_1771 -> (k_append_1771 rand_bool);;
  f_1814 k_append_1813 xs1_1781 -> (if rand_bool (l0 k_append_1813) (l1 (fail_1301 k_append_1813)));;
  fail_1301 k -> {fail} => k;;

TRANS_EAGER:
Main: main_1294
  main_1294 -> (append_1008 f_1805 ());;
  f_1805 k_main_1017_1784 ->
      (let f_1816 b_1815 = (if b_1815 (l0 (k_main_1017_1784 true)) (l1 (k_main_1017_1784 false))) in
       (if rand_bool (f_1816 true) (f_1816 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1809 xs__ys_1009 k_append_1119));;
  f_1809 xs__ys_1807 k_append_1808 xs1_1767 ->
      (let f_1818 b_1817 =
       (if b_1817 (l0 (append_1008 f_1811 k_append_1808)) (l1 (xs__ys_1807 (f_1814 k_append_1808)))) in
       (if rand_bool (f_1818 true) (f_1818 false)));;
  f_1811 k_append_1771 -> (if rand_bool (k_append_1771 true) (k_append_1771 false));;
  f_1814 k_append_1813 xs1_1781 ->
      (let f_1822 b_1821 = (if b_1821 (l0 k_append_1813) (l1 (fail_1301 k_append_1813))) in
       (if rand_bool (f_1822 true) (f_1822 false)));;
  fail_1301 k -> {fail} => k;;

PUT_INTO_IF:
Main: main_1294
  main_1294 -> (append_1008 f_1805 ());;
  f_1805 k_main_1017_1784 ->
      (let f_1816 b_1815 = (if b_1815 (l0 (k_main_1017_1784 true)) (l1 (k_main_1017_1784 false))) in
       (if rand_bool (f_1816 true) (f_1816 false)));;
  append_1008 xs__ys_1009 k_append_1119 -> (xs__ys_1009 (f_1809 xs__ys_1009 k_append_1119));;
  f_1809 xs__ys_1807 k_append_1808 xs1_1767 ->
      (let f_1818 b_1817 =
       (if b_1817 (l0 (append_1008 f_1811 k_append_1808)) (l1 (xs__ys_1807 (f_1814 k_append_1808)))) in
       (if rand_bool (f_1818 true) (f_1818 false)));;
  f_1811 k_append_1771 -> (if rand_bool (k_append_1771 true) (k_append_1771 false));;
  f_1814 k_append_1813 xs1_1781 ->
      (let f_1822 b_1821 = (if b_1821 (l0 k_append_1813) (l1 (fail_1301 k_append_1813))) in
       (if rand_bool (f_1822 true) (f_1822 false)));;
  fail_1301 k -> {fail} => k;;

DONE!

Verification failed (new error path not found)
