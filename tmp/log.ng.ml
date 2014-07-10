MoCHi: Model Checker for Higher-Order Programs
  Build: _438a443 (after 2014-07-10 12:53:16 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil_nth.ml -debug-module Tupling -disable-rc -color -tupling -gchi 
           -list-option -abs-remove-false

parsed:
 let rec append_1008 xs_1009 ys_1010 =
   match xs_1009 with
   | [] -> ys_1010
   | x_1011::xs'_1012 -> x_1011::append_1008 xs'_1012 ys_1010
 in
 let rec make_list_1013 n_1014 = if n_1014 < 0 then
                                   []
                                 else
                                   rand_int ()::make_list_1013 (n_1014 - 1) in
 let main_1015 i_1016 n_1017 =
   let xs_1018 = make_list_1013 n_1017 in
   let ys_1019 = append_1008 [] xs_1018 in
   if List.nth ys_1019 i_1016 = List.nth xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 ()

set_target:
 let rec append_1008 (xs_1009:!!! list) (ys_1010:!!! list) =
   match xs_1009 with
   | [] -> ys_1010
   | x_1011::xs'_1012 -> x_1011::append_1008 xs'_1012 ys_1010
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1013 n_1017 in
   let ys_1019 = append_1008 [] xs_1018 in
   if List.nth ys_1019 i_1016 = List.nth xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1049 = let arg1_1045 = rand_int () in
                 let arg2_1047 = rand_int () in
                 main_1015 arg1_1045 arg2_1047 in
 ()

make_ext_funs:
 let List.nth_1050 (x_1051:int list) (x_1052:int) = rand_int () in
 let rec append_1008 (xs_1009:!!! list) (ys_1010:!!! list) =
   match xs_1009 with
   | [] -> ys_1010
   | x_1011::xs'_1012 -> x_1011::append_1008 xs'_1012 ys_1010
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1013 n_1017 in
   let ys_1019 = append_1008 [] xs_1018 in
   if List.nth_1050 ys_1019 i_1016 = List.nth_1050 xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1049 = let arg1_1045 = rand_int () in
                 let arg2_1047 = rand_int () in
                 main_1015 arg1_1045 arg2_1047 in
 ()

copy_poly:
 let List.nth_1050 (x_1051:int list) (x_1052:int) = rand_int () in
 let rec append_1053 (xs_1009:int list) (ys_1010:int list) =
   match xs_1009 with
   | [] -> ys_1010
   | x_1011::xs'_1012 -> x_1011::append_1053 xs'_1012 ys_1010
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1013 n_1017 in
   let ys_1019 = append_1053 [] xs_1018 in
   if List.nth_1050 ys_1019 i_1016 = List.nth_1050 xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1049 = let arg1_1045 = rand_int () in
                 let arg2_1047 = rand_int () in
                 main_1015 arg1_1045 arg2_1047 in
 ()

encode_list:
 let List.nth_1050 (x_1051:(int -> (bool * int))) (x_1052:int) = rand_int () in
 let rec append_1053 (xs_1009:(int -> (bool * int))) (ys_1010:(int -> (bool * int))) =
   if fst (xs_1009 0) = false then
     ys_1010
   else
     if fst (xs_1009 0) <> false then
       let xs'_1012 (x_1087:int) = xs_1009 (x_1087 + 1) in
       let x_1011 = snd (xs_1009 0) in
       let cons_1150 (x_1146:int) (xs_1147:(int -> (bool * int))) (i_1145:int) =
         if i_1145 = 0 then
           (true, x_1146)
         else
           xs_1147 (i_1145 - 1)
       in
       cons_1150 x_1011 (append_1053 xs'_1012 ys_1010)
     else
       _|_
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1221:int) -> (false, 0)
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> (bool * int))) (i_1210:int) =
       if i_1210 = 0 then
         (true, x_1211)
       else
         xs_1212 (i_1210 - 1)
     in
     cons_1215 (rand_int ()) (make_list_1013 (n_1014 - 1))
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1013 n_1017 in
   let ys_1019 = append_1053 (fun (x_1254:int) -> (false, 0)) xs_1018 in
   if (let x_1313 = ys_1019 i_1016 in
       if fst x_1313 <> false then
         snd x_1313
       else
         _|_)
      = (let x_1303 = xs_1018 i_1016 in
         if fst x_1303 <> false then
           snd x_1303
         else
           _|_) then
     ()
   else
     {fail} ()
 in
 let main_1049 = let arg1_1045 = rand_int () in
                 let arg2_1047 = rand_int () in
                 main_1015 arg1_1045 arg2_1047 in
 ()

ret_fun:
 let List.nth_1050 (x_1051:(int -> (bool * int))) =
   ((fun (x_1052:int) -> (let f_1317 = rand_int in
                          let n_1319 = f_1317 () in
                          n_1319)), x_1051)
 in
 let rec append_1053 (xs_1009:(int -> (bool * int))) =
   ((fun (ys_1010:(int -> (bool * int))) ->
       (let p_1323 = xs_1009 0 in
        let b_1325 = fst p_1323 in
        let b_1320 = b_1325 = false in
        if b_1320 then
          (ys_1010, ys_1010)
        else
          let p_1330 = xs_1009 0 in
          let b_1332 = fst p_1330 in
          let b_1334 = b_1332 = false in
          let b_1327 = not b_1334 in
          if b_1327 then
            let xs'_1012 (x_1087:int) =
              let n_1338 = x_1087 + 1 in
              let p_1339 = xs_1009 n_1338 in
              let xs_1453 (n_1454:int) = if n_1454 = n_1338 then
                                           p_1339
                                         else
                                           xs_1009 n_1454 in
              p_1339
            in
            let p_1342 = xs_1009 0 in
            let x_1011 = snd p_1342 in
            let cons_1150 (x_1146:int) (xs_1147:(int -> (bool * int))) =
              ((fun (i_1145:int) ->
                  (let b_1344 = i_1145 = 0 in
                   if b_1344 then
                     (true, x_1146)
                   else
                     let n_1354 = i_1145 - 1 in
                     let p_1355 = xs_1147 n_1354 in
                     let xs_1459 (n_1460:int) = if n_1460 = n_1354 then
                                                  p_1355
                                                else
                                                  xs_1147 n_1460 in
                     p_1355)),
               xs_1147)
            in
            let p_1482 = append_1053 xs'_1012 in
            let f_1359 = fst p_1482 in
            let xs'_1483 = snd p_1482 in
            let p_1478 = f_1359 ys_1010 in
            let f_1360 = fst p_1478 in
            let ys_1479 = snd p_1478 in
            let f_1364 = cons_1150 x_1011 in
            let p_1474 = f_1364 f_1360 in
            let f_1365 = fst p_1474 in
            let f_1475 = snd p_1474 in
            (f_1365, ys_1479)
          else
            (_|_, ys_1010))),
    xs_1009)
 in
 let rec make_list_1013 (n_1014:int) =
   let b_1368 = n_1014 < 0 in
   if b_1368 then
     fun (x_1221:int) -> (false, 0)
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> (bool * int))) =
       ((fun (i_1210:int) ->
           (let b_1375 = i_1210 = 0 in
            if b_1375 then
              (true, x_1211)
            else
              let n_1385 = i_1210 - 1 in
              let p_1386 = xs_1212 n_1385 in
              let xs_1500 (n_1501:int) = if n_1501 = n_1385 then
                                           p_1386
                                         else
                                           xs_1212 n_1501 in
              p_1386)),
        xs_1212)
     in
     let n_1393 = n_1014 - 1 in
     let f_1394 = make_list_1013 n_1393 in
     let f_1387 = rand_int in
     let n_1389 = f_1387 () in
     let f_1398 = cons_1215 n_1389 in
     let p_1511 = f_1398 f_1394 in
     let f_1399 = fst p_1511 in
     let f_1512 = snd p_1511 in
     f_1399
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let f_1403 = make_list_1013 n_1017 in
   let f_1409 (x_1254:int) = (false, 0) in
   let p_1523 = append_1053 f_1409 in
   let f_1411 = fst p_1523 in
   let f_1524 = snd p_1523 in
   let p_1519 = f_1411 f_1403 in
   let f_1412 = fst p_1519 in
   let xs_1520 = snd p_1519 in
   let p_1416 = f_1412 i_1016 in
   let b_1419 = fst p_1416 in
   let b_1421 = b_1419 = false in
   let b_1417 = not b_1421 in
   let n_1432 = if b_1417 then
                  snd p_1416
                else
                  _|_ in
   let p_1425 = xs_1520 i_1016 in
   let b_1428 = fst p_1425 in
   let b_1430 = b_1428 = false in
   let b_1426 = not b_1430 in
   let n_1433 = if b_1426 then
                  snd p_1425
                else
                  _|_ in
   let b_1413 = n_1432 = n_1433 in
   if b_1413 then
     ()
   else
     let f_1434 = {fail} in
     let u_1436 = f_1434 () in
     u_1436
 in
 let f_1437 = rand_int in
 let n_1439 = f_1437 () in
 let f_1440 = rand_int in
 let n_1442 = f_1440 () in
 let f_1446 = main_1015 n_1439 in
 let u_1447 = f_1446 n_1442 in
 ()

ref_trans:
 let List.nth_1050 (x_1051:(int -> (bool * int))) =
   let x_1537 (x_1052:int) = rand_int () in
   let x_1541 (xi_3146:((bool * int) * (bool * int))) =
     ((if fst (fst xi_3146) = false then
         (false, 0)
       else
         (true, x_1537 (snd (fst xi_3146)))),
      (if fst (snd xi_3146) = false then
         (false, (true, 0))
       else
         (true, x_1051 (snd (snd xi_3146)))))
   in
   x_1541
 in
 let rec append_1053 (xs_1009:(int -> (bool * int))) =
   let x_1542 (ys_1010:(int -> (bool * int))) =
     let x_1543 = xs_1009 0 in
     if fst x_1543 = false then
       let x_1592 (ii_3021:((bool * int) * (bool * int))) =
         if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
           let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
           (r_3038, r_3038)
         else
           ((if fst (fst ii_3021) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (fst ii_3021)))),
            (if fst (snd ii_3021) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (snd ii_3021)))))
       in
       x_1592
     else
       let x_1547 = xs_1009 0 in
       if fst x_1547 <> false then
         let xs'_1012 (x_1087:int) =
           let x_1557 = xs_1009 (x_1087 + 1) in
           let xs_1453 (n_1454:int) = if n_1454 = x_1087 + 1 then
                                        x_1557
                                      else
                                        xs_1009 n_1454 in
           x_1557
         in
         let x_1560 = xs_1009 0 in
         let cons_1150 (x_1146:int) (xs_1147:(int -> (bool * int))) =
           let x_1562 (i_1145:int) =
             if i_1145 = 0 then
               (true, x_1146)
             else
               let x_1567 = xs_1147 (i_1145 - 1) in
               let xs_1459 (n_1460:int) = if n_1460 = i_1145 - 1 then
                                            x_1567
                                          else
                                            xs_1147 n_1460 in
               x_1567
           in
           let x_1576 (ii_2933:((bool * int) * (bool * int))) =
             ((if fst (fst ii_2933) = false then
                 (false, (true, 0))
               else
                 (true, x_1562 (snd (fst ii_2933)))),
              (if fst (snd ii_2933) = false then
                 (false, (true, 0))
               else
                 (true, xs_1147 (snd (snd ii_2933)))))
           in
           x_1576
         in
         let x_1577 = append_1053 xs'_1012 in
         let x_1578 (x_2860:(int -> (bool * int))) = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
         let x_1579 (i_2851:int) = snd (snd (x_1577 ((false, (fun (i_1141:int) -> (true, 0))), (true, i_2851)))) in
         let x_1580 = let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
                      snd (fst x_2850) in
         let x_1581 (i_2810:int) = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
         let x_1582 (i_2803:int) = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
         let x_1583 = cons_1150 (snd x_1560) in
         let x_1584 = x_1583 x_1581 in
         let x_1585 (i_2794:int) = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
         let x_1586 (i_2787:int) = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
         let x_1589 (ii_2770:((bool * int) * (bool * int))) =
           ((if fst (fst ii_2770) = false then
               (false, (true, 0))
             else
               (true, x_1585 (snd (fst ii_2770)))),
            (if fst (snd ii_2770) = false then
               (false, (true, 0))
             else
               (true, x_1582 (snd (snd ii_2770)))))
         in
         x_1589
       else
         let x_1551 = _|_ in
         let x_1554 (ii_2428:((bool * int) * (bool * int))) =
           ((if fst (fst ii_2428) = false then
               (false, (true, 0))
             else
               (true, x_1551 (snd (fst ii_2428)))),
            (if fst (snd ii_2428) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (snd ii_2428)))))
         in
         x_1554
   in
   let x_1595 (ysi_2272:((bool * (int -> (bool * int))) * (bool * int))) =
     ((if fst (fst ysi_2272) = false then
         (false, (fun (ii_2193:((bool * int) * (bool * int))) -> ((true, (true, 0)), (true, (true, 0)))))
       else
         (true, x_1542 (snd (fst ysi_2272)))),
      (if fst (snd ysi_2272) = false then
         (false, (true, 0))
       else
         (true, xs_1009 (snd (snd ysi_2272)))))
   in
   x_1595
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1221:int) -> (false, 0)
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> (bool * int))) =
       let x_1598 (i_1210:int) =
         if i_1210 = 0 then
           (true, x_1211)
         else
           let x_1603 = xs_1212 (i_1210 - 1) in
           let xs_1500 (n_1501:int) = if n_1501 = i_1210 - 1 then
                                        x_1603
                                      else
                                        xs_1212 n_1501 in
           x_1603
       in
       let x_1612 (ii_2097:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2097) = false then
             (false, (true, 0))
           else
             (true, x_1598 (snd (fst ii_2097)))),
          (if fst (snd ii_2097) = false then
             (false, (true, 0))
           else
             (true, xs_1212 (snd (snd ii_2097)))))
       in
       x_1612
     in
     let x_1615 = make_list_1013 (n_1014 - 1) in
     let x_1616 = rand_int () in
     let x_1617 = cons_1215 x_1616 in
     let x_1618 = x_1617 x_1615 in
     let x_1619 (i_2021:int) = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
     let x_1620 (i_2014:int) = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
     x_1619
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_1626 = make_list_1013 n_1017 in
   let f_1409 (x_1254:int) = (false, 0) in
   let x_1632 = append_1053 f_1409 in
   let x_1633 (x_1927:(int -> (bool * int))) = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
   let x_1634 (i_1918:int) = snd (snd (x_1632 ((false, (fun (i_1287:int) -> (true, 0))), (true, i_1918)))) in
   let x_1635 = let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
                snd (fst x_1917) in
   let x_1636 (i_1877:int) = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
   let x_1637 (i_1870:int) = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
   let x_1638 = let x_1869 = x_1635 ((true, i_1016), (false, 0)) in
                snd (fst x_1869) in
   let n_1432 = if fst x_1638 <> false then
                  snd x_1638
                else
                  _|_ in
   let x_1643 = let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
                snd (snd x_1848) in
   let n_1433 = if fst x_1643 <> false then
                  snd x_1643
                else
                  _|_ in
   if n_1432 = n_1433 then
     ()
   else
     {fail} ()
 in
 let x_1650 = rand_int () in
 let x_1651 = rand_int () in
 let x_1652 = main_1015 x_1650 in
 let x_1653 = x_1652 x_1651 in
 ()

inline_wrapped:
let List.nth_1050 x_1051 =
  let x_1537 x_1052 = rand_int () in
  let x_1541 xi_3146 =
    if fst (fst xi_3146) = false then
      ((false, 0), (if fst (snd xi_3146) = false then
                      (false, (true, 0))
                    else
                      (true, x_1051 (snd (snd xi_3146)))))
    else
      if fst (snd xi_3146) = false then
        ((true, x_1537 (snd (fst xi_3146))), (false, (true, 0)))
      else
        ((true, x_1537 (snd (fst xi_3146))), (true, x_1051 (snd (snd xi_3146))))
  in
  x_1541
in
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_1580 = let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
                     snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1635 = let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
               snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1638 = let x_1869 = x_1635 ((true, i_1016), (false, 0)) in
               snd (fst x_1869) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1643 = let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
               snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

flatten_let:
let List.nth_1050 x_1051 =
  let x_1537 x_1052 = rand_int () in
  let x_1541 xi_3146 =
    if fst (fst xi_3146) = false then
      ((false, 0), (if fst (snd xi_3146) = false then
                      (false, (true, 0))
                    else
                      (true, x_1051 (snd (snd xi_3146)))))
    else
      if fst (snd xi_3146) = false then
        ((true, x_1537 (snd (fst xi_3146))), (false, (true, 0)))
      else
        ((true, x_1537 (snd (fst xi_3146))), (true, x_1051 (snd (snd xi_3146))))
  in
  x_1541
in
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
        let x_1580 = snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
  let x_1635 = snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1869 = x_1635 ((true, i_1016), (false, 0)) in
  let x_1638 = snd (fst x_1869) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
  let x_1643 = snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

NORMALIZE: n_1432
[x_1848]
NORMALIZE: x_1638
[x_1848]
normalize let:
let List.nth_1050 x_1051 =
  let x_1537 x_1052 = rand_int () in
  let x_1541 xi_3146 =
    if fst (fst xi_3146) = false then
      ((false, 0), (if fst (snd xi_3146) = false then
                      (false, (true, 0))
                    else
                      (true, x_1051 (snd (snd xi_3146)))))
    else
      if fst (snd xi_3146) = false then
        ((true, x_1537 (snd (fst xi_3146))), (false, (true, 0)))
      else
        ((true, x_1537 (snd (fst xi_3146))), (true, x_1051 (snd (snd xi_3146))))
  in
  x_1541
in
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
        let x_1580 = snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
  let x_1635 = snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1869 = x_1635 ((true, i_1016), (false, 0)) in
  let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
  let x_1638 = snd (fst x_1869) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1643 = snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_1650; is_subsumed: 
rand_int (), x_1652 x_1651; is_subsumed: make_list_1013 n_1017, append_1053 f_1409; is_subsumed: 
append_1053 f_1409, snd (fst x_1917); is_subsumed: make_list_1013 n_1017, 
snd (fst x_1917); is_subsumed: x_1632 ((true, x_1626), (false, 0)), x_1635 ((true, i_1016), (false, 0)); is_subsumed: 
append_1053 f_1409, x_1635 ((true, i_1016), (false, 0)); is_subsumed: 
make_list_1013 n_1017, x_1635 ((true, i_1016), (false, 0)); is_subsumed: 
x_1635 ((true, i_1016), (false, 0)), x_1635 ((true, i_1016), (true, i_1016)); x_1869 |-> x_1848
is_subsumed: x_1635 ((true, i_1016), (false, 0)), snd (fst x_1848); is_subsumed: 
snd (fst x_1917), snd (fst x_1848); is_subsumed: x_1632 ((true, x_1626), (false, 0)), 
snd (fst x_1848); is_subsumed: append_1053 f_1409, snd (fst x_1848); is_subsumed: 
make_list_1013 n_1017, snd (fst x_1848); is_subsumed: x_1635 ((true, i_1016), (true, i_1016)), 
if fst x_1638 <> false then
  snd x_1638
else
  _|_; is_subsumed: x_1635 ((true, i_1016), (false, 0)), if fst x_1638 <> false then
                                                           snd x_1638
                                                         else
                                                           _|_; is_subsumed: 
snd (fst x_1917), if fst x_1638 <> false then
                    snd x_1638
                  else
                    _|_; is_subsumed: x_1632 ((true, x_1626), (false, 0)), 
if fst x_1638 <> false then
  snd x_1638
else
  _|_; is_subsumed: append_1053 f_1409, if fst x_1638 <> false then
                                          snd x_1638
                                        else
                                          _|_; is_subsumed: make_list_1013 n_1017, 
if fst x_1638 <> false then
  snd x_1638
else
  _|_; is_subsumed: if fst x_1638 <> false then
                      snd x_1638
                    else
                      _|_, snd (snd x_1848); is_subsumed: snd (fst x_1848), 
snd (snd x_1848); is_subsumed: x_1635 ((true, i_1016), (false, 0)), snd (snd x_1848); is_subsumed: 
snd (fst x_1917), snd (snd x_1848); is_subsumed: x_1632 ((true, x_1626), (false, 0)), 
snd (snd x_1848); is_subsumed: append_1053 f_1409, snd (snd x_1848); is_subsumed: 
make_list_1013 n_1017, snd (snd x_1848); is_subsumed: if fst x_1638 <> false then
                                                        snd x_1638
                                                      else
                                                        _|_, if fst x_1643 <> false then
                                                               snd x_1643
                                                             else
                                                               _|_; is_subsumed: 
snd (fst x_1848), if fst x_1643 <> false then
                    snd x_1643
                  else
                    _|_; is_subsumed: x_1635 ((true, i_1016), (true, i_1016)), 
if fst x_1643 <> false then
  snd x_1643
else
  _|_; is_subsumed: x_1635 ((true, i_1016), (false, 0)), if fst x_1643 <> false then
                                                           snd x_1643
                                                         else
                                                           _|_; is_subsumed: 
snd (fst x_1917), if fst x_1643 <> false then
                    snd x_1643
                  else
                    _|_; is_subsumed: x_1632 ((true, x_1626), (false, 0)), 
if fst x_1643 <> false then
  snd x_1643
else
  _|_; is_subsumed: append_1053 f_1409, if fst x_1643 <> false then
                                          snd x_1643
                                        else
                                          _|_; is_subsumed: make_list_1013 n_1017, 
if fst x_1643 <> false then
  snd x_1643
else
  _|_; is_subsumed: make_list_1013 (n_1014 - 1), rand_int (); is_subsumed: 
make_list_1013 (n_1014 - 1), cons_1215 x_1616; is_subsumed: rand_int (), 
x_1617 x_1615; is_subsumed: xs_1009 0, xs_1009 0; x_1543 |-> x_1547
is_subsumed: xs_1009 0, _|_; is_subsumed: xs_1009 0, _|_; is_subsumed: 
xs_1009 0, xs_1009 0; x_1547 |-> x_1560
is_subsumed: xs_1009 0, append_1053 xs'_1012; is_subsumed: xs_1009 0, 
append_1053 xs'_1012; is_subsumed: xs_1009 0, append_1053 xs'_1012; is_subsumed: 
xs_1009 0, x_1577 ((true, ys_1010), (false, 0)); is_subsumed: xs_1009 0, 
x_1577 ((true, ys_1010), (false, 0)); is_subsumed: xs_1009 0, x_1577 ((true, ys_1010), (false, 0)); is_subsumed: 
append_1053 xs'_1012, snd (fst x_2850); is_subsumed: xs_1009 0, snd (fst x_2850); is_subsumed: 
xs_1009 0, snd (fst x_2850); is_subsumed: xs_1009 0, snd (fst x_2850); is_subsumed: 
snd (fst x_2850), cons_1150 (snd x_1560); is_subsumed: x_1577 ((true, ys_1010), (false, 0)), 
cons_1150 (snd x_1560); is_subsumed: append_1053 xs'_1012, cons_1150 (snd x_1560); is_subsumed: 
xs_1009 0, cons_1150 (snd x_1560); is_subsumed: xs_1009 0, cons_1150 (snd x_1560); is_subsumed: 
snd (fst x_2850), x_1583 x_1581; is_subsumed: x_1577 ((true, ys_1010), (false, 0)), 
x_1583 x_1581; is_subsumed: append_1053 xs'_1012, x_1583 x_1581; is_subsumed: 
xs_1009 0, x_1583 x_1581; is_subsumed: xs_1009 0, x_1583 x_1581; is_subsumed: 
xs_1009 0, x_1583 x_1581; is_subsumed: xs_1009 0, xs_1147 (i_1145 - 1); is_subsumed: 
xs_1009 0, xs_1147 (i_1145 - 1); is_subsumed: xs_1009 0, xs_1147 (i_1145 - 1); is_subsumed: 
xs_1009 0, xs_1009 (x_1087 + 1); is_subsumed: xs_1009 0, xs_1009 (x_1087 + 1); is_subsumed: 
xs_1009 0, (true, ys_1010 (snd (fst ii_3021))); x_1543; x_1547; x_1869
elim_same_app:
let List.nth_1050 x_1051 =
  let x_1537 x_1052 = rand_int () in
  let x_1541 xi_3146 =
    if fst (fst xi_3146) = false then
      ((false, 0), (if fst (snd xi_3146) = false then
                      (false, (true, 0))
                    else
                      (true, x_1051 (snd (snd xi_3146)))))
    else
      if fst (snd xi_3146) = false then
        ((true, x_1537 (snd (fst xi_3146))), (false, (true, 0)))
      else
        ((true, x_1537 (snd (fst xi_3146))), (true, x_1051 (snd (snd xi_3146))))
  in
  x_1541
in
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
        let x_1580 = snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
  let x_1635 = snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
  let x_1638 = snd (fst x_1848) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1643 = snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

elim_unused_branch:
let List.nth_1050 x_1051 =
  let x_1537 x_1052 = rand_int () in
  let x_1541 xi_3146 =
    if fst (fst xi_3146) = false then
      ((false, 0), (if fst (snd xi_3146) = false then
                      (false, (true, 0))
                    else
                      (true, x_1051 (snd (snd xi_3146)))))
    else
      if fst (snd xi_3146) = false then
        ((true, x_1537 (snd (fst xi_3146))), (false, (true, 0)))
      else
        ((true, x_1537 (snd (fst xi_3146))), (true, x_1051 (snd (snd xi_3146))))
  in
  x_1541
in
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
        let x_1580 = snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
  let x_1635 = snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
  let x_1638 = snd (fst x_1848) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1643 = snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

elim_unused_let:
let rec append_1053 xs_1009 =
  let x_1542 ys_1010 =
    let x_1543 = xs_1009 0 in
    if fst x_1543 = false then
      let x_1592 ii_3021 =
        if (fst (fst ii_3021) <> false && fst (snd ii_3021) <> false) && snd (fst ii_3021) = snd (snd ii_3021) then
          let r_3038 = (true, ys_1010 (snd (fst ii_3021))) in
          (r_3038, r_3038)
        else
          if fst (fst ii_3021) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3021) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_3021)))))
          else
            if fst (snd ii_3021) = false then
              ((true, ys_1010 (snd (fst ii_3021))), (false, (true, 0)))
            else
              ((true, ys_1010 (snd (fst ii_3021))), (true, ys_1010 (snd (snd ii_3021))))
      in
      x_1592
    else
      let x_1547 = xs_1009 0 in
      if fst x_1547 <> false then
        let xs'_1012 x_1087 =
          let x_1557 = xs_1009 (x_1087 + 1) in
          let xs_1453 n_1454 = if n_1454 = x_1087 + 1 then
                                 x_1557
                               else
                                 xs_1009 n_1454 in
          x_1557
        in
        let x_1560 = xs_1009 0 in
        let cons_1150 x_1146 xs_1147 =
          let x_1562 i_1145 =
            if i_1145 = 0 then
              (true, x_1146)
            else
              let x_1567 = xs_1147 (i_1145 - 1) in
              let xs_1459 n_1460 = if n_1460 = i_1145 - 1 then
                                     x_1567
                                   else
                                     xs_1147 n_1460 in
              x_1567
          in
          let x_1576 ii_2933 =
            if fst (fst ii_2933) = false then
              ((false, (true, 0)), 
               (if fst (snd ii_2933) = false then
                  (false, (true, 0))
                else
                  (true, xs_1147 (snd (snd ii_2933)))))
            else
              if fst (snd ii_2933) = false then
                ((true, x_1562 (snd (fst ii_2933))), (false, (true, 0)))
              else
                ((true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933))))
          in
          x_1576
        in
        let x_1577 = append_1053 xs'_1012 in
        let x_1578 x_2860 = snd (fst (x_1577 ((true, x_2860), (false, 0)))) in
        let x_1579 i_2851 = snd (snd (x_1577 ((false, (fun i_1141 -> (true, 0))), (true, i_2851)))) in
        let x_2850 = x_1577 ((true, ys_1010), (false, 0)) in
        let x_1580 = snd (fst x_2850) in
        let x_1581 i_2810 = snd (fst (x_1580 ((true, i_2810), (false, 0)))) in
        let x_1582 i_2803 = snd (snd (x_1580 ((false, 0), (true, i_2803)))) in
        let x_1583 = cons_1150 (snd x_1560) in
        let x_1584 = x_1583 x_1581 in
        let x_1585 i_2794 = snd (fst (x_1584 ((true, i_2794), (false, 0)))) in
        let x_1586 i_2787 = snd (snd (x_1584 ((false, 0), (true, i_2787)))) in
        let x_1589 ii_2770 =
          if fst (fst ii_2770) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2770) = false then
                (false, (true, 0))
              else
                (true, x_1582 (snd (snd ii_2770)))))
          else
            if fst (snd ii_2770) = false then
              ((true, x_1585 (snd (fst ii_2770))), (false, (true, 0)))
            else
              ((true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770))))
        in
        x_1589
      else
        let x_1551 = _|_ in
        let x_1554 ii_2428 =
          if fst (fst ii_2428) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_2428) = false then
                (false, (true, 0))
              else
                (true, ys_1010 (snd (snd ii_2428)))))
          else
            if fst (snd ii_2428) = false then
              ((true, x_1551 (snd (fst ii_2428))), (false, (true, 0)))
            else
              ((true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428))))
        in
        x_1554
  in
  let x_1595 ysi_2272 =
    if fst (fst ysi_2272) = false then
      ((false, (fun ii_2289 -> ((true, (true, 0)), (true, (true, 0))))), 
       (if fst (snd ysi_2272) = false then
          (false, (true, 0))
        else
          (true, xs_1009 (snd (snd ysi_2272)))))
    else
      if fst (snd ysi_2272) = false then
        ((true, x_1542 (snd (fst ysi_2272))), (false, (true, 0)))
      else
        ((true, x_1542 (snd (fst ysi_2272))), (true, xs_1009 (snd (snd ysi_2272))))
  in
  x_1595
in
let rec make_list_1013 n_1014 =
  if n_1014 < 0 then
    fun x_1221 -> (false, 0)
  else
    let cons_1215 x_1211 xs_1212 =
      let x_1598 i_1210 =
        if i_1210 = 0 then
          (true, x_1211)
        else
          let x_1603 = xs_1212 (i_1210 - 1) in
          let xs_1500 n_1501 = if n_1501 = i_1210 - 1 then
                                 x_1603
                               else
                                 xs_1212 n_1501 in
          x_1603
      in
      let x_1612 ii_2097 =
        if fst (fst ii_2097) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2097) = false then
              (false, (true, 0))
            else
              (true, xs_1212 (snd (snd ii_2097)))))
        else
          if fst (snd ii_2097) = false then
            ((true, x_1598 (snd (fst ii_2097))), (false, (true, 0)))
          else
            ((true, x_1598 (snd (fst ii_2097))), (true, xs_1212 (snd (snd ii_2097))))
      in
      x_1612
    in
    let x_1615 = make_list_1013 (n_1014 - 1) in
    let x_1616 = rand_int () in
    let x_1617 = cons_1215 x_1616 in
    let x_1618 = x_1617 x_1615 in
    let x_1619 i_2021 = snd (fst (x_1618 ((true, i_2021), (false, 0)))) in
    let x_1620 i_2014 = snd (snd (x_1618 ((false, 0), (true, i_2014)))) in
    x_1619
in
let main_1015 i_1016 n_1017 =
  let x_1626 = make_list_1013 n_1017 in
  let f_1409 x_1254 = (false, 0) in
  let x_1632 = append_1053 f_1409 in
  let x_1633 x_1927 = snd (fst (x_1632 ((true, x_1927), (false, 0)))) in
  let x_1634 i_1918 = snd (snd (x_1632 ((false, (fun i_1287 -> (true, 0))), (true, i_1918)))) in
  let x_1917 = x_1632 ((true, x_1626), (false, 0)) in
  let x_1635 = snd (fst x_1917) in
  let x_1636 i_1877 = snd (fst (x_1635 ((true, i_1877), (false, 0)))) in
  let x_1637 i_1870 = snd (snd (x_1635 ((false, 0), (true, i_1870)))) in
  let x_1848 = x_1635 ((true, i_1016), (true, i_1016)) in
  let x_1638 = snd (fst x_1848) in
  let n_1432 = if fst x_1638 <> false then
                 snd x_1638
               else
                 _|_ in
  let x_1643 = snd (snd x_1848) in
  let n_1433 = if fst x_1643 <> false then
                 snd x_1643
               else
                 _|_ in
  if n_1432 = n_1433 then
    ()
  else
    {fail} ()
in
let x_1650 = rand_int () in
let x_1651 = rand_int () in
let x_1652 = main_1015 x_1650 in
let x_1653 = x_1652 x_1651 in
()

TUPLE: (true, x_1551 (snd (fst ii_2428))), (true, ys_1010 (snd (snd ii_2428)))
x_1551
TUPLE: (true, x_1562 (snd (fst ii_2933))), (true, xs_1147 (snd (snd ii_2933)))
x_1562
xs_1147
TUPLE: (true, x_1585 (snd (fst ii_2770))), (true, x_1582 (snd (snd ii_2770)))
x_1585
x_1582
compose: x_1585, snd
                 (fst
                  (x_1584
                    (let x1_3291 = let x1_3283 = true in
                                   let x2_3284 = x_3281 in
                                   (x1_3283, x2_3284) in
                     let x2_3292 = let x1_3287 = false in
                                   let x2_3288 = 0 in
                                   (x1_3287, x2_3288) in
                     (x1_3291, x2_3292)))); x_1582, snd
                                                    (snd
                                                     (x_1580
                                                       (let x1_3303 =
                                                          let x1_3295 = false in
                                                          let x2_3296 = 0 in
                                                          (x1_3295, x2_3296)
                                                        in
                                                        let x2_3304 =
                                                          let x1_3299 = true in
                                                          let x2_3300 = x_3282 in
                                                          (x1_3299, x2_3300)
                                                        in
                                                        (x1_3303, x2_3304)))); 
PB: x:x_1585
CHECK: snd
       (fst
        (x_1584
          (let x1_3291 = let x1_3283 = true in
                         let x2_3284 = x_3281 in
                         (x1_3283, x2_3284) in
           let x2_3292 = let x1_3287 = false in
                         let x2_3288 = 0 in
                         (x1_3287, x2_3288) in
           (x1_3291, x2_3292))))
PB: x:x_1582
CHECK: snd
       (snd
        (x_1580
          (let x1_3303 = let x1_3295 = false in
                         let x2_3296 = 0 in
                         (x1_3295, x2_3296) in
           let x2_3304 = let x1_3299 = true in
                         let x2_3300 = x_3282 in
                         (x1_3299, x2_3300) in
           (x1_3303, x2_3304))))
compose_let
x_1585:snd
       (fst
        (x_1584
          (let x1_3291 = let x1_3283 = true in
                         let x2_3284 = x_3281 in
                         (x1_3283, x2_3284) in
           let x2_3292 = let x1_3287 = false in
                         let x2_3288 = 0 in
                         (x1_3287, x2_3288) in
           (x1_3291, x2_3292))))

x_1582:snd
       (snd
        (x_1580
          (let x1_3303 = let x1_3295 = false in
                         let x2_3296 = 0 in
                         (x1_3295, x2_3296) in
           let x2_3304 = let x1_3299 = true in
                         let x2_3300 = x_3282 in
                         (x1_3299, x2_3300) in
           (x1_3303, x2_3304))))

compose_non_recursive
make_app: int <=/=>
bool
x_x_3307
fst (fst ii_2770)
Fatal error: exception Assert_failure("term_util.ml", 71, 8)
Raised at file "term_util.ml", line 71, characters 8-20
Called from file "term_util.ml", line 80, characters 6-29
Called from file "syntax.ml", line 325, characters 65-83
Called from file "syntax.ml", line 354, characters 39-61
Called from file "syntax.ml", line 325, characters 65-83
Called from file "syntax.ml", line 354, characters 39-61
Called from file "tupling.ml", line 268, characters 55-77
Called from file "extList.ml", line 102, characters 17-20
Called from file "tupling.ml", line 268, characters 22-87
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 270, characters 35-58
Called from file "syntax.ml", line 325, characters 45-63
Called from file "syntax.ml", line 354, characters 39-61
Called from file "tupling.ml", line 270, characters 35-58
Called from file "syntax.ml", line 325, characters 65-83
Called from file "syntax.ml", line 354, characters 39-61
Called from file "tupling.ml", line 270, characters 35-58
Called from file "tupling.ml", line 268, characters 55-77
Called from file "extList.ml", line 102, characters 17-20
Called from file "tupling.ml", line 268, characters 22-87
Called from file "tupling.ml", line 268, characters 55-77
Called from file "extList.ml", line 102, characters 17-20
Called from file "tupling.ml", line 268, characters 22-87
Called from file "tupling.ml", line 304, characters 11-32
Called from file "main_loop.ml", line 9, characters 10-13
Called from file "util.ml", line 19, characters 15-18
Called from file "main_loop.ml", line 212, characters 35-53
Called from file "mochi.ml", line 525, characters 9-17
