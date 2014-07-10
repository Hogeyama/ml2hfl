MoCHi: Model Checker for Higher-Order Programs
  Build: a0896ae (2014-07-08 17:41:44 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil_nth.ml -disable-rc -color -tupling -gchi -list-option -abs-remove-false

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
   let x_1541 (xi_3458:((bool * int) * (bool * int))) =
     ((if fst (fst xi_3458) = false then
         (false, 0)
       else
         (true, x_1537 (snd (fst xi_3458)))),
      (if fst (snd xi_3458) = false then
         (false, (true, 0))
       else
         (true, x_1051 (snd (snd xi_3458)))))
   in
   x_1541
 in
 let rec append_1053 (xs_1009:(int -> (bool * int))) =
   let x_1542 (ys_1010:(int -> (bool * int))) =
     let x_1543 = xs_1009 0 in
     if fst x_1543 = false then
       let x_1592 (ii_3320:((bool * int) * (bool * int))) =
         if (fst (fst ii_3320) <> false && fst (snd ii_3320) <> false) && snd (fst ii_3320) = snd (snd ii_3320) then
           let r_3337 = (true, ys_1010 (snd (fst ii_3320))) in
           (r_3337, r_3337)
         else
           ((if fst (fst ii_3320) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (fst ii_3320)))),
            (if fst (snd ii_3320) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (snd ii_3320)))))
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
           let x_1576 (ii_3219:((bool * int) * (bool * int))) =
             ((if fst (fst ii_3219) = false then
                 (false, (true, 0))
               else
                 (true, x_1562 (snd (fst ii_3219)))),
              (if fst (snd ii_3219) = false then
                 (false, (true, 0))
               else
                 (true, xs_1147 (snd (snd ii_3219)))))
           in
           x_1576
         in
         let x_1577 = append_1053 xs'_1012 in
         let x_1578 (x_3133:(int -> (bool * int))) = snd (fst (x_1577 ((true, x_3133), (false, 0)))) in
         let x_1579 (i_3124:int) = snd (snd (x_1577 ((false, (fun (i_1141:int) -> (true, 0))), (true, i_3124)))) in
         let x_1580 = let x_3123 = x_1577 ((true, ys_1010), (false, 0)) in
                      snd (fst x_3123) in
         let x_1581 (i_3083:int) = snd (fst (x_1580 ((true, i_3083), (false, 0)))) in
         let x_1582 (i_3076:int) = snd (snd (x_1580 ((false, 0), (true, i_3076)))) in
         let x_1583 = cons_1150 (snd x_1560) in
         let x_1584 = x_1583 x_1581 in
         let x_1585 (i_3067:int) = snd (fst (x_1584 ((true, i_3067), (false, 0)))) in
         let x_1586 (i_3060:int) = snd (snd (x_1584 ((false, 0), (true, i_3060)))) in
         let x_1589 (ii_3043:((bool * int) * (bool * int))) =
           ((if fst (fst ii_3043) = false then
               (false, (true, 0))
             else
               (true, x_1585 (snd (fst ii_3043)))),
            (if fst (snd ii_3043) = false then
               (false, (true, 0))
             else
               (true, x_1582 (snd (snd ii_3043)))))
         in
         x_1589
       else
         let x_1551 = _|_ in
         let x_1554 (ii_2532:((bool * int) * (bool * int))) =
           ((if fst (fst ii_2532) = false then
               (false, (true, 0))
             else
               (true, x_1551 (snd (fst ii_2532)))),
            (if fst (snd ii_2532) = false then
               (false, (true, 0))
             else
               (true, ys_1010 (snd (snd ii_2532)))))
         in
         x_1554
   in
   let x_1595 (ysi_2311:((bool * (int -> (bool * int))) * (bool * int))) =
     ((if fst (fst ysi_2311) = false then
         (false, (fun (ii_2232:((bool * int) * (bool * int))) -> ((true, (true, 0)), (true, (true, 0)))))
       else
         (true, x_1542 (snd (fst ysi_2311)))),
      (if fst (snd ysi_2311) = false then
         (false, (true, 0))
       else
         (true, xs_1009 (snd (snd ysi_2311)))))
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
       let x_1612 (ii_2110:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2110) = false then
             (false, (true, 0))
           else
             (true, x_1598 (snd (fst ii_2110)))),
          (if fst (snd ii_2110) = false then
             (false, (true, 0))
           else
             (true, xs_1212 (snd (snd ii_2110)))))
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

tupling:
 let rec append_1053 (xs_1009:(int -> (bool * int))) =
   let x_1542 (ys_1010:(int -> (bool * int))) =
     let x_3635 = xs_1009 0 in
     if fst x_3635 = false then
       let x_1592 (ii_3320:((bool * int) * (bool * int))) =
         if (fst (fst ii_3320) <> false && fst (snd ii_3320) <> false) && snd (fst ii_3320) = snd (snd ii_3320) then
           let x_4239 = ys_1010 (snd (fst ii_3320)) in
           ((true, x_4239), (true, x_4239))
         else
           if fst (fst ii_3320) = false then
             let x_4199 =
               if fst (snd ii_3320) = false then
                 (false, (true, 0))
               else
                 let x_4205 = ys_1010 (snd (snd ii_3320)) in
                 (true, x_4205)
             in
             ((false, (true, 0)), x_4199)
           else
             if fst (snd ii_3320) = false then
               let x_4164 = ys_1010 (snd (fst ii_3320)) in
               ((true, x_4164), (false, (true, 0)))
             else
               let x_4141 = ys_1010 (snd (fst ii_3320)) in
               let x_4151 = ys_1010 (snd (snd ii_3320)) in
               ((true, x_4141), (true, x_4151))
       in
       x_1592
     else
       let x_3636 = xs_1009 0 in
       if fst x_3636 <> false then
         let xs'_1012 (x_1087:int) =
           let x_3738 = xs_1009 (x_1087 + 1) in
           let xs_1453 (n_1454:int) = if n_1454 = x_1087 + 1 then
                                        x_3738
                                      else
                                        xs_1009 n_1454 in
           x_3738
         in
         let x_3744 = xs_1009 0 in
         let cons_1150 (x_1146:int) (xs_1147:(int -> (bool * int))) =
           let x_1562 (i_1145:int) =
             if i_1145 = 0 then
               (true, x_1146)
             else
               let x_3747 = xs_1147 (i_1145 - 1) in
               let xs_1459 (n_1460:int) = if n_1460 = i_1145 - 1 then
                                            x_3747
                                          else
                                            xs_1147 n_1460 in
               x_3747
           in
           let x_1576 (ii_3219:((bool * int) * (bool * int))) =
             if fst (fst ii_3219) = false then
               let x_3823 =
                 if fst (snd ii_3219) = false then
                   (false, (true, 0))
                 else
                   let x_3829 = xs_1147 (snd (snd ii_3219)) in
                   (true, x_3829)
               in
               ((false, (true, 0)), x_3823)
             else
               if fst (snd ii_3219) = false then
                 let x_3788 = x_1562 (snd (fst ii_3219)) in
                 ((true, x_3788), (false, (true, 0)))
               else
                 let x_3765 = x_1562 (snd (fst ii_3219)) in
                 let x_3775 = xs_1147 (snd (snd ii_3219)) in
                 ((true, x_3765), (true, x_3775))
           in
           x_1576
         in
         let x_3858 = append_1053 xs'_1012 in
         let x_1578 (x_3133:(int -> (bool * int))) =
           let x_3875 = x_3858 ((true, x_3133), (false, 0)) in
           snd (fst x_3875)
         in
         let x_1579 (i_3124:int) =
           let x_3899 = x_3858 (let x_3882 (i_1141:int) = (true, 0) in
                                ((false, x_3882), (true, i_3124))) in
           snd (snd x_3899)
         in
         let x_3916 = x_3858 ((true, ys_1010), (false, 0)) in
         let x_1581 (i_3083:int) = let x_3936 = (snd (fst x_3916)) ((true, i_3083), (false, 0)) in
                                   snd (fst x_3936) in
         let x_1582 (i_3076:int) = let x_3955 = (snd (fst x_3916)) ((false, 0), (true, i_3076)) in
                                   snd (snd x_3955) in
         let x_3959 = cons_1150 (snd x_3744) in
         let x_3960 = x_3959 x_1581 in
         let x_1585 (i_3067:int) = let x_3977 = x_3960 ((true, i_3067), (false, 0)) in
                                   snd (fst x_3977) in
         let rec x_x_3621 (x_3593:int) (y_3594:int) =
           let x_3991 = x_3960 ((true, x_3593), (false, 0)) in
           let x_4006 = (snd (fst x_3916)) ((false, 0), (true, y_3594)) in
           (snd (fst x_3991), snd (snd x_4006))
         in
         let x_1586 (i_3060:int) = let x_4028 = x_3960 ((false, 0), (true, i_3060)) in
                                   snd (snd x_4028) in
         let x_1589 (ii_3043:((bool * int) * (bool * int))) =
           if fst (fst ii_3043) = false then
             let x_4096 =
               if fst (snd ii_3043) = false then
                 (false, (true, 0))
               else
                 let x_4102 = x_1582 (snd (snd ii_3043)) in
                 (true, x_4102)
             in
             ((false, (true, 0)), x_4096)
           else
             if fst (snd ii_3043) = false then
               let x_4061 = x_1585 (snd (fst ii_3043)) in
               ((true, x_4061), (false, (true, 0)))
             else
               let x_4037 = x_x_3621 (snd (fst ii_3043)) (snd (snd ii_3043)) in
               ((true, fst x_4037), (true, snd x_4037))
         in
         x_1589
       else
         let x_1551 = _|_ in
         let x_1554 (ii_2532:((bool * int) * (bool * int))) =
           if fst (fst ii_2532) = false then
             let x_3701 =
               if fst (snd ii_2532) = false then
                 (false, (true, 0))
               else
                 let x_3707 = ys_1010 (snd (snd ii_2532)) in
                 (true, x_3707)
             in
             ((false, (true, 0)), x_3701)
           else
             if fst (snd ii_2532) = false then
               let x_3666 = x_1551 (snd (fst ii_2532)) in
               ((true, x_3666), (false, (true, 0)))
             else
               let x_3643 = x_1551 (snd (fst ii_2532)) in
               let x_3653 = ys_1010 (snd (snd ii_2532)) in
               ((true, x_3643), (true, x_3653))
         in
         x_1554
   in
   let x_1595 (ysi_2311:((bool * (int -> (bool * int))) * (bool * int))) =
     if fst (fst ysi_2311) = false then
       let x_4332 (ii_2328:((bool * int) * (bool * int))) = ((true, (true, 0)), (true, (true, 0))) in
       let x_4361 =
         if fst (snd ysi_2311) = false then
           (false, (true, 0))
         else
           let x_4367 = xs_1009 (snd (snd ysi_2311)) in
           (true, x_4367)
       in
       ((false, x_4332), x_4361)
     else
       if fst (snd ysi_2311) = false then
         let x_4306 = x_1542 (snd (fst ysi_2311)) in
         ((true, x_4306), (false, (true, 0)))
       else
         let x_4283 = x_1542 (snd (fst ysi_2311)) in
         let x_4293 = xs_1009 (snd (snd ysi_2311)) in
         ((true, x_4283), (true, x_4293))
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
           let x_4398 = xs_1212 (i_1210 - 1) in
           let xs_1500 (n_1501:int) = if n_1501 = i_1210 - 1 then
                                        x_4398
                                      else
                                        xs_1212 n_1501 in
           x_4398
       in
       let x_1612 (ii_2110:((bool * int) * (bool * int))) =
         if fst (fst ii_2110) = false then
           let x_4474 =
             if fst (snd ii_2110) = false then
               (false, (true, 0))
             else
               let x_4480 = xs_1212 (snd (snd ii_2110)) in
               (true, x_4480)
           in
           ((false, (true, 0)), x_4474)
         else
           if fst (snd ii_2110) = false then
             let x_4439 = x_1598 (snd (fst ii_2110)) in
             ((true, x_4439), (false, (true, 0)))
           else
             let x_4416 = x_1598 (snd (fst ii_2110)) in
             let x_4426 = xs_1212 (snd (snd ii_2110)) in
             ((true, x_4416), (true, x_4426))
       in
       x_1612
     in
     let x_4511 = make_list_1013 (n_1014 - 1) in
     let x_4513 = rand_int () in
     let x_4514 = cons_1215 x_4513 in
     let x_4515 = x_4514 x_4511 in
     let x_1619 (i_2021:int) = let x_4532 = x_4515 ((true, i_2021), (false, 0)) in
                               snd (fst x_4532) in
     let x_1620 (i_2014:int) = let x_4551 = x_4515 ((false, 0), (true, i_2014)) in
                               snd (snd x_4551) in
     x_1619
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_4561 = make_list_1013 n_1017 in
   let f_1409 (x_1254:int) = (false, 0) in
   let x_4567 = append_1053 f_1409 in
   let x_1633 (x_1927:(int -> (bool * int))) = let x_4584 = x_4567 ((true, x_1927), (false, 0)) in
                                               snd (fst x_4584) in
   let x_1634 (i_1918:int) =
     let x_4608 = x_4567 (let x_4591 (i_1287:int) = (true, 0) in
                          ((false, x_4591), (true, i_1918))) in
     snd (snd x_4608)
   in
   let x_4625 = x_4567 ((true, x_4561), (false, 0)) in
   let x_1636 (i_1877:int) = let x_4645 = (snd (fst x_4625)) ((true, i_1877), (false, 0)) in
                             snd (fst x_4645) in
   let x_1637 (i_1870:int) = let x_4664 = (snd (fst x_4625)) ((false, 0), (true, i_1870)) in
                             snd (snd x_4664) in
   let x_4680 = (snd (fst x_4625)) ((true, i_1016), (true, i_1016)) in
   let n_1432 = if fst (snd (fst x_4680)) <> false then
                  snd (snd (fst x_4680))
                else
                  _|_ in
   let n_1433 = if fst (snd (snd x_4680)) <> false then
                  snd (snd (snd x_4680))
                else
                  _|_ in
   if n_1432 = n_1433 then
     ()
   else
     {fail} ()
 in
 let x_4701 = rand_int () in
 let x_4703 = rand_int () in
 let x_4704 = main_1015 x_4701 in
 let x_4705 = x_4704 x_4703 in
 let x_1653 = x_4705 in
 ()

CPS:
 let rec
   append_1053 (xs_1009:(int -> ((bool * int) -> X) -> X)) 
              (k_append_4727:((((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int)) ->
                                 (((bool * 
                                    (((bool * int) * (bool * int)) ->
                                       (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                   * (bool * (bool * int))) -> X) -> X) -> X)) =
   k_append_4727
     (let
        x_1542 (ys_1010:(int -> ((bool * int) -> X) -> X)) 
              (k_append_x_4744:((((bool * int) * (bool * int)) ->
                                   (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X) -> X)) =
        let x_3635 (k_append_x_x_4754:((bool * int) -> X)) = xs_1009 0 k_append_x_x_4754 in
        x_3635
          (fun (x_6191:(bool * int)) ->
             (if fst x_6191 = false then
                k_append_x_4744
                  (let
                     x_1592 (ii_3320:((bool * int) * (bool * int))) 
                           (k_append_x_x_4764:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                     let k_append_x_x_4771 (b_7709:bool) =
                       if b_7709 then
                         let x_7786 (k_append_x_x_x_7787:((bool * int) -> X)) =
                           ys_1010 (snd (fst ii_3320)) k_append_x_x_x_7787
                         in
                         x_7786 (fun (x_7790:(bool * int)) -> k_append_x_x_4764 ((true, x_7790), (true, x_7790)))
                       else
                         if fst (fst ii_3320) = false then
                           let x_7756 (k_append_x_x_x_7757:((bool * (bool * int)) -> X)) =
                             if fst (snd ii_3320) = false then
                               k_append_x_x_x_7757 (false, (true, 0))
                             else
                               let x_7761 (k_append_x_x_x_x_7762:((bool * int) -> X)) =
                                 ys_1010 (snd (snd ii_3320)) k_append_x_x_x_x_7762
                               in
                               x_7761 (fun (x_7765:(bool * int)) -> k_append_x_x_x_7757 (true, x_7765))
                           in
                           x_7756
                             (fun (x_7775:(bool * (bool * int))) -> k_append_x_x_4764 ((false, (true, 0)), x_7775))
                         else
                           if fst (snd ii_3320) = false then
                             let x_7738 (k_append_x_x_x_7739:((bool * int) -> X)) =
                               ys_1010 (snd (fst ii_3320)) k_append_x_x_x_7739
                             in
                             x_7738
                               (fun (x_7742:(bool * int)) -> k_append_x_x_4764 ((true, x_7742), (false, (true, 0))))
                           else
                             let x_7717 (k_append_x_x_x_7718:((bool * int) -> X)) =
                               ys_1010 (snd (fst ii_3320)) k_append_x_x_x_7718
                             in
                             x_7717
                               (fun (x_7721:(bool * int)) ->
                                  (let x_7723 (k_append_x_x_x_7724:((bool * int) -> X)) =
                                     ys_1010 (snd (snd ii_3320)) k_append_x_x_x_7724
                                   in
                                   x_7723
                                     (fun (x_7727:(bool * int)) -> k_append_x_x_4764 ((true, x_7721), (true, x_7727)))))
                     in
                     let k_append_x_x_4767 (b_7708:bool) =
                       if b_7708 then
                         k_append_x_x_4771 (snd (fst ii_3320) = snd (snd ii_3320))
                       else
                         k_append_x_x_4771 false
                     in
                     if fst (fst ii_3320) <> false then
                       k_append_x_x_4767 (fst (snd ii_3320) <> false)
                     else
                       k_append_x_x_4767 false
                   in
                   x_1592)
              else
                let x_3636 (k_append_x_x_4965:((bool * int) -> X)) = xs_1009 0 k_append_x_x_4965 in
                x_3636
                  (fun (x_6183:(bool * int)) ->
                     (if fst x_6183 <> false then
                        let xs'_1012 (x_1087:int) (k_append_x_xs'_4977:((bool * int) -> X)) =
                          let x_3738 (k_append_x_xs'_x_4984:((bool * int) -> X)) =
                            xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984
                          in
                          x_3738 (fun (x_5009:(bool * int)) -> k_append_x_xs'_4977 x_5009)
                        in
                        let x_3744 (k_append_x_x_5022:((bool * int) -> X)) = xs_1009 0 k_append_x_x_5022 in
                        x_3744
                          (fun (x_6001:(bool * int)) ->
                             (let cons_1150 (x_1146:int) (xs_1147:(int -> ((bool * int) -> X) -> X)) =
                                let x_1562 (i_1145:int) (k_append_x_cons_x_5040:((bool * int) -> X)) =
                                  if i_1145 = 0 then
                                    k_append_x_cons_x_5040 (true, x_1146)
                                  else
                                    let x_3747 (k_append_x_cons_x_x_5053:((bool * int) -> X)) =
                                      xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053
                                    in
                                    x_3747 (fun (x_5078:(bool * int)) -> k_append_x_cons_x_5040 x_5078)
                                in
                                let
                                  x_1576 (ii_3219:((bool * int) * (bool * int))) 
                                        (k_append_x_cons_x_5087:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                  if fst (fst ii_3219) = false then
                                    let x_3823 (k_append_x_cons_x_x_5122:((bool * (bool * int)) -> X)) =
                                      if fst (snd ii_3219) = false then
                                        k_append_x_cons_x_x_5122 (false, (true, 0))
                                      else
                                        let x_3829 (k_append_x_cons_x_x_x_5108:((bool * int) -> X)) =
                                          xs_1147 (snd (snd ii_3219)) k_append_x_cons_x_x_x_5108
                                        in
                                        x_3829 (fun (x_5120:(bool * int)) -> k_append_x_cons_x_x_5122 (true, x_5120))
                                    in
                                    x_3823
                                      (fun (x_5143:(bool * (bool * int))) ->
                                         k_append_x_cons_x_5087 ((false, (true, 0)), x_5143))
                                  else
                                    if fst (snd ii_3219) = false then
                                      let x_3788 (k_append_x_cons_x_x_5150:((bool * int) -> X)) =
                                        x_1562 (snd (fst ii_3219)) k_append_x_cons_x_x_5150
                                      in
                                      x_3788
                                        (fun (x_5180:(bool * int)) ->
                                           k_append_x_cons_x_5087 ((true, x_5180), (false, (true, 0))))
                                    else
                                      let x_3765 (k_append_x_cons_x_x_5187:((bool * int) -> X)) =
                                        x_1562 (snd (fst ii_3219)) k_append_x_cons_x_x_5187
                                      in
                                      x_3765
                                        (fun (x_5224:(bool * int)) ->
                                           (let x_3775 (k_append_x_cons_x_x_5199:((bool * int) -> X)) =
                                              xs_1147 (snd (snd ii_3219)) k_append_x_cons_x_x_5199
                                            in
                                            x_3775
                                              (fun (x_5223:(bool * int)) ->
                                                 k_append_x_cons_x_5087 ((true, x_5224), (true, x_5223)))))
                                in
                                x_1576
                              in
                              let
                                x_3858
                                      (k_append_x_x_5277:((((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int))
                                                             ->
                                                             (((bool * 
                                                                (((bool * int) * (bool * int)) ->
                                                                   (((bool * (bool * int)) * (bool * (bool * int))) ->
                                                                    X) -> X))
                                                               * (bool * (bool * int))) -> X) -> X) -> X)) =
                                append_1053 xs'_1012 k_append_x_x_5277
                              in
                              x_3858
                                (fun (x_5991:(((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int)) ->
                                                (((bool * 
                                                   (((bool * int) * (bool * int)) ->
                                                      (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                                  * (bool * (bool * int))) -> X) -> X)) ->
                                   (let
                                      x_3916
                                            (k_append_x_x_5559:(((bool * 
                                                                  (((bool * int) * (bool * int)) ->
                                                                    (((bool * (bool * int)) * (bool * (bool * int))) ->
                                                                    X) -> X))
                                                                 * (bool * (bool * int))) -> X)) =
                                      x_5991 ((true, ys_1010), (false, 0)) k_append_x_x_5559
                                    in
                                    x_3916
                                      (fun (x_5967:((bool * 
                                                     (((bool * int) * (bool * int)) ->
                                                        (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                                    * (bool * (bool * int)))) ->
                                         k_append_x_4744
                                           (let x_1581 (i_3083:int) (k_append_x_x_5581:((bool * int) -> X)) =
                                              let
                                                x_3936
                                                      (k_append_x_x_x_5606:(
                                                      ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                                (snd (fst x_5967)) ((true, i_3083), (false, 0)) k_append_x_x_x_5606
                                              in
                                              x_3936
                                                (fun (x_5612:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                                   k_append_x_x_5581 (snd (fst x_5612)))
                                            in
                                            let x_1582 (i_3076:int) (k_append_x_x_5617:((bool * int) -> X)) =
                                              let
                                                x_3955
                                                      (k_append_x_x_x_5642:(
                                                      ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                                (snd (fst x_5967)) ((false, 0), (true, i_3076)) k_append_x_x_x_5642
                                              in
                                              x_3955
                                                (fun (x_5648:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                                   k_append_x_x_5617 (snd (snd x_5648)))
                                            in
                                            let x_1585 (i_3067:int) (k_append_x_x_5681:((bool * int) -> X)) =
                                              let
                                                x_3977
                                                      (k_append_x_x_x_5706:(
                                                      ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                                ((cons_1150 (snd x_6001)) x_1581) 
                                                  ((true, i_3067), (false, 0)) k_append_x_x_x_5706
                                              in
                                              x_3977
                                                (fun (x_5712:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                                   k_append_x_x_5681 (snd (fst x_5712)))
                                            in
                                            let rec
                                              x_x_3621 (x_3593:int) (y_3594:int) 
                                                      (k_append_x_x_x_5718:(
                                                      ((bool * int) * (bool * int)) -> X)) =
                                              let
                                                x_3991
                                                      (k_append_x_x_x_x_5743:(
                                                      ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                                ((cons_1150 (snd x_6001)) x_1581) 
                                                  ((true, x_3593), (false, 0)) k_append_x_x_x_x_5743
                                              in
                                              x_3991
                                                (fun (x_5786:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                                   (let
                                                      x_4006
                                                            (k_append_x_x_x_x_5773:(
                                                            ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                                      (snd (fst x_5967)) 
                                                        ((false, 0), (true, y_3594)) k_append_x_x_x_x_5773
                                                    in
                                                    x_4006
                                                      (fun (x_5785:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                                         k_append_x_x_x_5718 (snd (fst x_5786), snd (snd x_5785)))))
                                            in
                                            let
                                              x_1589 (ii_3043:((bool * int) * (bool * int))) 
                                                    (k_append_x_x_5827:(
                                                    ((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                              if fst (fst ii_3043) = false then
                                                let x_4096 (k_append_x_x_x_5862:((bool * (bool * int)) -> X)) =
                                                  if fst (snd ii_3043) = false then
                                                    k_append_x_x_x_5862 (false, (true, 0))
                                                  else
                                                    let x_4102 (k_append_x_x_x_x_5848:((bool * int) -> X)) =
                                                      x_1582 (snd (snd ii_3043)) k_append_x_x_x_x_5848
                                                    in
                                                    x_4102
                                                      (fun (x_5860:(bool * int)) -> k_append_x_x_x_5862 (true, x_5860))
                                                in
                                                x_4096
                                                  (fun (x_5883:(bool * (bool * int))) ->
                                                     k_append_x_x_5827 ((false, (true, 0)), x_5883))
                                              else
                                                if fst (snd ii_3043) = false then
                                                  let x_4061 (k_append_x_x_x_5890:((bool * int) -> X)) =
                                                    x_1585 (snd (fst ii_3043)) k_append_x_x_x_5890
                                                  in
                                                  x_4061
                                                    (fun (x_5920:(bool * int)) ->
                                                       k_append_x_x_5827 ((true, x_5920), (false, (true, 0))))
                                                else
                                                  let
                                                    x_4037 (k_append_x_x_x_5928:(((bool * int) * (bool * int)) -> X)) =
                                                    x_x_3621 (snd (fst ii_3043)) (
                                                      snd (snd ii_3043)) k_append_x_x_x_5928
                                                  in
                                                  x_4037
                                                    (fun (x_5952:((bool * int) * (bool * int))) ->
                                                       k_append_x_x_5827 ((true, fst x_5952), (true, snd x_5952)))
                                            in
                                            x_1589))))))
                      else
                        let x_1551 (k_append_x_x_6015:((int -> ((bool * int) -> X) -> X) -> X)) = _|_ in
                        x_1551
                          (fun (x_6175:(int -> ((bool * int) -> X) -> X)) ->
                             k_append_x_4744
                               (let
                                  x_1554 (ii_2532:((bool * int) * (bool * int))) 
                                        (k_append_x_x_6023:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                  if fst (fst ii_2532) = false then
                                    let x_3701 (k_append_x_x_x_6058:((bool * (bool * int)) -> X)) =
                                      if fst (snd ii_2532) = false then
                                        k_append_x_x_x_6058 (false, (true, 0))
                                      else
                                        let x_3707 (k_append_x_x_x_x_6044:((bool * int) -> X)) =
                                          ys_1010 (snd (snd ii_2532)) k_append_x_x_x_x_6044
                                        in
                                        x_3707 (fun (x_6056:(bool * int)) -> k_append_x_x_x_6058 (true, x_6056))
                                    in
                                    x_3701
                                      (fun (x_6079:(bool * (bool * int))) ->
                                         k_append_x_x_6023 ((false, (true, 0)), x_6079))
                                  else
                                    if fst (snd ii_2532) = false then
                                      let x_3666 (k_append_x_x_x_6086:((bool * int) -> X)) =
                                        x_6175 (snd (fst ii_2532)) k_append_x_x_x_6086
                                      in
                                      x_3666
                                        (fun (x_6116:(bool * int)) ->
                                           k_append_x_x_6023 ((true, x_6116), (false, (true, 0))))
                                    else
                                      let x_3643 (k_append_x_x_x_6123:((bool * int) -> X)) =
                                        x_6175 (snd (fst ii_2532)) k_append_x_x_x_6123
                                      in
                                      x_3643
                                        (fun (x_6160:(bool * int)) ->
                                           (let x_3653 (k_append_x_x_x_6135:((bool * int) -> X)) =
                                              ys_1010 (snd (snd ii_2532)) k_append_x_x_x_6135
                                            in
                                            x_3653
                                              (fun (x_6159:(bool * int)) ->
                                                 k_append_x_x_6023 ((true, x_6160), (true, x_6159)))))
                                in
                                x_1554))))))
      in
      let
        x_1595 (ysi_2311:((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int))) 
              (k_append_x_6208:(((bool * 
                                  (((bool * int) * (bool * int)) ->
                                     (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                 * (bool * (bool * int))) -> X)) =
        if fst (fst ysi_2311) = false then
          let
            x_4332 (ii_2328:((bool * int) * (bool * int))) 
                  (k_append_x_x_6218:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
            k_append_x_x_6218 ((true, (true, 0)), (true, (true, 0)))
          in
          let x_4361 (k_append_x_x_6295:((bool * (bool * int)) -> X)) =
            if fst (snd ysi_2311) = false then
              k_append_x_x_6295 (false, (true, 0))
            else
              let x_4367 (k_append_x_x_x_6281:((bool * int) -> X)) = xs_1009 (snd (snd ysi_2311)) k_append_x_x_x_6281 in
              x_4367 (fun (x_6293:(bool * int)) -> k_append_x_x_6295 (true, x_6293))
          in
          x_4361 (fun (x_6325:(bool * (bool * int))) -> k_append_x_6208 ((false, x_4332), x_6325))
        else
          if fst (snd ysi_2311) = false then
            let
              x_4306
                    (k_append_x_x_6354:((((bool * int) * (bool * int)) ->
                                           (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X) -> X)) =
              x_1542 (snd (fst ysi_2311)) k_append_x_x_6354
            in
            x_4306
              (fun (x_6414:(((bool * int) * (bool * int)) ->
                              (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                 k_append_x_6208 ((true, x_6414), (false, (true, 0))))
          else
            let
              x_4283
                    (k_append_x_x_6436:((((bool * int) * (bool * int)) ->
                                           (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X) -> X)) =
              x_1542 (snd (fst ysi_2311)) k_append_x_x_6436
            in
            x_4283
              (fun (x_6509:(((bool * int) * (bool * int)) ->
                              (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                 (let x_4293 (k_append_x_x_6466:((bool * int) -> X)) = xs_1009 (snd (snd ysi_2311)) k_append_x_x_6466 in
                  x_4293 (fun (x_6505:(bool * int)) -> k_append_x_6208 ((true, x_6509), (true, x_6505)))))
      in
      x_1595)
 in
 let rec make_list_1013 (n_1014:int) (k_make_list_6554:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1014 < 0 then
     k_make_list_6554 (fun (x_1221:int) -> fun (k_make_list_6556:((bool * int) -> X)) -> k_make_list_6556 (false, 0))
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> ((bool * int) -> X) -> X)) =
       let x_1598 (i_1210:int) (k_make_list_cons_x_6575:((bool * int) -> X)) =
         if i_1210 = 0 then
           k_make_list_cons_x_6575 (true, x_1211)
         else
           let x_4398 (k_make_list_cons_x_x_6588:((bool * int) -> X)) = xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588 in
           x_4398 (fun (x_6613:(bool * int)) -> k_make_list_cons_x_6575 x_6613)
       in
       let
         x_1612 (ii_2110:((bool * int) * (bool * int))) 
               (k_make_list_cons_x_6622:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
         if fst (fst ii_2110) = false then
           let x_4474 (k_make_list_cons_x_x_6657:((bool * (bool * int)) -> X)) =
             if fst (snd ii_2110) = false then
               k_make_list_cons_x_x_6657 (false, (true, 0))
             else
               let x_4480 (k_make_list_cons_x_x_x_6643:((bool * int) -> X)) =
                 xs_1212 (snd (snd ii_2110)) k_make_list_cons_x_x_x_6643
               in
               x_4480 (fun (x_6655:(bool * int)) -> k_make_list_cons_x_x_6657 (true, x_6655))
           in
           x_4474 (fun (x_6678:(bool * (bool * int))) -> k_make_list_cons_x_6622 ((false, (true, 0)), x_6678))
         else
           if fst (snd ii_2110) = false then
             let x_4439 (k_make_list_cons_x_x_6685:((bool * int) -> X)) =
               x_1598 (snd (fst ii_2110)) k_make_list_cons_x_x_6685
             in
             x_4439 (fun (x_6715:(bool * int)) -> k_make_list_cons_x_6622 ((true, x_6715), (false, (true, 0))))
           else
             let x_4416 (k_make_list_cons_x_x_6722:((bool * int) -> X)) =
               x_1598 (snd (fst ii_2110)) k_make_list_cons_x_x_6722
             in
             x_4416
               (fun (x_6759:(bool * int)) ->
                  (let x_4426 (k_make_list_cons_x_x_6734:((bool * int) -> X)) =
                     xs_1212 (snd (snd ii_2110)) k_make_list_cons_x_x_6734
                   in
                   x_4426 (fun (x_6758:(bool * int)) -> k_make_list_cons_x_6622 ((true, x_6759), (true, x_6758)))))
       in
       x_1612
     in
     let x_4511 (k_make_list_x_6794:((int -> ((bool * int) -> X) -> X) -> X)) =
       make_list_1013 (n_1014 - 1) k_make_list_x_6794
     in
     x_4511
       (fun (x_6928:(int -> ((bool * int) -> X) -> X)) ->
          (let x_4513 (k_make_list_x_6815:(int -> X)) = rand_int_cps () k_make_list_x_6815 in
           x_4513
             (fun (x_6924:int) ->
                k_make_list_6554
                  (let x_1619 (i_2021:int) (k_make_list_x_6853:((bool * int) -> X)) =
                     let x_4532 (k_make_list_x_x_6878:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       ((cons_1215 x_6924) x_6928) ((true, i_2021), (false, 0)) k_make_list_x_x_6878
                     in
                     x_4532
                       (fun (x_6884:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_make_list_x_6853 (snd (fst x_6884)))
                   in
                   x_1619))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_6954:(unit -> X)) =
   let x_4561 (k_main_x_6967:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1013 n_1017 k_main_x_6967 in
   x_4561
     (fun (x_7492:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1409 (x_1254:int) (k_main_f_6982:((bool * int) -> X)) = k_main_f_6982 (false, 0) in
         let
           x_4567
                 (k_main_x_7022:((((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int)) ->
                                    (((bool * 
                                       (((bool * int) * (bool * int)) ->
                                          (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                      * (bool * (bool * int))) -> X) -> X) -> X)) = 
           append_1053 f_1409 k_main_x_7022
         in
         x_4567
           (fun (x_7484:(((bool * (int -> ((bool * int) -> X) -> X)) * (bool * int)) ->
                           (((bool * 
                              (((bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                             * (bool * (bool * int))) -> X) -> X)) ->
              (let
                 x_4625
                       (k_main_x_7295:(((bool * 
                                         (((bool * int) * (bool * int)) ->
                                            (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                                        * (bool * (bool * int))) -> X)) =
                 x_7484 ((true, x_7492), (false, 0)) k_main_x_7295
               in
               x_4625
                 (fun (x_7460:((bool * 
                                (((bool * int) * (bool * int)) ->
                                   (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
                               * (bool * (bool * int)))) ->
                    (let x_4680 (k_main_x_7413:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       (snd (fst x_7460)) ((true, i_1016), (true, i_1016)) k_main_x_7413
                     in
                     x_4680
                       (fun (x_7448:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          (let n_1432 (k_main_n_7424:(int -> X)) =
                             if fst (snd (fst x_7448)) <> false then
                               k_main_n_7424 (snd (snd (fst x_7448)))
                             else
                               _|_
                           in
                           n_1432
                             (fun (n_7447:int) ->
                                (let n_1433 (k_main_n_7432:(int -> X)) =
                                   if fst (snd (snd x_7448)) <> false then
                                     k_main_n_7432 (snd (snd (snd x_7448)))
                                   else
                                     _|_
                                 in
                                 n_1433
                                   (fun (n_7446:int) ->
                                      (if n_7447 = n_7446 then
                                         k_main_6954 ()
                                       else
                                         {|fail|} () k_main_6954))))))))))))
 in
 let x_4701 (k_x_7503:(int -> X)) = rand_int_cps () k_x_7503 in
 x_4701
   (fun (x_7548:int) ->
      (let x_4703 (k_x_7515:(int -> X)) = rand_int_cps () k_x_7515 in
       x_4703
         (fun (x_7547:int) ->
            (let x_4705 (k_x_7536:(unit -> X)) = (main_1015 x_7548) x_7547 k_x_7536 in
             x_4705 (fun (x_7542:unit) -> {end})))))

remove_pair:
 let rec
   append_1053 (xs_1009:(int -> (bool -> int -> X) -> X)) 
              (k_append_4727:((bool ->
                                 (int -> (bool -> int -> X) -> X) ->
                                   bool ->
                                     int ->
                                       (bool ->
                                          (bool ->
                                             int ->
                                               bool ->
                                                 int ->
                                                   (bool ->
                                                      bool ->
                                                        r011_4722:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_4722:int[\r111_4722. r011_4722 = r111_4722] -> X) ->
                                                     X) -> bool -> bool -> int -> X) -> X) -> X)) =
   k_append_4727
     (let
        x_1542 (ys_1010:(int -> (bool -> int -> X) -> X)) 
              (k_append_x_4744:((bool ->
                                   int ->
                                     bool ->
                                       int ->
                                         (bool ->
                                            bool ->
                                              r011_4733:int ->
                                                bool -> bool -> r111_4733:int[\r111_4733. r011_4733 = r111_4733] -> X)
                                           -> X) -> X)) =
        let x_3635 (k_append_x_x_4754:(bool -> int -> X)) = xs_1009 0 k_append_x_x_4754 in
        x_3635
          (fun (x0_6191:bool) ->
             fun (x1_6191:int) ->
               (if x0_6191 = false then
                  k_append_x_4744
                    (let
                       x_1592 (ii00_3320:bool) (ii01_3320:int) (ii10_3320:bool) (ii11_3320:int) 
                             (k_append_x_x_4764:(bool ->
                                                   bool ->
                                                     r011_4761:int ->
                                                       bool ->
                                                         bool -> r111_4761:int[\r111_4761. r011_4761 = r111_4761] -> X)) =
                       let k_append_x_x_4771 (b_7709:bool) =
                         if b_7709 then
                           let x_7786 (k_append_x_x_x_7787:(bool -> int -> X)) = ys_1010 ii01_3320 k_append_x_x_x_7787 in
                           x_7786
                             (fun (x0_7790:bool) ->
                                fun (x1_7790:int) -> k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790)
                         else
                           if ii00_3320 = false then
                             let x_7756 (k_append_x_x_x_7757:(bool -> bool -> int -> X)) =
                               if ii10_3320 = false then
                                 k_append_x_x_x_7757 false true 0
                               else
                                 let x_7761 (k_append_x_x_x_x_7762:(bool -> int -> X)) =
                                   ys_1010 ii11_3320 k_append_x_x_x_x_7762
                                 in
                                 x_7761
                                   (fun (x0_7765:bool) ->
                                      fun (x1_7765:int) -> k_append_x_x_x_7757 true x0_7765 x1_7765)
                             in
                             x_7756
                               (fun (x0_7775:bool) ->
                                  fun (x10_7775:bool) ->
                                    fun (x11_7775:int) -> k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775)
                           else
                             if ii10_3320 = false then
                               let x_7738 (k_append_x_x_x_7739:(bool -> int -> X)) =
                                 ys_1010 ii01_3320 k_append_x_x_x_7739
                               in
                               x_7738
                                 (fun (x0_7742:bool) ->
                                    fun (x1_7742:int) -> k_append_x_x_4764 true x0_7742 x1_7742 false true 0)
                             else
                               let x_7717 (k_append_x_x_x_7718:(bool -> int -> X)) =
                                 ys_1010 ii01_3320 k_append_x_x_x_7718
                               in
                               x_7717
                                 (fun (x0_7721:bool) ->
                                    fun (x1_7721:int) ->
                                      (let x_7723 (k_append_x_x_x_7724:(bool -> int -> X)) =
                                         ys_1010 ii11_3320 k_append_x_x_x_7724
                                       in
                                       x_7723
                                         (fun (x0_7727:bool) ->
                                            fun (x1_7727:int) ->
                                              k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727)))
                       in
                       let k_append_x_x_4767 (b_7708:bool) =
                         if b_7708 then
                           k_append_x_x_4771 (ii01_3320 = ii11_3320)
                         else
                           k_append_x_x_4771 false
                       in
                       if ii00_3320 <> false then
                         k_append_x_x_4767 (ii10_3320 <> false)
                       else
                         k_append_x_x_4767 false
                     in
                     x_1592)
                else
                  let x_3636 (k_append_x_x_4965:(bool -> int -> X)) = xs_1009 0 k_append_x_x_4965 in
                  x_3636
                    (fun (x0_6183:bool) ->
                       fun (x1_6183:int) ->
                         (if x0_6183 <> false then
                            let xs'_1012 (x_1087:int) (k_append_x_xs'_4977:(bool -> int -> X)) =
                              let x_3738 (k_append_x_xs'_x_4984:(bool -> int -> X)) =
                                xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984
                              in
                              x_3738 (fun (x0_5009:bool) -> fun (x1_5009:int) -> k_append_x_xs'_4977 x0_5009 x1_5009)
                            in
                            let x_3744 (k_append_x_x_5022:(bool -> int -> X)) = xs_1009 0 k_append_x_x_5022 in
                            x_3744
                              (fun (x0_6001:bool) ->
                                 fun (x1_6001:int) ->
                                   (let cons_1150 (x_1146:int) (xs_1147:(int -> (bool -> int -> X) -> X)) =
                                      let x_1562 (i_1145:int) (k_append_x_cons_x_5040:(bool -> int -> X)) =
                                        if i_1145 = 0 then
                                          k_append_x_cons_x_5040 true x_1146
                                        else
                                          let x_3747 (k_append_x_cons_x_x_5053:(bool -> int -> X)) =
                                            xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053
                                          in
                                          x_3747
                                            (fun (x0_5078:bool) ->
                                               fun (x1_5078:int) -> k_append_x_cons_x_5040 x0_5078 x1_5078)
                                      in
                                      let
                                        x_1576 (ii00_3219:bool) (ii01_3219:int) (ii10_3219:bool) (ii11_3219:int) 
                                              (k_append_x_cons_x_5087:(
                                              bool ->
                                                bool ->
                                                  r011_5084:int ->
                                                    bool ->
                                                      bool -> r111_5084:int[\r111_5084. r011_5084 = r111_5084] -> X)) =
                                        if ii00_3219 = false then
                                          let x_3823 (k_append_x_cons_x_x_5122:(bool -> bool -> int -> X)) =
                                            if ii10_3219 = false then
                                              k_append_x_cons_x_x_5122 false true 0
                                            else
                                              let x_3829 (k_append_x_cons_x_x_x_5108:(bool -> int -> X)) =
                                                xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108
                                              in
                                              x_3829
                                                (fun (x0_5120:bool) ->
                                                   fun (x1_5120:int) -> k_append_x_cons_x_x_5122 true x0_5120 x1_5120)
                                          in
                                          x_3823
                                            (fun (x0_5143:bool) ->
                                               fun (x10_5143:bool) ->
                                                 fun (x11_5143:int) ->
                                                   k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143)
                                        else
                                          if ii10_3219 = false then
                                            let x_3788 (k_append_x_cons_x_x_5150:(bool -> int -> X)) =
                                              x_1562 ii01_3219 k_append_x_cons_x_x_5150
                                            in
                                            x_3788
                                              (fun (x0_5180:bool) ->
                                                 fun (x1_5180:int) ->
                                                   k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0)
                                          else
                                            let x_3765 (k_append_x_cons_x_x_5187:(bool -> int -> X)) =
                                              x_1562 ii01_3219 k_append_x_cons_x_x_5187
                                            in
                                            x_3765
                                              (fun (x0_5224:bool) ->
                                                 fun (x1_5224:int) ->
                                                   (let x_3775 (k_append_x_cons_x_x_5199:(bool -> int -> X)) =
                                                      xs_1147 ii11_3219 k_append_x_cons_x_x_5199
                                                    in
                                                    x_3775
                                                      (fun (x0_5223:bool) ->
                                                         fun (x1_5223:int) ->
                                                           k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223
                                                             x1_5223)))
                                      in
                                      x_1576
                                    in
                                    let
                                      x_3858
                                            (k_append_x_x_5277:((bool ->
                                                                   (int -> (bool -> int -> X) -> X) ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    (bool ->
                                                                    int ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5272:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5272:
                                                                    int[\r111_5272. r011_5272 = r111_5272] -> X) -> X)
                                                                    -> 
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                                      append_1053 xs'_1012 k_append_x_x_5277
                                    in
                                    x_3858
                                      (fun (x_5991:(bool ->
                                                      (int -> (bool -> int -> X) -> X) ->
                                                        bool ->
                                                          int ->
                                                            (bool ->
                                                               (bool ->
                                                                  int ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5987:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5987:
                                                                    int[\r111_5987. r011_5987 = r111_5987] -> X) -> X)
                                                                 -> bool -> bool -> int -> X) -> X)) ->
                                         (let
                                            x_3916
                                                  (k_append_x_x_5559:(
                                                  bool ->
                                                    (bool ->
                                                       int ->
                                                         bool ->
                                                           int ->
                                                             (bool ->
                                                                bool ->
                                                                  r011_5556:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5556:
                                                                    int[\r111_5556. r011_5556 = r111_5556] -> X) -> X)
                                                      -> bool -> bool -> int -> X)) =
                                            x_5991 true ys_1010 false 0 k_append_x_x_5559
                                          in
                                          x_3916
                                            (fun (x00_5967:bool) ->
                                               fun (x01_5967:(bool ->
                                                                int ->
                                                                  bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5965:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5965:
                                                                    int[\r111_5965. r011_5965 = r111_5965] -> X) -> X)) ->
                                                 fun (x10_5967:bool) ->
                                                   fun (x110_5967:bool) ->
                                                     fun (x111_5967:int) ->
                                                       k_append_x_4744
                                                         (let
                                                            x_1581 (i_3083:int) (k_append_x_x_5581:(bool -> int -> X)) =
                                                            let
                                                              x_3936
                                                                    (k_append_x_x_x_5606:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5605:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5605:
                                                                    int[\r111_5605. r011_5605 = r111_5605] -> X)) =
                                                              x01_5967 true i_3083 false 0 k_append_x_x_x_5606
                                                            in
                                                            x_3936
                                                              (fun (x00_5612:bool) ->
                                                                 fun (x010_5612:bool) ->
                                                                   fun (x011_5612:int) ->
                                                                    fun (x10_5612:bool) ->
                                                                    fun (x110_5612:bool) ->
                                                                    fun (x111_5612:int) ->
                                                                    k_append_x_x_5581 x010_5612 x011_5612)
                                                          in
                                                          let
                                                            x_1582 (i_3076:int) (k_append_x_x_5617:(bool -> int -> X)) =
                                                            let
                                                              x_3955
                                                                    (k_append_x_x_x_5642:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5641:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5641:
                                                                    int[\r111_5641. r011_5641 = r111_5641] -> X)) =
                                                              x01_5967 false 0 true i_3076 k_append_x_x_x_5642
                                                            in
                                                            x_3955
                                                              (fun (x00_5648:bool) ->
                                                                 fun (x010_5648:bool) ->
                                                                   fun (x011_5648:int) ->
                                                                    fun (x10_5648:bool) ->
                                                                    fun (x110_5648:bool) ->
                                                                    fun (x111_5648:int) ->
                                                                    k_append_x_x_5617 x110_5648 x111_5648)
                                                          in
                                                          let
                                                            x_1585 (i_3067:int) (k_append_x_x_5681:(bool -> int -> X)) =
                                                            let
                                                              x_3977
                                                                    (k_append_x_x_x_5706:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5705:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5705:
                                                                    int[\r111_5705. r011_5705 = r111_5705] -> X)) =
                                                              cons_1150 x1_6001 x_1581 true i_3067 false 0
                                                                k_append_x_x_x_5706
                                                            in
                                                            x_3977
                                                              (fun (x00_5712:bool) ->
                                                                 fun (x010_5712:bool) ->
                                                                   fun (x011_5712:int) ->
                                                                    fun (x10_5712:bool) ->
                                                                    fun (x110_5712:bool) ->
                                                                    fun (x111_5712:int) ->
                                                                    k_append_x_x_5681 x010_5712 x011_5712)
                                                          in
                                                          let rec
                                                            x_x_3621 (x_3593:int) (y_3594:int) 
                                                                    (k_append_x_x_x_5718:(
                                                                    bool -> 
                                                                    int -> bool -> int -> X)) =
                                                            let
                                                              x_3991
                                                                    (k_append_x_x_x_x_5743:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5742:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5742:
                                                                    int[\r111_5742. r011_5742 = r111_5742] -> X)) =
                                                              cons_1150 x1_6001 x_1581 true x_3593 false 0
                                                                k_append_x_x_x_x_5743
                                                            in
                                                            x_3991
                                                              (fun (x00_5786:bool) ->
                                                                 fun (x010_5786:bool) ->
                                                                   fun (x011_5786:int) ->
                                                                    fun (x10_5786:bool) ->
                                                                    fun (x110_5786:bool) ->
                                                                    fun (x111_5786:int) ->
                                                                    (let
                                                                    x_4006
                                                                     (k_append_x_x_x_x_5773:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5772:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5772:
                                                                    int[\r111_5772. r011_5772 = r111_5772] -> X)) =
                                                                    x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773
                                                                    in
                                                                    x_4006
                                                                    (fun (x00_5785:bool) ->
                                                                    fun (x010_5785:bool) ->
                                                                    fun (x011_5785:int) ->
                                                                    fun (x10_5785:bool) ->
                                                                    fun (x110_5785:bool) ->
                                                                    fun (x111_5785:int) ->
                                                                    k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785
                                                                    x111_5785)))
                                                          in
                                                          let
                                                            x_1589 (ii00_3043:bool) (ii01_3043:int) (ii10_3043:bool) 
                                                                  (ii11_3043:int) 
                                                                  (k_append_x_x_5827:(
                                                                  bool ->
                                                                    bool ->
                                                                    r011_5824:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5824:
                                                                    int[\r111_5824. r011_5824 = r111_5824] -> X)) =
                                                            if ii00_3043 = false then
                                                              let
                                                                x_4096
                                                                 (k_append_x_x_x_5862:(bool -> bool -> int -> X)) =
                                                                if ii10_3043 = false then
                                                                  k_append_x_x_x_5862 false true 0
                                                                else
                                                                  let
                                                                    x_4102
                                                                     (k_append_x_x_x_x_5848:(bool -> int -> X)) =
                                                                    x_1582 ii11_3043 k_append_x_x_x_x_5848
                                                                  in
                                                                  x_4102
                                                                    (
                                                                    fun (x0_5860:bool) ->
                                                                    fun (x1_5860:int) ->
                                                                    k_append_x_x_x_5862 true x0_5860 x1_5860)
                                                              in
                                                              x_4096
                                                                (fun (x0_5883:bool) ->
                                                                   fun (x10_5883:bool) ->
                                                                    fun (x11_5883:int) ->
                                                                    k_append_x_x_5827 false true 0 x0_5883 x10_5883
                                                                    x11_5883)
                                                            else
                                                              if ii10_3043 = false then
                                                                let x_4061
                                                                   (k_append_x_x_x_5890:(bool -> int -> X)) =
                                                                  x_1585 ii01_3043 k_append_x_x_x_5890
                                                                in
                                                                x_4061
                                                                  (fun (x0_5920:bool) ->
                                                                    fun (x1_5920:int) ->
                                                                    k_append_x_x_5827 true x0_5920 x1_5920 false true 0)
                                                              else
                                                                let
                                                                  x_4037
                                                                   (k_append_x_x_x_5928:(
                                                                  bool -> 
                                                                    int -> bool -> int -> X)) =
                                                                  x_x_3621 ii01_3043 ii11_3043 k_append_x_x_x_5928
                                                                in
                                                                x_4037
                                                                  (fun (x00_5952:bool) ->
                                                                    fun (x01_5952:int) ->
                                                                    fun (x10_5952:bool) ->
                                                                    fun (x11_5952:int) ->
                                                                    k_append_x_x_5827 true x00_5952 x01_5952 true
                                                                    x10_5952 x11_5952)
                                                          in
                                                          x_1589))))))
                          else
                            let x_1551 (k_append_x_x_6015:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                            x_1551
                              (fun (x_6175:(int -> (bool -> int -> X) -> X)) ->
                                 k_append_x_4744
                                   (let
                                      x_1554 (ii00_2532:bool) (ii01_2532:int) (ii10_2532:bool) (ii11_2532:int) 
                                            (k_append_x_x_6023:(bool ->
                                                                  bool ->
                                                                    r011_6020:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_6020:
                                                                    int[\r111_6020. r011_6020 = r111_6020] -> X)) =
                                      if ii00_2532 = false then
                                        let x_3701 (k_append_x_x_x_6058:(bool -> bool -> int -> X)) =
                                          if ii10_2532 = false then
                                            k_append_x_x_x_6058 false true 0
                                          else
                                            let x_3707 (k_append_x_x_x_x_6044:(bool -> int -> X)) =
                                              ys_1010 ii11_2532 k_append_x_x_x_x_6044
                                            in
                                            x_3707
                                              (fun (x0_6056:bool) ->
                                                 fun (x1_6056:int) -> k_append_x_x_x_6058 true x0_6056 x1_6056)
                                        in
                                        x_3701
                                          (fun (x0_6079:bool) ->
                                             fun (x10_6079:bool) ->
                                               fun (x11_6079:int) ->
                                                 k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079)
                                      else
                                        if ii10_2532 = false then
                                          let x_3666 (k_append_x_x_x_6086:(bool -> int -> X)) =
                                            x_6175 ii01_2532 k_append_x_x_x_6086
                                          in
                                          x_3666
                                            (fun (x0_6116:bool) ->
                                               fun (x1_6116:int) -> k_append_x_x_6023 true x0_6116 x1_6116 false true 0)
                                        else
                                          let x_3643 (k_append_x_x_x_6123:(bool -> int -> X)) =
                                            x_6175 ii01_2532 k_append_x_x_x_6123
                                          in
                                          x_3643
                                            (fun (x0_6160:bool) ->
                                               fun (x1_6160:int) ->
                                                 (let x_3653 (k_append_x_x_x_6135:(bool -> int -> X)) =
                                                    ys_1010 ii11_2532 k_append_x_x_x_6135
                                                  in
                                                  x_3653
                                                    (fun (x0_6159:bool) ->
                                                       fun (x1_6159:int) ->
                                                         k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159)))
                                    in
                                    x_1554))))))
      in
      let
        x_1595 (ysi00_2311:bool) (ysi01_2311:(int -> (bool -> int -> X) -> X)) (ysi10_2311:bool) (ysi11_2311:int) 
              (k_append_x_6208:(bool ->
                                  (bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_6197:int ->
                                                  bool -> bool -> r111_6197:int[\r111_6197. r011_6197 = r111_6197] -> X)
                                             -> X) -> bool -> bool -> int -> X)) =
        if ysi00_2311 = false then
          let
            x_4332 (ii00_2328:bool) (ii01_2328:int) (ii10_2328:bool) (ii11_2328:int) 
                  (k_append_x_x_6218:(bool ->
                                        bool ->
                                          r011_6217:int ->
                                            bool -> bool -> r111_6217:int[\r111_6217. r011_6217 = r111_6217] -> X)) =
            k_append_x_x_6218 true true 0 true true 0
          in
          let x_4361 (k_append_x_x_6295:(bool -> bool -> int -> X)) =
            if ysi10_2311 = false then
              k_append_x_x_6295 false true 0
            else
              let x_4367 (k_append_x_x_x_6281:(bool -> int -> X)) = xs_1009 ysi11_2311 k_append_x_x_x_6281 in
              x_4367 (fun (x0_6293:bool) -> fun (x1_6293:int) -> k_append_x_x_6295 true x0_6293 x1_6293)
          in
          x_4361
            (fun (x0_6325:bool) ->
               fun (x10_6325:bool) -> fun (x11_6325:int) -> k_append_x_6208 false x_4332 x0_6325 x10_6325 x11_6325)
        else
          if ysi10_2311 = false then
            let
              x_4306
                    (k_append_x_x_6354:((bool ->
                                           int ->
                                             bool ->
                                               int ->
                                                 (bool ->
                                                    bool ->
                                                      r011_6351:int ->
                                                        bool ->
                                                          bool -> r111_6351:int[\r111_6351. r011_6351 = r111_6351] -> X)
                                                   -> X) -> X)) = x_1542 ysi01_2311 k_append_x_x_6354
            in
            x_4306
              (fun (x_6414:(bool ->
                              int ->
                                bool ->
                                  int ->
                                    (bool ->
                                       bool ->
                                         r011_6412:int ->
                                           bool -> bool -> r111_6412:int[\r111_6412. r011_6412 = r111_6412] -> X) -> X)) ->
                 k_append_x_6208 true x_6414 false true 0)
          else
            let
              x_4283
                    (k_append_x_x_6436:((bool ->
                                           int ->
                                             bool ->
                                               int ->
                                                 (bool ->
                                                    bool ->
                                                      r011_6433:int ->
                                                        bool ->
                                                          bool -> r111_6433:int[\r111_6433. r011_6433 = r111_6433] -> X)
                                                   -> X) -> X)) = x_1542 ysi01_2311 k_append_x_x_6436
            in
            x_4283
              (fun (x_6509:(bool ->
                              int ->
                                bool ->
                                  int ->
                                    (bool ->
                                       bool ->
                                         r011_6507:int ->
                                           bool -> bool -> r111_6507:int[\r111_6507. r011_6507 = r111_6507] -> X) -> X)) ->
                 (let x_4293 (k_append_x_x_6466:(bool -> int -> X)) = xs_1009 ysi11_2311 k_append_x_x_6466 in
                  x_4293 (fun (x0_6505:bool) -> fun (x1_6505:int) -> k_append_x_6208 true x_6509 true x0_6505 x1_6505)))
      in
      x_1595)
 in
 let rec make_list_1013 (n_1014:int) (k_make_list_6554:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1014 < 0 then
     k_make_list_6554 (fun (x_1221:int) -> fun (k_make_list_6556:(bool -> int -> X)) -> k_make_list_6556 false 0)
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> (bool -> int -> X) -> X)) =
       let x_1598 (i_1210:int) (k_make_list_cons_x_6575:(bool -> int -> X)) =
         if i_1210 = 0 then
           k_make_list_cons_x_6575 true x_1211
         else
           let x_4398 (k_make_list_cons_x_x_6588:(bool -> int -> X)) = xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588 in
           x_4398 (fun (x0_6613:bool) -> fun (x1_6613:int) -> k_make_list_cons_x_6575 x0_6613 x1_6613)
       in
       let
         x_1612 (ii00_2110:bool) (ii01_2110:int) (ii10_2110:bool) (ii11_2110:int) 
               (k_make_list_cons_x_6622:(bool ->
                                           bool ->
                                             r011_6619:int ->
                                               bool -> bool -> r111_6619:int[\r111_6619. r011_6619 = r111_6619] -> X)) =
         if ii00_2110 = false then
           let x_4474 (k_make_list_cons_x_x_6657:(bool -> bool -> int -> X)) =
             if ii10_2110 = false then
               k_make_list_cons_x_x_6657 false true 0
             else
               let x_4480 (k_make_list_cons_x_x_x_6643:(bool -> int -> X)) =
                 xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643
               in
               x_4480 (fun (x0_6655:bool) -> fun (x1_6655:int) -> k_make_list_cons_x_x_6657 true x0_6655 x1_6655)
           in
           x_4474
             (fun (x0_6678:bool) ->
                fun (x10_6678:bool) ->
                  fun (x11_6678:int) -> k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678)
         else
           if ii10_2110 = false then
             let x_4439 (k_make_list_cons_x_x_6685:(bool -> int -> X)) = x_1598 ii01_2110 k_make_list_cons_x_x_6685 in
             x_4439
               (fun (x0_6715:bool) -> fun (x1_6715:int) -> k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0)
           else
             let x_4416 (k_make_list_cons_x_x_6722:(bool -> int -> X)) = x_1598 ii01_2110 k_make_list_cons_x_x_6722 in
             x_4416
               (fun (x0_6759:bool) ->
                  fun (x1_6759:int) ->
                    (let x_4426 (k_make_list_cons_x_x_6734:(bool -> int -> X)) =
                       xs_1212 ii11_2110 k_make_list_cons_x_x_6734
                     in
                     x_4426
                       (fun (x0_6758:bool) ->
                          fun (x1_6758:int) -> k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758)))
       in
       x_1612
     in
     let x_4511 (k_make_list_x_6794:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1013 (n_1014 - 1) k_make_list_x_6794
     in
     x_4511
       (fun (x_6928:(int -> (bool -> int -> X) -> X)) ->
          (let x_4513 (k_make_list_x_6815:(int -> X)) = rand_int_cps () k_make_list_x_6815 in
           x_4513
             (fun (x_6924:int) ->
                k_make_list_6554
                  (let x_1619 (i_2021:int) (k_make_list_x_6853:(bool -> int -> X)) =
                     let
                       x_4532
                             (k_make_list_x_x_6878:(bool ->
                                                      bool ->
                                                        r011_6877:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_6877:int[\r111_6877. r011_6877 = r111_6877] -> X)) =
                       cons_1215 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878
                     in
                     x_4532
                       (fun (x00_6884:bool) ->
                          fun (x010_6884:bool) ->
                            fun (x011_6884:int) ->
                              fun (x10_6884:bool) ->
                                fun (x110_6884:bool) -> fun (x111_6884:int) -> k_make_list_x_6853 x010_6884 x011_6884)
                   in
                   x_1619))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_6954:(unit -> X)) =
   let x_4561 (k_main_x_6967:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1013 n_1017 k_main_x_6967 in
   x_4561
     (fun (x_7492:(int -> (bool -> int -> X) -> X)) ->
        (let f_1409 (x_1254:int) (k_main_f_6982:(bool -> int -> X)) = k_main_f_6982 false 0 in
         let
           x_4567
                 (k_main_x_7022:((bool ->
                                    (int -> (bool -> int -> X) -> X) ->
                                      bool ->
                                        int ->
                                          (bool ->
                                             (bool ->
                                                int ->
                                                  bool ->
                                                    int ->
                                                      (bool ->
                                                         bool ->
                                                           r011_7017:int ->
                                                             bool ->
                                                               bool ->
                                                                 r111_7017:int[\r111_7017. r011_7017 = r111_7017] -> X)
                                                        -> X) -> bool -> bool -> int -> X) -> X) -> X)) =
           append_1053 f_1409 k_main_x_7022
         in
         x_4567
           (fun (x_7484:(bool ->
                           (int -> (bool -> int -> X) -> X) ->
                             bool ->
                               int ->
                                 (bool ->
                                    (bool ->
                                       int ->
                                         bool ->
                                           int ->
                                             (bool ->
                                                bool ->
                                                  r011_7480:int ->
                                                    bool ->
                                                      bool -> r111_7480:int[\r111_7480. r011_7480 = r111_7480] -> X) ->
                                               X) -> bool -> bool -> int -> X) -> X)) ->
              (let
                 x_4625
                       (k_main_x_7295:(bool ->
                                         (bool ->
                                            int ->
                                              bool ->
                                                int ->
                                                  (bool ->
                                                     bool ->
                                                       r011_7292:int ->
                                                         bool ->
                                                           bool ->
                                                             r111_7292:int[\r111_7292. r011_7292 = r111_7292] -> X) ->
                                                    X) -> bool -> bool -> int -> X)) =
                 x_7484 true x_7492 false 0 k_main_x_7295
               in
               x_4625
                 (fun (x00_7460:bool) ->
                    fun (x01_7460:(bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_7458:int ->
                                                  bool -> bool -> r111_7458:int[\r111_7458. r011_7458 = r111_7458] -> X)
                                             -> X)) ->
                      fun (x10_7460:bool) ->
                        fun (x110_7460:bool) ->
                          fun (x111_7460:int) ->
                            (let
                               x_4680
                                     (k_main_x_7413:(bool ->
                                                       bool ->
                                                         r011_7412:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_7412:int[\r111_7412. r011_7412 = r111_7412] -> X)) =
                               x01_7460 true i_1016 true i_1016 k_main_x_7413
                             in
                             x_4680
                               (fun (x00_7448:bool) ->
                                  fun (x010_7448:bool) ->
                                    fun (x011_7448:int) ->
                                      fun (x10_7448:bool) ->
                                        fun (x110_7448:bool) ->
                                          fun (x111_7448:int) ->
                                            (let n_1432 (k_main_n_7424:(int -> X)) =
                                               if x010_7448 <> false then
                                                 k_main_n_7424 x011_7448
                                               else
                                                 _|_
                                             in
                                             n_1432
                                               (fun (n_7447:int) ->
                                                  (let n_1433 (k_main_n_7432:(int -> X)) =
                                                     if x110_7448 <> false then
                                                       k_main_n_7432 x111_7448
                                                     else
                                                       _|_
                                                   in
                                                   n_1433
                                                     (fun (n_7446:int) ->
                                                        (if n_7447 = n_7446 then
                                                           k_main_6954 ()
                                                         else
                                                           {|fail|} () k_main_6954))))))))))))
 in
 let x_4701 (k_x_7503:(int -> X)) = rand_int_cps () k_x_7503 in
 x_4701
   (fun (x_7548:int) ->
      (let x_4703 (k_x_7515:(int -> X)) = rand_int_cps () k_x_7515 in
       x_4703
         (fun (x_7547:int) ->
            (let x_4705 (k_x_7536:(unit -> X)) = main_1015 x_7548 x_7547 k_x_7536 in
             x_4705 (fun (x_7542:unit) -> {end})))))

eliminate same arguments:
 let rec
   append_1053 (xs_1009:(int -> (bool -> int -> X) -> X)) 
              (k_append_4727:((bool ->
                                 (int -> (bool -> int -> X) -> X) ->
                                   bool ->
                                     int ->
                                       (bool ->
                                          (bool ->
                                             int ->
                                               bool ->
                                                 int ->
                                                   (bool ->
                                                      bool ->
                                                        r011_4722:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_4722:int[\r111_4722. r011_4722 = r111_4722] -> X) ->
                                                     X) -> bool -> bool -> int -> X) -> X) -> X)) =
   k_append_4727
     (let
        x_1542 (ys_1010:(int -> (bool -> int -> X) -> X)) 
              (k_append_x_4744:((bool ->
                                   int ->
                                     bool ->
                                       int ->
                                         (bool ->
                                            bool ->
                                              r011_4733:int ->
                                                bool -> bool -> r111_4733:int[\r111_4733. r011_4733 = r111_4733] -> X)
                                           -> X) -> X)) =
        let x_3635 (k_append_x_x_4754:(bool -> int -> X)) = xs_1009 0 k_append_x_x_4754 in
        x_3635
          (fun (x0_6191:bool) ->
             fun (x1_6191:int) ->
               (if x0_6191 = false then
                  k_append_x_4744
                    (let
                       x_1592 (ii00_3320:bool) (ii01_3320:int) (ii10_3320:bool) (ii11_3320:int) 
                             (k_append_x_x_4764:(bool ->
                                                   bool ->
                                                     r011_4761:int ->
                                                       bool ->
                                                         bool -> r111_4761:int[\r111_4761. r011_4761 = r111_4761] -> X)) =
                       let k_append_x_x_4771 (b_7709:bool) =
                         if b_7709 then
                           let x_7786 (k_append_x_x_x_7787:(bool -> int -> X)) = ys_1010 ii01_3320 k_append_x_x_x_7787 in
                           x_7786
                             (fun (x0_7790:bool) ->
                                fun (x1_7790:int) -> k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790)
                         else
                           if ii00_3320 = false then
                             let x_7756 (k_append_x_x_x_7757:(bool -> bool -> int -> X)) =
                               if ii10_3320 = false then
                                 k_append_x_x_x_7757 false true 0
                               else
                                 let x_7761 (k_append_x_x_x_x_7762:(bool -> int -> X)) =
                                   ys_1010 ii11_3320 k_append_x_x_x_x_7762
                                 in
                                 x_7761
                                   (fun (x0_7765:bool) ->
                                      fun (x1_7765:int) -> k_append_x_x_x_7757 true x0_7765 x1_7765)
                             in
                             x_7756
                               (fun (x0_7775:bool) ->
                                  fun (x10_7775:bool) ->
                                    fun (x11_7775:int) -> k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775)
                           else
                             if ii10_3320 = false then
                               let x_7738 (k_append_x_x_x_7739:(bool -> int -> X)) =
                                 ys_1010 ii01_3320 k_append_x_x_x_7739
                               in
                               x_7738
                                 (fun (x0_7742:bool) ->
                                    fun (x1_7742:int) -> k_append_x_x_4764 true x0_7742 x1_7742 false true 0)
                             else
                               let x_7717 (k_append_x_x_x_7718:(bool -> int -> X)) =
                                 ys_1010 ii01_3320 k_append_x_x_x_7718
                               in
                               x_7717
                                 (fun (x0_7721:bool) ->
                                    fun (x1_7721:int) ->
                                      (let x_7723 (k_append_x_x_x_7724:(bool -> int -> X)) =
                                         ys_1010 ii11_3320 k_append_x_x_x_7724
                                       in
                                       x_7723
                                         (fun (x0_7727:bool) ->
                                            fun (x1_7727:int) ->
                                              k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727)))
                       in
                       let k_append_x_x_4767 (b_7708:bool) =
                         if b_7708 then
                           k_append_x_x_4771 (ii01_3320 = ii11_3320)
                         else
                           k_append_x_x_4771 false
                       in
                       if ii00_3320 <> false then
                         k_append_x_x_4767 (ii10_3320 <> false)
                       else
                         k_append_x_x_4767 false
                     in
                     x_1592)
                else
                  let x_3636 (k_append_x_x_4965:(bool -> int -> X)) = xs_1009 0 k_append_x_x_4965 in
                  x_3636
                    (fun (x0_6183:bool) ->
                       fun (x1_6183:int) ->
                         (if x0_6183 <> false then
                            let xs'_1012 (x_1087:int) (k_append_x_xs'_4977:(bool -> int -> X)) =
                              let x_3738 (k_append_x_xs'_x_4984:(bool -> int -> X)) =
                                xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984
                              in
                              x_3738 (fun (x0_5009:bool) -> fun (x1_5009:int) -> k_append_x_xs'_4977 x0_5009 x1_5009)
                            in
                            let x_3744 (k_append_x_x_5022:(bool -> int -> X)) = xs_1009 0 k_append_x_x_5022 in
                            x_3744
                              (fun (x0_6001:bool) ->
                                 fun (x1_6001:int) ->
                                   (let cons_1150 (x_1146:int) (xs_1147:(int -> (bool -> int -> X) -> X)) =
                                      let x_1562 (i_1145:int) (k_append_x_cons_x_5040:(bool -> int -> X)) =
                                        if i_1145 = 0 then
                                          k_append_x_cons_x_5040 true x_1146
                                        else
                                          let x_3747 (k_append_x_cons_x_x_5053:(bool -> int -> X)) =
                                            xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053
                                          in
                                          x_3747
                                            (fun (x0_5078:bool) ->
                                               fun (x1_5078:int) -> k_append_x_cons_x_5040 x0_5078 x1_5078)
                                      in
                                      let
                                        x_1576 (ii00_3219:bool) (ii01_3219:int) (ii10_3219:bool) (ii11_3219:int) 
                                              (k_append_x_cons_x_5087:(
                                              bool ->
                                                bool ->
                                                  r011_5084:int ->
                                                    bool ->
                                                      bool -> r111_5084:int[\r111_5084. r011_5084 = r111_5084] -> X)) =
                                        if ii00_3219 = false then
                                          let x_3823 (k_append_x_cons_x_x_5122:(bool -> bool -> int -> X)) =
                                            if ii10_3219 = false then
                                              k_append_x_cons_x_x_5122 false true 0
                                            else
                                              let x_3829 (k_append_x_cons_x_x_x_5108:(bool -> int -> X)) =
                                                xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108
                                              in
                                              x_3829
                                                (fun (x0_5120:bool) ->
                                                   fun (x1_5120:int) -> k_append_x_cons_x_x_5122 true x0_5120 x1_5120)
                                          in
                                          x_3823
                                            (fun (x0_5143:bool) ->
                                               fun (x10_5143:bool) ->
                                                 fun (x11_5143:int) ->
                                                   k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143)
                                        else
                                          if ii10_3219 = false then
                                            let x_3788 (k_append_x_cons_x_x_5150:(bool -> int -> X)) =
                                              x_1562 ii01_3219 k_append_x_cons_x_x_5150
                                            in
                                            x_3788
                                              (fun (x0_5180:bool) ->
                                                 fun (x1_5180:int) ->
                                                   k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0)
                                          else
                                            let x_3765 (k_append_x_cons_x_x_5187:(bool -> int -> X)) =
                                              x_1562 ii01_3219 k_append_x_cons_x_x_5187
                                            in
                                            x_3765
                                              (fun (x0_5224:bool) ->
                                                 fun (x1_5224:int) ->
                                                   (let x_3775 (k_append_x_cons_x_x_5199:(bool -> int -> X)) =
                                                      xs_1147 ii11_3219 k_append_x_cons_x_x_5199
                                                    in
                                                    x_3775
                                                      (fun (x0_5223:bool) ->
                                                         fun (x1_5223:int) ->
                                                           k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223
                                                             x1_5223)))
                                      in
                                      x_1576
                                    in
                                    let
                                      x_3858
                                            (k_append_x_x_5277:((bool ->
                                                                   (int -> (bool -> int -> X) -> X) ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    (bool ->
                                                                    int ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5272:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5272:
                                                                    int[\r111_5272. r011_5272 = r111_5272] -> X) -> X)
                                                                    -> 
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                                      append_1053 xs'_1012 k_append_x_x_5277
                                    in
                                    x_3858
                                      (fun (x_5991:(bool ->
                                                      (int -> (bool -> int -> X) -> X) ->
                                                        bool ->
                                                          int ->
                                                            (bool ->
                                                               (bool ->
                                                                  int ->
                                                                    bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5987:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5987:
                                                                    int[\r111_5987. r011_5987 = r111_5987] -> X) -> X)
                                                                 -> bool -> bool -> int -> X) -> X)) ->
                                         (let
                                            x_3916
                                                  (k_append_x_x_5559:(
                                                  bool ->
                                                    (bool ->
                                                       int ->
                                                         bool ->
                                                           int ->
                                                             (bool ->
                                                                bool ->
                                                                  r011_5556:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5556:
                                                                    int[\r111_5556. r011_5556 = r111_5556] -> X) -> X)
                                                      -> bool -> bool -> int -> X)) =
                                            x_5991 true ys_1010 false 0 k_append_x_x_5559
                                          in
                                          x_3916
                                            (fun (x00_5967:bool) ->
                                               fun (x01_5967:(bool ->
                                                                int ->
                                                                  bool ->
                                                                    int ->
                                                                    (bool ->
                                                                    bool ->
                                                                    r011_5965:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5965:
                                                                    int[\r111_5965. r011_5965 = r111_5965] -> X) -> X)) ->
                                                 fun (x10_5967:bool) ->
                                                   fun (x110_5967:bool) ->
                                                     fun (x111_5967:int) ->
                                                       k_append_x_4744
                                                         (let
                                                            x_1581 (i_3083:int) (k_append_x_x_5581:(bool -> int -> X)) =
                                                            let
                                                              x_3936
                                                                    (k_append_x_x_x_5606:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5605:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5605:
                                                                    int[\r111_5605. r011_5605 = r111_5605] -> X)) =
                                                              x01_5967 true i_3083 false 0 k_append_x_x_x_5606
                                                            in
                                                            x_3936
                                                              (fun (x00_5612:bool) ->
                                                                 fun (x010_5612:bool) ->
                                                                   fun (x011_5612:int) ->
                                                                    fun (x10_5612:bool) ->
                                                                    fun (x110_5612:bool) ->
                                                                    fun (x111_5612:int) ->
                                                                    k_append_x_x_5581 x010_5612 x011_5612)
                                                          in
                                                          let
                                                            x_1582 (i_3076:int) (k_append_x_x_5617:(bool -> int -> X)) =
                                                            let
                                                              x_3955
                                                                    (k_append_x_x_x_5642:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5641:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5641:
                                                                    int[\r111_5641. r011_5641 = r111_5641] -> X)) =
                                                              x01_5967 false 0 true i_3076 k_append_x_x_x_5642
                                                            in
                                                            x_3955
                                                              (fun (x00_5648:bool) ->
                                                                 fun (x010_5648:bool) ->
                                                                   fun (x011_5648:int) ->
                                                                    fun (x10_5648:bool) ->
                                                                    fun (x110_5648:bool) ->
                                                                    fun (x111_5648:int) ->
                                                                    k_append_x_x_5617 x110_5648 x111_5648)
                                                          in
                                                          let
                                                            x_1585 (i_3067:int) (k_append_x_x_5681:(bool -> int -> X)) =
                                                            let
                                                              x_3977
                                                                    (k_append_x_x_x_5706:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5705:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5705:
                                                                    int[\r111_5705. r011_5705 = r111_5705] -> X)) =
                                                              cons_1150 x1_6001 x_1581 true i_3067 false 0
                                                                k_append_x_x_x_5706
                                                            in
                                                            x_3977
                                                              (fun (x00_5712:bool) ->
                                                                 fun (x010_5712:bool) ->
                                                                   fun (x011_5712:int) ->
                                                                    fun (x10_5712:bool) ->
                                                                    fun (x110_5712:bool) ->
                                                                    fun (x111_5712:int) ->
                                                                    k_append_x_x_5681 x010_5712 x011_5712)
                                                          in
                                                          let rec
                                                            x_x_3621 (x_3593:int) (y_3594:int) 
                                                                    (k_append_x_x_x_5718:(
                                                                    bool -> 
                                                                    int -> bool -> int -> X)) =
                                                            let
                                                              x_3991
                                                                    (k_append_x_x_x_x_5743:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5742:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5742:
                                                                    int[\r111_5742. r011_5742 = r111_5742] -> X)) =
                                                              cons_1150 x1_6001 x_1581 true x_3593 false 0
                                                                k_append_x_x_x_x_5743
                                                            in
                                                            x_3991
                                                              (fun (x00_5786:bool) ->
                                                                 fun (x010_5786:bool) ->
                                                                   fun (x011_5786:int) ->
                                                                    fun (x10_5786:bool) ->
                                                                    fun (x110_5786:bool) ->
                                                                    fun (x111_5786:int) ->
                                                                    (let
                                                                    x_4006
                                                                     (k_append_x_x_x_x_5773:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_5772:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5772:
                                                                    int[\r111_5772. r011_5772 = r111_5772] -> X)) =
                                                                    x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773
                                                                    in
                                                                    x_4006
                                                                    (fun (x00_5785:bool) ->
                                                                    fun (x010_5785:bool) ->
                                                                    fun (x011_5785:int) ->
                                                                    fun (x10_5785:bool) ->
                                                                    fun (x110_5785:bool) ->
                                                                    fun (x111_5785:int) ->
                                                                    k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785
                                                                    x111_5785)))
                                                          in
                                                          let
                                                            x_1589 (ii00_3043:bool) (ii01_3043:int) (ii10_3043:bool) 
                                                                  (ii11_3043:int) 
                                                                  (k_append_x_x_5827:(
                                                                  bool ->
                                                                    bool ->
                                                                    r011_5824:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_5824:
                                                                    int[\r111_5824. r011_5824 = r111_5824] -> X)) =
                                                            if ii00_3043 = false then
                                                              let
                                                                x_4096
                                                                 (k_append_x_x_x_5862:(bool -> bool -> int -> X)) =
                                                                if ii10_3043 = false then
                                                                  k_append_x_x_x_5862 false true 0
                                                                else
                                                                  let
                                                                    x_4102
                                                                     (k_append_x_x_x_x_5848:(bool -> int -> X)) =
                                                                    x_1582 ii11_3043 k_append_x_x_x_x_5848
                                                                  in
                                                                  x_4102
                                                                    (
                                                                    fun (x0_5860:bool) ->
                                                                    fun (x1_5860:int) ->
                                                                    k_append_x_x_x_5862 true x0_5860 x1_5860)
                                                              in
                                                              x_4096
                                                                (fun (x0_5883:bool) ->
                                                                   fun (x10_5883:bool) ->
                                                                    fun (x11_5883:int) ->
                                                                    k_append_x_x_5827 false true 0 x0_5883 x10_5883
                                                                    x11_5883)
                                                            else
                                                              if ii10_3043 = false then
                                                                let x_4061
                                                                   (k_append_x_x_x_5890:(bool -> int -> X)) =
                                                                  x_1585 ii01_3043 k_append_x_x_x_5890
                                                                in
                                                                x_4061
                                                                  (fun (x0_5920:bool) ->
                                                                    fun (x1_5920:int) ->
                                                                    k_append_x_x_5827 true x0_5920 x1_5920 false true 0)
                                                              else
                                                                let
                                                                  x_4037
                                                                   (k_append_x_x_x_5928:(
                                                                  bool -> 
                                                                    int -> bool -> int -> X)) =
                                                                  x_x_3621 ii01_3043 ii11_3043 k_append_x_x_x_5928
                                                                in
                                                                x_4037
                                                                  (fun (x00_5952:bool) ->
                                                                    fun (x01_5952:int) ->
                                                                    fun (x10_5952:bool) ->
                                                                    fun (x11_5952:int) ->
                                                                    k_append_x_x_5827 true x00_5952 x01_5952 true
                                                                    x10_5952 x11_5952)
                                                          in
                                                          x_1589))))))
                          else
                            let x_1551 (k_append_x_x_6015:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                            x_1551
                              (fun (x_6175:(int -> (bool -> int -> X) -> X)) ->
                                 k_append_x_4744
                                   (let
                                      x_1554 (ii00_2532:bool) (ii01_2532:int) (ii10_2532:bool) (ii11_2532:int) 
                                            (k_append_x_x_6023:(bool ->
                                                                  bool ->
                                                                    r011_6020:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_6020:
                                                                    int[\r111_6020. r011_6020 = r111_6020] -> X)) =
                                      if ii00_2532 = false then
                                        let x_3701 (k_append_x_x_x_6058:(bool -> bool -> int -> X)) =
                                          if ii10_2532 = false then
                                            k_append_x_x_x_6058 false true 0
                                          else
                                            let x_3707 (k_append_x_x_x_x_6044:(bool -> int -> X)) =
                                              ys_1010 ii11_2532 k_append_x_x_x_x_6044
                                            in
                                            x_3707
                                              (fun (x0_6056:bool) ->
                                                 fun (x1_6056:int) -> k_append_x_x_x_6058 true x0_6056 x1_6056)
                                        in
                                        x_3701
                                          (fun (x0_6079:bool) ->
                                             fun (x10_6079:bool) ->
                                               fun (x11_6079:int) ->
                                                 k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079)
                                      else
                                        if ii10_2532 = false then
                                          let x_3666 (k_append_x_x_x_6086:(bool -> int -> X)) =
                                            x_6175 ii01_2532 k_append_x_x_x_6086
                                          in
                                          x_3666
                                            (fun (x0_6116:bool) ->
                                               fun (x1_6116:int) -> k_append_x_x_6023 true x0_6116 x1_6116 false true 0)
                                        else
                                          let x_3643 (k_append_x_x_x_6123:(bool -> int -> X)) =
                                            x_6175 ii01_2532 k_append_x_x_x_6123
                                          in
                                          x_3643
                                            (fun (x0_6160:bool) ->
                                               fun (x1_6160:int) ->
                                                 (let x_3653 (k_append_x_x_x_6135:(bool -> int -> X)) =
                                                    ys_1010 ii11_2532 k_append_x_x_x_6135
                                                  in
                                                  x_3653
                                                    (fun (x0_6159:bool) ->
                                                       fun (x1_6159:int) ->
                                                         k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159)))
                                    in
                                    x_1554))))))
      in
      let
        x_1595 (ysi00_2311:bool) (ysi01_2311:(int -> (bool -> int -> X) -> X)) (ysi10_2311:bool) (ysi11_2311:int) 
              (k_append_x_6208:(bool ->
                                  (bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_6197:int ->
                                                  bool -> bool -> r111_6197:int[\r111_6197. r011_6197 = r111_6197] -> X)
                                             -> X) -> bool -> bool -> int -> X)) =
        if ysi00_2311 = false then
          let
            x_4332 (ii00_2328:bool) (ii01_2328:int) (ii10_2328:bool) (ii11_2328:int) 
                  (k_append_x_x_6218:(bool ->
                                        bool ->
                                          r011_6217:int ->
                                            bool -> bool -> r111_6217:int[\r111_6217. r011_6217 = r111_6217] -> X)) =
            k_append_x_x_6218 true true 0 true true 0
          in
          let x_4361 (k_append_x_x_6295:(bool -> bool -> int -> X)) =
            if ysi10_2311 = false then
              k_append_x_x_6295 false true 0
            else
              let x_4367 (k_append_x_x_x_6281:(bool -> int -> X)) = xs_1009 ysi11_2311 k_append_x_x_x_6281 in
              x_4367 (fun (x0_6293:bool) -> fun (x1_6293:int) -> k_append_x_x_6295 true x0_6293 x1_6293)
          in
          x_4361
            (fun (x0_6325:bool) ->
               fun (x10_6325:bool) -> fun (x11_6325:int) -> k_append_x_6208 false x_4332 x0_6325 x10_6325 x11_6325)
        else
          if ysi10_2311 = false then
            let
              x_4306
                    (k_append_x_x_6354:((bool ->
                                           int ->
                                             bool ->
                                               int ->
                                                 (bool ->
                                                    bool ->
                                                      r011_6351:int ->
                                                        bool ->
                                                          bool -> r111_6351:int[\r111_6351. r011_6351 = r111_6351] -> X)
                                                   -> X) -> X)) = x_1542 ysi01_2311 k_append_x_x_6354
            in
            x_4306
              (fun (x_6414:(bool ->
                              int ->
                                bool ->
                                  int ->
                                    (bool ->
                                       bool ->
                                         r011_6412:int ->
                                           bool -> bool -> r111_6412:int[\r111_6412. r011_6412 = r111_6412] -> X) -> X)) ->
                 k_append_x_6208 true x_6414 false true 0)
          else
            let
              x_4283
                    (k_append_x_x_6436:((bool ->
                                           int ->
                                             bool ->
                                               int ->
                                                 (bool ->
                                                    bool ->
                                                      r011_6433:int ->
                                                        bool ->
                                                          bool -> r111_6433:int[\r111_6433. r011_6433 = r111_6433] -> X)
                                                   -> X) -> X)) = x_1542 ysi01_2311 k_append_x_x_6436
            in
            x_4283
              (fun (x_6509:(bool ->
                              int ->
                                bool ->
                                  int ->
                                    (bool ->
                                       bool ->
                                         r011_6507:int ->
                                           bool -> bool -> r111_6507:int[\r111_6507. r011_6507 = r111_6507] -> X) -> X)) ->
                 (let x_4293 (k_append_x_x_6466:(bool -> int -> X)) = xs_1009 ysi11_2311 k_append_x_x_6466 in
                  x_4293 (fun (x0_6505:bool) -> fun (x1_6505:int) -> k_append_x_6208 true x_6509 true x0_6505 x1_6505)))
      in
      x_1595)
 in
 let rec make_list_1013 (n_1014:int) (k_make_list_6554:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1014 < 0 then
     k_make_list_6554 (fun (x_1221:int) -> fun (k_make_list_6556:(bool -> int -> X)) -> k_make_list_6556 false 0)
   else
     let cons_1215 (x_1211:int) (xs_1212:(int -> (bool -> int -> X) -> X)) =
       let x_1598 (i_1210:int) (k_make_list_cons_x_6575:(bool -> int -> X)) =
         if i_1210 = 0 then
           k_make_list_cons_x_6575 true x_1211
         else
           let x_4398 (k_make_list_cons_x_x_6588:(bool -> int -> X)) = xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588 in
           x_4398 (fun (x0_6613:bool) -> fun (x1_6613:int) -> k_make_list_cons_x_6575 x0_6613 x1_6613)
       in
       let
         x_1612 (ii00_2110:bool) (ii01_2110:int) (ii10_2110:bool) (ii11_2110:int) 
               (k_make_list_cons_x_6622:(bool ->
                                           bool ->
                                             r011_6619:int ->
                                               bool -> bool -> r111_6619:int[\r111_6619. r011_6619 = r111_6619] -> X)) =
         if ii00_2110 = false then
           let x_4474 (k_make_list_cons_x_x_6657:(bool -> bool -> int -> X)) =
             if ii10_2110 = false then
               k_make_list_cons_x_x_6657 false true 0
             else
               let x_4480 (k_make_list_cons_x_x_x_6643:(bool -> int -> X)) =
                 xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643
               in
               x_4480 (fun (x0_6655:bool) -> fun (x1_6655:int) -> k_make_list_cons_x_x_6657 true x0_6655 x1_6655)
           in
           x_4474
             (fun (x0_6678:bool) ->
                fun (x10_6678:bool) ->
                  fun (x11_6678:int) -> k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678)
         else
           if ii10_2110 = false then
             let x_4439 (k_make_list_cons_x_x_6685:(bool -> int -> X)) = x_1598 ii01_2110 k_make_list_cons_x_x_6685 in
             x_4439
               (fun (x0_6715:bool) -> fun (x1_6715:int) -> k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0)
           else
             let x_4416 (k_make_list_cons_x_x_6722:(bool -> int -> X)) = x_1598 ii01_2110 k_make_list_cons_x_x_6722 in
             x_4416
               (fun (x0_6759:bool) ->
                  fun (x1_6759:int) ->
                    (let x_4426 (k_make_list_cons_x_x_6734:(bool -> int -> X)) =
                       xs_1212 ii11_2110 k_make_list_cons_x_x_6734
                     in
                     x_4426
                       (fun (x0_6758:bool) ->
                          fun (x1_6758:int) -> k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758)))
       in
       x_1612
     in
     let x_4511 (k_make_list_x_6794:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1013 (n_1014 - 1) k_make_list_x_6794
     in
     x_4511
       (fun (x_6928:(int -> (bool -> int -> X) -> X)) ->
          (let x_4513 (k_make_list_x_6815:(int -> X)) = rand_int_cps () k_make_list_x_6815 in
           x_4513
             (fun (x_6924:int) ->
                k_make_list_6554
                  (let x_1619 (i_2021:int) (k_make_list_x_6853:(bool -> int -> X)) =
                     let
                       x_4532
                             (k_make_list_x_x_6878:(bool ->
                                                      bool ->
                                                        r011_6877:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_6877:int[\r111_6877. r011_6877 = r111_6877] -> X)) =
                       cons_1215 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878
                     in
                     x_4532
                       (fun (x00_6884:bool) ->
                          fun (x010_6884:bool) ->
                            fun (x011_6884:int) ->
                              fun (x10_6884:bool) ->
                                fun (x110_6884:bool) -> fun (x111_6884:int) -> k_make_list_x_6853 x010_6884 x011_6884)
                   in
                   x_1619))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_6954:(unit -> X)) =
   let x_4561 (k_main_x_6967:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1013 n_1017 k_main_x_6967 in
   x_4561
     (fun (x_7492:(int -> (bool -> int -> X) -> X)) ->
        (let f_1409 (x_1254:int) (k_main_f_6982:(bool -> int -> X)) = k_main_f_6982 false 0 in
         let
           x_4567
                 (k_main_x_7022:((bool ->
                                    (int -> (bool -> int -> X) -> X) ->
                                      bool ->
                                        int ->
                                          (bool ->
                                             (bool ->
                                                int ->
                                                  bool ->
                                                    int ->
                                                      (bool ->
                                                         bool ->
                                                           r011_7017:int ->
                                                             bool ->
                                                               bool ->
                                                                 r111_7017:int[\r111_7017. r011_7017 = r111_7017] -> X)
                                                        -> X) -> bool -> bool -> int -> X) -> X) -> X)) =
           append_1053 f_1409 k_main_x_7022
         in
         x_4567
           (fun (x_7484:(bool ->
                           (int -> (bool -> int -> X) -> X) ->
                             bool ->
                               int ->
                                 (bool ->
                                    (bool ->
                                       int ->
                                         bool ->
                                           int ->
                                             (bool ->
                                                bool ->
                                                  r011_7480:int ->
                                                    bool ->
                                                      bool -> r111_7480:int[\r111_7480. r011_7480 = r111_7480] -> X) ->
                                               X) -> bool -> bool -> int -> X) -> X)) ->
              (let
                 x_4625
                       (k_main_x_7295:(bool ->
                                         (bool ->
                                            int ->
                                              bool ->
                                                int ->
                                                  (bool ->
                                                     bool ->
                                                       r011_7292:int ->
                                                         bool ->
                                                           bool ->
                                                             r111_7292:int[\r111_7292. r011_7292 = r111_7292] -> X) ->
                                                    X) -> bool -> bool -> int -> X)) =
                 x_7484 true x_7492 false 0 k_main_x_7295
               in
               x_4625
                 (fun (x00_7460:bool) ->
                    fun (x01_7460:(bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_7458:int ->
                                                  bool -> bool -> r111_7458:int[\r111_7458. r011_7458 = r111_7458] -> X)
                                             -> X)) ->
                      fun (x10_7460:bool) ->
                        fun (x110_7460:bool) ->
                          fun (x111_7460:int) ->
                            (let
                               x_4680
                                     (k_main_x_7413:(bool ->
                                                       bool ->
                                                         r011_7412:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_7412:int[\r111_7412. r011_7412 = r111_7412] -> X)) =
                               x01_7460 true i_1016 true i_1016 k_main_x_7413
                             in
                             x_4680
                               (fun (x00_7448:bool) ->
                                  fun (x010_7448:bool) ->
                                    fun (x011_7448:int) ->
                                      fun (x10_7448:bool) ->
                                        fun (x110_7448:bool) ->
                                          fun (x111_7448:int) ->
                                            (let n_1432 (k_main_n_7424:(int -> X)) =
                                               if x010_7448 <> false then
                                                 k_main_n_7424 x011_7448
                                               else
                                                 _|_
                                             in
                                             n_1432
                                               (fun (n_7447:int) ->
                                                  (let n_1433 (k_main_n_7432:(int -> X)) =
                                                     if x110_7448 <> false then
                                                       k_main_n_7432 x111_7448
                                                     else
                                                       _|_
                                                   in
                                                   n_1433
                                                     (fun (n_7446:int) ->
                                                        (if n_7447 = n_7446 then
                                                           k_main_6954 ()
                                                         else
                                                           {|fail|} () k_main_6954))))))))))))
 in
 let x_4701 (k_x_7503:(int -> X)) = rand_int_cps () k_x_7503 in
 x_4701
   (fun (x_7548:int) ->
      (let x_4703 (k_x_7515:(int -> X)) = rand_int_cps () k_x_7515 in
       x_4703
         (fun (x_7547:int) ->
            (let x_4705 (k_x_7536:(unit -> X)) = main_1015 x_7548 x_7547 k_x_7536 in
             x_4705 (fun (x_7542:unit) -> {end})))))

Program with abstraction types (CEGAR-cycle 0)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     int ->
                     x_25:bool[x_25] ->
                     int ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              int ->
              x_19:bool[x_19] ->
              int ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(0-1) Abstracting ... DONE!

(0-2) Checking HORS ... DONE!

Error trace::
  main_7945 ... --> 
  x_4701 ... --> 
  f_8000 ... --> 
  x_4703 ... --> 
  f_8001 ... --> 
  x_4705 ... --> 
  main_1015 ... --> 
  x_4561 ... --> 
  make_list_1013 [1/2] ... --> 
  f_main_7994 ... --> 
  x_4567 ... --> 
  append_1053 ... --> 
  f_main_7995 ... --> 
  x_4625 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  f_1409 ... --> 
  f_x_7946 [1/2] ... --> 
  f_x_7981 ... --> 
  f_main_7996 ... --> 
  x_4680 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [2/2] ... --> 
  br_k_append_x_x_8005 [2/2] ... --> 
  br_k_append_x_x_8003 [2/2] ... --> 
  x_7717 ... --> 
  f_make_list_7984 ... --> 
  f_k_append_x_x_7951 ... --> 
  x_7723 ... --> 
  f_make_list_7984 ... --> 
  f_k_append_x_x_7952 ... --> 
  f_main_7997 ... --> 
  n_1432 [1/2] ... --> 
  f_main_7998 ... --> 
  n_1433 [1/2] ... --> 
  f_main_7999 [2/2] ... --> 
  fail_8017 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  1; 0

(0-3) Checking counterexample ... DONE!

(0-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 1)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     x_24:int ->
                     x_25:bool[x_25] ->
                     x_26:int[x_26 >= x_24 && x_26 <= x_24] ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              x_18:int ->
              x_19:bool[x_19] ->
              x_20:int[x_20 >= x_18 && x_20 <= x_18] ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(1-1) Abstracting ... DONE!

(1-2) Checking HORS ... DONE!

Error trace::
  main_7945 ... --> 
  x_4701 ... --> 
  f_8000 ... --> 
  x_4703 ... --> 
  f_8001 ... --> 
  x_4705 ... --> 
  main_1015 ... --> 
  x_4561 ... --> 
  make_list_1013 [2/2] ... --> 
  x_4511 ... --> 
  make_list_1013 [1/2] ... --> 
  f_make_list_7991 ... --> 
  x_4513 ... --> 
  f_make_list_7992 ... --> 
  f_main_7994 ... --> 
  x_4567 ... --> 
  append_1053 ... --> 
  f_main_7995 ... --> 
  x_4625 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [2/2] ... --> 
  x_4283 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  f_1409 ... --> 
  f_x_7946 [2/2] ... --> 
  x_3636 ... --> 
  f_1409 ... --> 
  f_x_7953 [1/2] ... --> 
  x_3744 ... --> 
  f_1409 ... --> 
  f_x_7955 ... --> 
  x_3858 ... --> 
  append_1053 ... --> 
  f_x_7962 ... --> 
  x_3916 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  xs'_1012 ... --> 
  x_3738 ... --> 
  f_1409 ... --> 
  f_xs'_7954 ... --> 
  f_x_7946 [1/2] ... --> 
  f_x_7981 ... --> 
  f_x_7963 ... --> 
  f_x_7982 ... --> 
  x_4293 ... --> 
  f_1409 ... --> 
  f_x_7983 ... --> 
  f_main_7996 ... --> 
  x_4680 ... --> 
  x_1589 [2/2] ... --> 
  br_x_8009 [2/2] ... --> 
  x_4037 ... --> 
  x_x_3621 ... --> 
  x_3991 ... --> 
  cons_1150 ... --> 
  x_1576 [2/2] ... --> 
  br_x_8007 [2/2] ... --> 
  x_3765 ... --> 
  x_1562 [2/2] ... --> 
  x_3747 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_7964 ... --> 
  f_x_7956 ... --> 
  f_x_7960 ... --> 
  x_3775 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_7964 ... --> 
  f_x_7961 ... --> 
  f_x_x_7967 ... --> 
  x_4006 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [2/2] ... --> 
  br_k_append_x_x_8005 [2/2] ... --> 
  br_k_append_x_x_8003 [2/2] ... --> 
  x_7717 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7951 ... --> 
  x_7723 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7952 ... --> 
  f_x_x_7968 ... --> 
  f_x_7972 ... --> 
  f_main_7997 ... --> 
  n_1432 [1/2] ... --> 
  f_main_7998 ... --> 
  n_1433 [1/2] ... --> 
  f_main_7999 [2/2] ... --> 
  fail_8017 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 
  0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(1-3) Checking counterexample ... DONE!

(1-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1

Program with abstraction types (CEGAR-cycle 2)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int[(not x_18)] ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     x_24:int ->
                     x_25:bool[x_25] ->
                     x_26:int[x_26 >= x_24 && x_26 <= x_24] ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              x_18:int ->
              x_19:bool[x_19] ->
              x_20:int[x_20 >= x_18 && x_20 <= x_18] ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(2-1) Abstracting ... DONE!

(2-2) Checking HORS ... DONE!

Error trace::
  main_7945 ... --> 
  x_4701 ... --> 
  f_8000 ... --> 
  x_4703 ... --> 
  f_8001 ... --> 
  x_4705 ... --> 
  main_1015 ... --> 
  x_4561 ... --> 
  make_list_1013 [2/2] ... --> 
  x_4511 ... --> 
  make_list_1013 [1/2] ... --> 
  f_make_list_7991 ... --> 
  x_4513 ... --> 
  f_make_list_7992 ... --> 
  f_main_7994 ... --> 
  x_4567 ... --> 
  append_1053 ... --> 
  f_main_7995 ... --> 
  x_4625 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  f_1409 ... --> 
  f_x_7946 [2/2] ... --> 
  x_3636 ... --> 
  f_1409 ... --> 
  f_x_7953 [1/2] ... --> 
  x_3744 ... --> 
  f_1409 ... --> 
  f_x_7955 ... --> 
  x_3858 ... --> 
  append_1053 ... --> 
  f_x_7962 ... --> 
  x_3916 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  xs'_1012 ... --> 
  x_3738 ... --> 
  f_1409 ... --> 
  f_xs'_7954 ... --> 
  f_x_7946 [1/2] ... --> 
  f_x_7981 ... --> 
  f_x_7963 ... --> 
  f_x_7981 ... --> 
  f_main_7996 ... --> 
  x_4680 ... --> 
  x_1589 [2/2] ... --> 
  br_x_8009 [2/2] ... --> 
  x_4037 ... --> 
  x_x_3621 ... --> 
  x_3991 ... --> 
  cons_1150 ... --> 
  x_1576 [2/2] ... --> 
  br_x_8007 [2/2] ... --> 
  x_3765 ... --> 
  x_1562 [2/2] ... --> 
  x_3747 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_7964 ... --> 
  f_x_7956 ... --> 
  f_x_7960 ... --> 
  x_3775 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_7964 ... --> 
  f_x_7961 ... --> 
  f_x_x_7967 ... --> 
  x_4006 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [2/2] ... --> 
  br_k_append_x_x_8005 [2/2] ... --> 
  br_k_append_x_x_8003 [2/2] ... --> 
  x_7717 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7951 ... --> 
  x_7723 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7952 ... --> 
  f_x_x_7968 ... --> 
  f_x_7972 ... --> 
  f_main_7997 ... --> 
  n_1432 [1/2] ... --> 
  f_main_7998 ... --> 
  n_1433 [1/2] ... --> 
  f_main_7999 [2/2] ... --> 
  fail_8017 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 1; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 
  1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(2-3) Checking counterexample ... DONE!

(2-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 3)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int[(not x_18)] ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     x_24:int ->
                     x_25:bool[x_25] ->
                     x_26:int[x_26 >= x_24 && x_26 <= x_24] ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int[(not x_17)] ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              x_18:int ->
              x_19:bool[x_19] ->
              x_20:int[x_20 >= x_18 && x_20 <= x_18] ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(3-1) Abstracting ... DONE!

(3-2) Checking HORS ... DONE!

Error trace::
  main_7945 ... --> 
  x_4701 ... --> 
  f_8000 ... --> 
  x_4703 ... --> 
  f_8001 ... --> 
  x_4705 ... --> 
  main_1015 ... --> 
  x_4561 ... --> 
  make_list_1013 [2/2] ... --> 
  x_4511 ... --> 
  make_list_1013 [1/2] ... --> 
  f_make_list_7991 ... --> 
  x_4513 ... --> 
  f_make_list_7992 ... --> 
  f_main_7994 ... --> 
  x_4567 ... --> 
  append_1053 ... --> 
  f_main_7995 ... --> 
  x_4625 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  f_1409 ... --> 
  f_x_7946 [2/2] ... --> 
  x_3636 ... --> 
  f_1409 ... --> 
  f_x_7953 [1/2] ... --> 
  x_3744 ... --> 
  f_1409 ... --> 
  f_x_7955 ... --> 
  x_3858 ... --> 
  append_1053 ... --> 
  f_x_7962 ... --> 
  x_3916 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  xs'_1012 ... --> 
  x_3738 ... --> 
  f_1409 ... --> 
  f_xs'_7954 ... --> 
  f_x_7946 [1/2] ... --> 
  f_x_7981 ... --> 
  f_x_7963 ... --> 
  f_x_7981 ... --> 
  f_main_7996 ... --> 
  x_4680 ... --> 
  x_1589 [2/2] ... --> 
  br_x_8009 [2/2] ... --> 
  x_4037 ... --> 
  x_x_3621 ... --> 
  x_3991 ... --> 
  cons_1150 ... --> 
  x_1576 [2/2] ... --> 
  br_x_8007 [1/2] ... --> 
  x_3788 ... --> 
  x_1562 [2/2] ... --> 
  x_3747 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_7964 ... --> 
  f_x_7956 ... --> 
  f_x_7959 ... --> 
  f_x_x_7967 ... --> 
  x_4006 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [1/2] ... --> 
  x_7786 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7947 ... --> 
  f_x_x_7968 ... --> 
  f_x_7972 ... --> 
  f_main_7997 ... --> 
  n_1432 [1/2] ... --> 
  f_main_7998 ... --> 
  n_1433 [1/2] ... --> 
  f_main_7999 [2/2] ... --> 
  fail_8017 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(3-3) Checking counterexample ... DONE!

(3-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 4)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int[(not x_18)] ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     x_24:int ->
                     x_25:bool[x_25] ->
                     x_26:int[(not x_25); x_26 >= x_24 && x_26 <= x_24] ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int[(not x_17)] ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              x_18:int ->
              x_19:bool[x_19] ->
              x_20:int[(not x_19); x_20 >= x_18 && x_20 <= x_18] ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int[(not x_14)] ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(4-1) Abstracting ... DONE!

(4-2) Checking HORS ... DONE!

Error trace::
  main_7945 ... --> 
  x_4701 ... --> 
  f_8000 ... --> 
  x_4703 ... --> 
  f_8001 ... --> 
  x_4705 ... --> 
  main_1015 ... --> 
  x_4561 ... --> 
  make_list_1013 [2/2] ... --> 
  x_4511 ... --> 
  make_list_1013 [1/2] ... --> 
  f_make_list_7991 ... --> 
  x_4513 ... --> 
  f_make_list_7992 ... --> 
  f_main_7994 ... --> 
  x_4567 ... --> 
  append_1053 ... --> 
  f_main_7995 ... --> 
  x_4625 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  f_1409 ... --> 
  f_x_7946 [2/2] ... --> 
  x_3636 ... --> 
  f_1409 ... --> 
  f_x_7953 [1/2] ... --> 
  x_3744 ... --> 
  f_1409 ... --> 
  f_x_7955 ... --> 
  x_3858 ... --> 
  append_1053 ... --> 
  f_x_7962 ... --> 
  x_3916 ... --> 
  x_1595 [2/2] ... --> 
  br_x_8013 [1/2] ... --> 
  x_4306 ... --> 
  x_1542 ... --> 
  x_3635 ... --> 
  xs'_1012 ... --> 
  x_3738 ... --> 
  f_1409 ... --> 
  f_xs'_7954 ... --> 
  f_x_7946 [1/2] ... --> 
  f_x_7981 ... --> 
  f_x_7963 ... --> 
  f_x_7981 ... --> 
  f_main_7996 ... --> 
  x_4680 ... --> 
  x_1589 [2/2] ... --> 
  br_x_8009 [2/2] ... --> 
  x_4037 ... --> 
  x_x_3621 ... --> 
  x_3991 ... --> 
  cons_1150 ... --> 
  x_1576 [2/2] ... --> 
  br_x_8007 [1/2] ... --> 
  x_3788 ... --> 
  x_1562 [2/2] ... --> 
  x_3747 ... --> 
  x_1581 ... --> 
  x_3936 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [2/2] ... --> 
  k_append_x_x_4771 [2/2] ... --> 
  br_k_append_x_x_8005 [2/2] ... --> 
  br_k_append_x_x_8003 [1/2] ... --> 
  x_7738 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7950 ... --> 
  f_x_7964 ... --> 
  f_x_7956 ... --> 
  f_x_7959 ... --> 
  f_x_x_7967 ... --> 
  x_4006 ... --> 
  x_1592 [1/2] ... --> 
  k_append_x_x_4767 [1/2] ... --> 
  k_append_x_x_4771 [2/2] ... --> 
  br_k_append_x_x_8005 [2/2] ... --> 
  br_k_append_x_x_8003 [2/2] ... --> 
  x_7717 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7951 ... --> 
  x_7723 ... --> 
  x_1619 ... --> 
  x_4532 ... --> 
  cons_1215 ... --> 
  x_1612 [2/2] ... --> 
  br_x_8015 [1/2] ... --> 
  x_4439 ... --> 
  x_1598 [2/2] ... --> 
  x_4398 ... --> 
  f_make_list_7984 ... --> 
  f_x_7985 ... --> 
  f_x_7988 ... --> 
  f_x_7993 ... --> 
  f_k_append_x_x_7952 ... --> 
  f_x_x_7968 ... --> 
  f_x_7972 ... --> 
  f_main_7997 ... --> 
  n_1432 [1/2] ... --> 
  f_main_7998 ... --> 
  n_1433 [1/2] ... --> 
  f_main_7999 [2/2] ... --> 
  fail_8017 ... --> fail -->
  ERROR!

Spurious counterexample::
  0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 1; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0

(4-3) Checking counterexample ... DONE!

(4-4) Discovering predicates ... 
DONE!

Prefix of spurious counterexample::
0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 1

Program with abstraction types (CEGAR-cycle 5)::
Main: main_7945
  main_7945 -> (x_4701 f_8000).
  append_1053 xs_1009 k_append_4727 -> (k_append_4727 (x_1595 xs_1009)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8004 ->
      (x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8003 b_8004 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8004) ->
      (x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_8006 ->
      (x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  br_k_append_x_x_8005 b_8006 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_8006) ->
      (br_k_append_x_x_8003 (ii10_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when b_8008 ->
      (x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  br_x_8007 b_8008 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 
  ii11_3219 k_append_x_cons_x_5087 when (not b_8008) ->
      (x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087 xs_1147)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when b_8010 ->
      (x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8009 b_8010 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not b_8010) ->
      (x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when b_8012 ->
      (x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  br_x_8011 b_8012 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 
  k_append_x_x_6023 when (not b_8012) ->
      (x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175
        (f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when b_8014 ->
      (x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  br_x_8013 b_8014 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not b_8014) ->
      (x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311
        (f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when b_8016 ->
      (x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  br_x_8015 b_8016 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not b_8016) ->
      (x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212)).
  cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022 ->
      (x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 x_8018 x_8019 x_8020 x_8021 x_8022).
  cons_1215 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027 ->
      (x_1612 n_1014 x_1211 xs_1212 x_8023 x_8024 x_8025 x_8026 x_8027).
  f_1409 i_1016 n_1017 x_1254 k_main_f_6982 -> (k_main_f_6982 false 0).
  f_8000 x_7548 -> (x_4703 x_7548 (f_8001 x_7548)).
  f_8001 x_7548 x_7547 -> (x_4705 x_7547 x_7548 (f_8002 x_7547 x_7548)).
  f_8002 x_7547 x_7548 x_7542 -> end.
  f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7790 x1_7790 ->
      (k_append_x_x_4764 true x0_7790 x1_7790 true x0_7790 x1_7790).
  f_k_append_x_x_7949 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7775 
  x10_7775 x11_7775 -> (k_append_x_x_4764 false true 0 x0_7775 x10_7775 x11_7775).
  f_k_append_x_x_7950 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 x0_7742 x1_7742 ->
      (k_append_x_x_4764 true x0_7742 x1_7742 false true 0).
  f_k_append_x_x_7951 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 x0_7721 
  x1_7721 ->
      (x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010
        (f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721
          k_append_x_x_4764)).
  f_k_append_x_x_7952 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 k_append_x_x_4764 
  x0_7727 x1_7727 -> (k_append_x_x_4764 true x0_7721 x1_7721 true x0_7727 x1_7727).
  f_main_7994 i_1016 n_1017 k_main_6954 x_7492 ->
      (x_4567 i_1016 n_1017 (f_main_7995 i_1016 n_1017 k_main_6954 x_7492)).
  f_main_7995 i_1016 n_1017 k_main_6954 x_7492 x_7484 ->
      (x_4625 i_1016 n_1017 x_7484 x_7492 (f_main_7996 i_1016 n_1017 k_main_6954)).
  f_main_7996 i_1016 n_1017 k_main_6954 x00_7460 x01_7460 x10_7460 x110_7460 x111_7460 ->
      (x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460
        (f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954)).
  f_main_7997 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 k_main_6954 x00_7448 x010_7448 x011_7448 x10_7448 
  x110_7448 x111_7448 ->
      (n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448
        x111_7460
        (f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7998 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_6954 n_7447 ->
      (n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
        x111_7448 x111_7460
        (f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460
          x111_7448 x111_7460 k_main_6954)).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (n_7447 = n_7446) -> (k_main_6954 ()).
  f_main_7999 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 
  x111_7448 x111_7460 k_main_6954 n_7446 when (not (n_7447 = n_7446)) -> (
      fail_8017 true k_main_6954).
  f_make_list_7984 n_1014 x_1221 k_make_list_6556 -> (k_make_list_6556 false 0).
  f_make_list_7991 n_1014 k_make_list_6554 x_6928 -> (x_4513 n_1014 (f_make_list_7992 n_1014 k_make_list_6554 x_6928)).
  f_make_list_7992 n_1014 k_make_list_6554 x_6928 x_6924 -> (k_make_list_6554 (x_1619 n_1014 x_6924 x_6928)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (x0_6191 <=> false) ->
      (k_append_x_4744 (x_1592 x0_6191 x1_6191 ys_1010)).
  f_x_7946 k_append_x_4744 xs_1009 ys_1010 x0_6191 x1_6191 when (not (x0_6191 <=> false)) ->
      (x_3636 x0_6191 x1_6191 xs_1009 (f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757 x0_7765 x1_7765 ->
      (k_append_x_x_x_7757 true x0_7765 x1_7765).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (x0_6183 <=> false)) ->
      (x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009
        (f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010)).
  f_x_7953 x0_6191 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6183 x1_6183 when (
      not (not (x0_6183 <=> false))) ->
      (x_1551 x0_6183 x0_6191 x1_6183 x1_6191 (f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7955 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 xs_1009 ys_1010 x0_6001 x1_6001 ->
      (x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009
        (f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010)).
  f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040 x0_5078 x1_5078 ->
      (k_append_x_cons_x_5040 x0_5078 x1_5078).
  f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_x_5122 x0_5120 x1_5120 -> (k_append_x_cons_x_x_5122 true x0_5120 x1_5120).
  f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5143 x10_5143 x11_5143 -> (k_append_x_cons_x_5087 false true 0 x0_5143 x10_5143 x11_5143).
  f_x_7959 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 x0_5180 x1_5180 -> (k_append_x_cons_x_5087 true x0_5180 x1_5180 false true 0).
  f_x_7960 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 
  k_append_x_cons_x_5087 xs_1147 x0_5224 x1_5224 ->
      (x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191
        x_1146 xs_1147
        (f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183
          x1_6191 x_1146 k_append_x_cons_x_5087)).
  f_x_7961 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 k_append_x_cons_x_5087 x0_5223 x1_5223 -> (k_append_x_cons_x_5087 true x0_5224 x1_5224 true x0_5223 x1_5223).
  f_x_7962 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_5991 ->
      (x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010
        (f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744)).
  f_x_7963 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 k_append_x_4744 x00_5967 x01_5967 x10_5967 x110_5967 
  x111_5967 ->
      (k_append_x_4744
        (x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967)).
  f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5581 x00_5612 x010_5612 x011_5612 x10_5612 x110_5612 x111_5612 ->
      (k_append_x_x_5581 x010_5612 x011_5612).
  f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5617 x00_5648 x010_5648 x011_5648 x10_5648 x110_5648 x111_5648 ->
      (k_append_x_x_5617 x110_5648 x111_5648).
  f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 
  k_append_x_x_5681 x00_5712 x010_5712 x011_5712 x10_5712 x110_5712 x111_5712 ->
      (k_append_x_x_5681 x010_5712 x011_5712).
  f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862 x0_5860 x1_5860 -> (k_append_x_x_x_5862 true x0_5860 x1_5860).
  f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5883 x10_5883 x11_5883 ->
      (k_append_x_x_5827 false true 0 x0_5883 x10_5883 x11_5883).
  f_x_7971 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x0_5920 x1_5920 -> (k_append_x_x_5827 true x0_5920 x1_5920 false true 0).
  f_x_7972 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 
  x1_6001 x1_6183 x1_6191 k_append_x_x_5827 x00_5952 x01_5952 x10_5952 x11_5952 ->
      (k_append_x_x_5827 true x00_5952 x01_5952 true x10_5952 x11_5952).
  f_x_7973 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_4744 ys_1010 x_6175 ->
      (k_append_x_4744 (x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010)).
  f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058 x0_6056 x1_6056 ->
      (k_append_x_x_x_6058 true x0_6056 x1_6056).
  f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6079 x10_6079 
  x11_6079 -> (k_append_x_x_6023 false true 0 x0_6079 x10_6079 x11_6079).
  f_x_7976 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 x0_6116 x1_6116 ->
      (k_append_x_x_6023 true x0_6116 x1_6116 false true 0).
  f_x_7977 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023 ys_1010 x0_6160 
  x1_6160 ->
      (x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010
        (f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191
          k_append_x_x_6023)).
  f_x_7978 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 k_append_x_x_6023 
  x0_6159 x1_6159 -> (k_append_x_x_6023 true x0_6160 x1_6160 true x0_6159 x1_6159).
  f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295 x0_6293 x1_6293 ->
      (k_append_x_x_6295 true x0_6293 x1_6293).
  f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x0_6325 x10_6325 x11_6325 ->
      (k_append_x_6208 false (x_4332 ysi00_2311 ysi10_2311 ysi11_2311) x0_6325 x10_6325 x11_6325).
  f_x_7981 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6414 -> (k_append_x_6208 true x_6414 false true 0).
  f_x_7982 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 xs_1009 x_6509 ->
      (x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009
        (f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509)).
  f_x_7983 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208 x_6509 x0_6505 x1_6505 ->
      (k_append_x_6208 true x_6509 true x0_6505 x1_6505).
  f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575 x0_6613 x1_6613 -> (k_make_list_cons_x_6575 x0_6613 x1_6613).
  f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657 x0_6655 x1_6655 ->
      (k_make_list_cons_x_x_6657 true x0_6655 x1_6655).
  f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6678 x10_6678 x11_6678 ->
      (k_make_list_cons_x_6622 false true 0 x0_6678 x10_6678 x11_6678).
  f_x_7988 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 x0_6715 x1_6715 ->
      (k_make_list_cons_x_6622 true x0_6715 x1_6715 false true 0).
  f_x_7989 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622 xs_1212 x0_6759 x1_6759 ->
      (x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212
        (f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622)).
  f_x_7990 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 k_make_list_cons_x_6622 x0_6758 
  x1_6758 -> (k_make_list_cons_x_6622 true x0_6759 x1_6759 true x0_6758 x1_6758).
  f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853 x00_6884 x010_6884 x011_6884 x10_6884 x110_6884 x111_6884 ->
      (k_make_list_x_6853 x010_6884 x011_6884).
  f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 
  k_append_x_x_x_5718 x01_5967 x00_5786 x010_5786 x011_5786 x10_5786 x110_5786 x111_5786 ->
      (x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
        x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967
        (f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967
          x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718)).
  f_x_x_7968 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 
  x111_5786 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 k_append_x_x_x_5718 x00_5785 x010_5785 x011_5785 x10_5785 
  x110_5785 x111_5785 -> (k_append_x_x_x_5718 x010_5786 x011_5786 x110_5785 x111_5785).
  f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977 x0_5009 x1_5009 ->
      (k_append_x_xs'_4977 x0_5009 x1_5009).
  fail_8017 b k -> {fail} => (k ()).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when b_7708 ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (ii01_3320 = ii11_3320)).
  k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7708 when (
      not b_7708) ->
      (k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when b_7709 ->
      (x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_k_append_x_x_7947 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764)).
  k_append_x_x_4771 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 b_7709 when (
      not b_7709) ->
      (br_k_append_x_x_8005 (ii00_3320 <=> false) ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191
        k_append_x_x_4764 ys_1010 b_7709).
  main_1015 i_1016 n_1017 k_main_6954 -> (x_4561 i_1016 n_1017 (f_main_7994 i_1016 n_1017 k_main_6954)).
  make_list_1013 n_1014 k_make_list_6554 when (n_1014 < 0) -> (k_make_list_6554 (f_make_list_7984 n_1014)).
  make_list_1013 n_1014 k_make_list_6554 when (not (n_1014 < 0)) ->
      (x_4511 n_1014 (f_make_list_7991 n_1014 k_make_list_6554)).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (x010_7448 <=> false)) -> (k_main_n_7424 x011_7448).
  n_1432 i_1016 n_1017 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 x111_7460 
  k_main_n_7424 when (not (not (x010_7448 <=> false))) -> _|_.
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (x110_7448 <=> false)) -> (k_main_n_7432 x111_7448).
  n_1433 i_1016 n_1017 n_7447 x00_7448 x00_7460 x010_7448 x011_7448 x10_7448 x10_7460 x110_7448 x110_7460 x111_7448 
  x111_7460 k_main_n_7432 when (not (not (x110_7448 <=> false))) -> _|_.
  x_1542 xs_1009 ys_1010 k_append_x_4744 -> (x_3635 xs_1009 (f_x_7946 k_append_x_4744 xs_1009 ys_1010)).
  x_1551 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6015 -> _|_.
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      ii00_2532 <=> false) ->
      (x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7975 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_6023)).
  x_1554 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532 ii11_2532 k_append_x_x_6023 when (
      not (ii00_2532 <=> false)) ->
      (br_x_8011 (ii10_2532 <=> false) x0_6183 x0_6191 x1_6183 x1_6191 x_6175 ys_1010 ii00_2532 ii01_2532 ii10_2532
        ii11_2532 k_append_x_x_6023).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      i_1145 = 0) -> (k_append_x_cons_x_5040 true x_1146).
  x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 i_1145 k_append_x_cons_x_5040 when (
      not (i_1145 = 0)) ->
      (x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7956 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 k_append_x_cons_x_5040)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (ii00_3219 <=> false) ->
      (x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7958 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_5087)).
  x_1576 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219 ii01_3219 ii10_3219 ii11_3219 
  k_append_x_cons_x_5087 when (not (ii00_3219 <=> false)) ->
      (br_x_8007 (ii10_3219 <=> false) x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii00_3219
        ii01_3219 ii10_3219 ii11_3219 k_append_x_cons_x_5087).
  x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3083 
  k_append_x_x_5581 ->
      (x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7964 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5581)).
  x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3076 
  k_append_x_x_5617 ->
      (x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7965 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5617)).
  x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 i_3067 
  k_append_x_x_5681 ->
      (x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7966 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191
          k_append_x_x_5681)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (ii00_3043 <=> false) ->
      (x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7970 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_5827)).
  x_1589 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii00_3043 
  ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827 when (not (ii00_3043 <=> false)) ->
      (br_x_8009 (ii10_3043 <=> false) x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183
        x1_6191 x01_5967 ii00_3043 ii01_3043 ii10_3043 ii11_3043 k_append_x_x_5827).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (ii00_3320 <=> false)) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010
        (not (ii10_3320 <=> false))).
  x_1592 x0_6191 x1_6191 ys_1010 ii00_3320 ii01_3320 ii10_3320 ii11_3320 k_append_x_x_4764 when (
      not (not (ii00_3320 <=> false))) ->
      (k_append_x_x_4767 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_4764 ys_1010 false).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      ysi00_2311 <=> false) ->
      (x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7980 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_6208)).
  x_1595 xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208 when (
      not (ysi00_2311 <=> false)) ->
      (br_x_8013 (ysi10_2311 <=> false) xs_1009 ysi00_2311 ysi01_2311 ysi10_2311 ysi11_2311 k_append_x_6208).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (i_1210 = 0) ->
      (k_make_list_cons_x_6575 true x_1211).
  x_1598 n_1014 x_1211 xs_1212 i_1210 k_make_list_cons_x_6575 when (not (i_1210 = 0)) ->
      (x_4398 i_1210 n_1014 x_1211 xs_1212 (f_x_7985 i_1210 n_1014 x_1211 k_make_list_cons_x_6575)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      ii00_2110 <=> false) ->
      (x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7987 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_6622)).
  x_1612 n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110 k_make_list_cons_x_6622 when (
      not (ii00_2110 <=> false)) ->
      (br_x_8015 (ii10_2110 <=> false) n_1014 x_1211 xs_1212 ii00_2110 ii01_2110 ii10_2110 ii11_2110
        k_make_list_cons_x_6622).
  x_1619 n_1014 x_6924 x_6928 i_2021 k_make_list_x_6853 ->
      (x_4532 i_2021 n_1014 x_6924 x_6928 (f_x_7993 i_2021 n_1014 x_6924 k_make_list_x_6853)).
  x_3635 xs_1009 k_append_x_x_4754 -> (xs_1009 0 k_append_x_x_4754).
  x_3636 x0_6191 x1_6191 xs_1009 k_append_x_x_4965 -> (xs_1009 0 k_append_x_x_4965).
  x_3643 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6123 ->
      (x_6175 ii01_2532 k_append_x_x_x_6123).
  x_3653 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6160 x0_6183 x0_6191 x1_6160 x1_6183 x1_6191 ys_1010 
  k_append_x_x_x_6135 -> (ys_1010 ii11_2532 k_append_x_x_x_6135).
  x_3666 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 x_6175 k_append_x_x_x_6086 ->
      (x_6175 ii01_2532 k_append_x_x_x_6086).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      ii10_2532 <=> false) -> (k_append_x_x_x_6058 false true 0).
  x_3701 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_6058 when (
      not (ii10_2532 <=> false)) ->
      (x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010
        (f_x_7974 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 k_append_x_x_x_6058)).
  x_3707 ii00_2532 ii01_2532 ii10_2532 ii11_2532 x0_6183 x0_6191 x1_6183 x1_6191 ys_1010 k_append_x_x_x_x_6044 ->
      (ys_1010 ii11_2532 k_append_x_x_x_x_6044).
  x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009 k_append_x_xs'_x_4984 ->
      (xs_1009 (x_1087 + 1) k_append_x_xs'_x_4984).
  x_3744 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 k_append_x_x_5022 -> (xs_1009 0 k_append_x_x_5022).
  x_3747 i_1145 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 k_append_x_cons_x_x_5053 ->
      (xs_1147 (i_1145 - 1) k_append_x_cons_x_x_5053).
  x_3765 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5187 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5187).
  x_3775 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_5224 x0_6001 x0_6183 x0_6191 x1_5224 x1_6001 x1_6183 x1_6191 
  x_1146 xs_1147 k_append_x_cons_x_x_5199 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_5199).
  x_3788 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5150 ->
      (x_1562 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 ii01_3219 k_append_x_cons_x_x_5150).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (ii10_3219 <=> false) -> (k_append_x_cons_x_x_5122 false true 0).
  x_3823 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_5122 when (not (ii10_3219 <=> false)) ->
      (x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147
        (f_x_7957 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146
          k_append_x_cons_x_x_5122)).
  x_3829 ii00_3219 ii01_3219 ii10_3219 ii11_3219 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_1146 xs_1147 
  k_append_x_cons_x_x_x_5108 -> (xs_1147 ii11_3219 k_append_x_cons_x_x_x_5108).
  x_3858 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 xs_1009 k_append_x_x_5277 ->
      (append_1053 (xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009) k_append_x_x_5277).
  x_3916 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x_5991 ys_1010 k_append_x_x_5559 ->
      (x_5991 true ys_1010 false 0 k_append_x_x_5559).
  x_3936 i_3083 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5606 -> (x01_5967 true i_3083 false 0 k_append_x_x_x_5606).
  x_3955 i_3076 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5642 -> (x01_5967 false 0 true i_3076 k_append_x_x_x_5642).
  x_3977 i_3067 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 
  k_append_x_x_x_5706 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        i_3067 false 0 k_append_x_x_x_5706).
  x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 
  k_append_x_x_x_x_5743 ->
      (cons_1150 x0_6001 x0_6183 x0_6191 x1_6001 x1_6183 x1_6191 x1_6001
        (x_1581 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967) true
        x_3593 false 0 k_append_x_x_x_x_5743).
  x_4006 x00_5786 x00_5967 x010_5786 x011_5786 x0_6001 x0_6183 x0_6191 x10_5786 x10_5967 x110_5786 x110_5967 x111_5786 
  x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594 x01_5967 k_append_x_x_x_x_5773 ->
      (x01_5967 false 0 true y_3594 k_append_x_x_x_x_5773).
  x_4037 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5928 ->
      (x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967
        ii01_3043 ii11_3043 k_append_x_x_x_5928).
  x_4061 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5890 ->
      (x_1585 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii01_3043
        k_append_x_x_x_5890).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (ii10_3043 <=> false) -> (
      k_append_x_x_x_5862 false true 0).
  x_4096 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_5862 when (not (ii10_3043 <=> false)) ->
      (x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
        x1_6001 x1_6183 x1_6191 x01_5967
        (f_x_7969 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967
          x1_6001 x1_6183 x1_6191 k_append_x_x_x_5862)).
  x_4102 ii00_3043 ii01_3043 ii10_3043 ii11_3043 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 
  x1_6183 x1_6191 x01_5967 k_append_x_x_x_x_5848 ->
      (x_1582 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 ii11_3043
        k_append_x_x_x_x_5848).
  x_4283 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6436 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6436).
  x_4293 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6466 -> (xs_1009 ysi11_2311 k_append_x_x_6466).
  x_4306 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 ysi01_2311 k_append_x_x_6354 ->
      (x_1542 xs_1009 ysi01_2311 k_append_x_x_6354).
  x_4332 ysi00_2311 ysi10_2311 ysi11_2311 ii00_2328 ii01_2328 ii10_2328 ii11_2328 k_append_x_x_6218 ->
      (k_append_x_x_6218 true true 0 true true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      ysi10_2311 <=> false) -> (k_append_x_x_6295 false true 0).
  x_4361 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_6295 when (
      not (ysi10_2311 <=> false)) ->
      (x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 (f_x_7979 ysi00_2311 ysi10_2311 ysi11_2311 k_append_x_x_6295)).
  x_4367 ysi00_2311 ysi10_2311 ysi11_2311 xs_1009 k_append_x_x_x_6281 -> (xs_1009 ysi11_2311 k_append_x_x_x_6281).
  x_4398 i_1210 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6588 -> (xs_1212 (i_1210 - 1) k_make_list_cons_x_x_6588).
  x_4416 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6722 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6722).
  x_4426 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x0_6759 x1_6759 x_1211 xs_1212 k_make_list_cons_x_x_6734 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_6734).
  x_4439 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6685 ->
      (x_1598 n_1014 x_1211 xs_1212 ii01_2110 k_make_list_cons_x_x_6685).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      ii10_2110 <=> false) -> (k_make_list_cons_x_x_6657 false true 0).
  x_4474 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_6657 when (
      not (ii10_2110 <=> false)) ->
      (x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212
        (f_x_7986 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 k_make_list_cons_x_x_6657)).
  x_4480 ii00_2110 ii01_2110 ii10_2110 ii11_2110 n_1014 x_1211 xs_1212 k_make_list_cons_x_x_x_6643 ->
      (xs_1212 ii11_2110 k_make_list_cons_x_x_x_6643).
  x_4511 n_1014 k_make_list_x_6794 -> (make_list_1013 (n_1014 - 1) k_make_list_x_6794).
  x_4513 n_1014 k_make_list_x_6815 -> (rand_int k_make_list_x_6815).
  x_4532 i_2021 n_1014 x_6924 x_6928 k_make_list_x_x_6878 ->
      (cons_1215 n_1014 x_6924 x_6928 true i_2021 false 0 k_make_list_x_x_6878).
  x_4561 i_1016 n_1017 k_main_x_6967 -> (make_list_1013 n_1017 k_main_x_6967).
  x_4567 i_1016 n_1017 k_main_x_7022 -> (append_1053 (f_1409 i_1016 n_1017) k_main_x_7022).
  x_4625 i_1016 n_1017 x_7484 x_7492 k_main_x_7295 -> (x_7484 true x_7492 false 0 k_main_x_7295).
  x_4680 i_1016 n_1017 x00_7460 x10_7460 x110_7460 x111_7460 x01_7460 k_main_x_7413 ->
      (x01_7460 true i_1016 true i_1016 k_main_x_7413).
  x_4701 k_x_7503 -> (rand_int k_x_7503).
  x_4703 x_7548 k_x_7515 -> (rand_int k_x_7515).
  x_4705 x_7547 x_7548 k_x_7536 -> (main_1015 x_7548 x_7547 k_x_7536).
  x_7717 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7718 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7718).
  x_7723 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x0_7721 x1_6191 x1_7721 ys_1010 k_append_x_x_x_7724 ->
      (ys_1010 ii11_3320 k_append_x_x_x_7724).
  x_7738 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7739 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7739).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      ii10_3320 <=> false) -> (k_append_x_x_x_7757 false true 0).
  x_7756 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7757 when (
      not (ii10_3320 <=> false)) ->
      (x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010
        (f_x_7948 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 k_append_x_x_x_7757)).
  x_7761 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_x_7762 ->
      (ys_1010 ii11_3320 k_append_x_x_x_x_7762).
  x_7786 b_7709 ii00_3320 ii01_3320 ii10_3320 ii11_3320 x0_6191 x1_6191 ys_1010 k_append_x_x_x_7787 ->
      (ys_1010 ii01_3320 k_append_x_x_x_7787).
  x_x_3621 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x01_5967 x_3593 
  y_3594 k_append_x_x_x_5718 ->
      (x_3991 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
        x01_5967
        (f_x_x_7967 x00_5967 x0_6001 x0_6183 x0_6191 x10_5967 x110_5967 x111_5967 x1_6001 x1_6183 x1_6191 x_3593 y_3594
          k_append_x_x_x_5718 x01_5967)).
  xs'_1012 x0_6183 x0_6191 x1_6183 x1_6191 xs_1009 x_1087 k_append_x_xs'_4977 ->
      (x_3738 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 xs_1009
        (f_xs'_7954 x0_6183 x0_6191 x1_6183 x1_6191 x_1087 k_append_x_xs'_4977)).
Types:
  main_7945 : X
  append_1053 : ((int -> (x_4:bool[x_4] -> int[(not x_4)] -> X) -> X) ->
                 ((x_10:bool[x_10] ->
                   (int -> (x_14:bool[x_14] -> int -> X) -> X) ->
                   x_18:bool[x_18] ->
                   int[(not x_18)] ->
                   (x_21:bool[x_21] ->
                    (x_23:bool[x_23] ->
                     x_24:int ->
                     x_25:bool[x_25] ->
                     x_26:int[(not x_25); x_26 >= x_24 && x_26 <= x_24] ->
                     (x_28:bool[x_28] ->
                      x_29:bool[x_29] -> x_30:int -> x_31:bool[x_31] -> x_32:bool[x_32] -> x_33:int[x_30 = x_33] -> X)
                     -> X)
                    -> x_36:bool[x_36] -> x_37:bool[x_37] -> int -> X)
                   -> X)
                 -> X) -> X)
  cons_1150 : (x_1:bool[x_1] ->
               x_2:bool[x_2] ->
               x_3:bool[x_3] ->
               int ->
               int ->
               int ->
               int ->
               (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
               x_15:bool[x_15] ->
               int ->
               x_17:bool[x_17] ->
               int[(not x_17)] ->
               (x_20:bool[x_20] ->
                x_21:bool[x_21] -> x_22:int -> x_23:bool[x_23] -> x_24:bool[x_24] -> x_25:int[x_22 = x_25] -> X)
               -> X)
  fail_8017 : (x_1:bool[x_1] -> (unit -> X) -> X)
  make_list_1013 : (int -> ((int -> (x_6:bool[x_6] -> int -> X) -> X) -> X) -> X)
  x_1542 : ((int -> (x_4:bool[x_4] -> int[(not x_4)] -> X) -> X) ->
            (int -> (x_11:bool[x_11] -> int -> X) -> X) ->
            ((x_17:bool[x_17] ->
              x_18:int ->
              x_19:bool[x_19] ->
              x_20:int[(not x_19); x_20 >= x_18 && x_20 <= x_18] ->
              (x_22:bool[x_22] ->
               x_23:bool[x_23] -> x_24:int -> x_25:bool[x_25] -> x_26:bool[x_26] -> x_27:int[x_24 = x_27] -> X)
              -> X)
            -> X) -> X)
  x_1562 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            int ->
            int ->
            int -> int -> (int -> (x_11:bool[x_11] -> int -> X) -> X) -> int -> (x_17:bool[x_17] -> int -> X) -> X)
  x_1581 : (x_1:bool[x_1] ->
            x_2:bool[x_2] ->
            x_3:bool[x_3] ->
            x_4:bool[x_4] ->
            x_5:bool[x_5] ->
            x_6:bool[x_6] ->
            int ->
            int ->
            int ->
            int ->
            (x_12:bool[x_12] ->
             int ->
             x_14:bool[x_14] ->
             int[(not x_14)] ->
             (x_17:bool[x_17] ->
              x_18:bool[x_18] -> x_19:int -> x_20:bool[x_20] -> x_21:bool[x_21] -> x_22:int[x_19 = x_22] -> X)
             -> X)
            -> int -> (x_27:bool[x_27] -> int -> X) -> X)
  x_1598 : (int -> int -> (int -> (x_6:bool[x_6] -> int -> X) -> X) -> int -> (x_12:bool[x_12] -> int -> X) -> X)

(5-1) Abstracting ... DONE!

(5-2) Checking HORS ... DONE!

Safe!

cycles: 5
total: 170.985 sec
  abst: 3.147 sec
  mc: 1.459 sec
  refine: 163.955 sec
    exparam: 18.191 sec
