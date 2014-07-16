MoCHi: Model Checker for Higher-Order Programs
  Build: 02f7962 (2014-07-16 00:57:40 +0900)
  FPAT version: 3c21822
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil2_nth.ml -disable-rc -color -tupling -list-option -abs-remove-false -fpat 
           -hccs 1 -bool-init-empty -debug-module Tupling

parsed:
 let rec make_list_1008 n_1009 = if n_1009 < 0 then
                                   []
                                 else
                                   rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 x_1023 =
   match x_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 -> x_1013::append_1010 (xs'_1014, ys_1012))
 in
 let main_1015 i_1016 n_1017 =
   let xs_1018 = make_list_1008 n_1017 in
   let ys_1019 = append_1010 (xs_1018, []) in
   if List.nth ys_1019 i_1016 = List.nth xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 ()

set_target:
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 (x_1023:(!!! list * !!! list)) =
   match x_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 -> x_1013::append_1010 (xs'_1014, ys_1012))
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1008 n_1017 in
   let ys_1019 = append_1010 (xs_1018, []) in
   if List.nth ys_1019 i_1016 = List.nth xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1055 = let arg1_1051 = rand_int () in
                 let arg2_1053 = rand_int () in
                 main_1015 arg1_1051 arg2_1053 in
 ()

make_ext_funs:
 let List.nth_1056 (x_1057:int list) (x_1058:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1010 (x_1023:(!!! list * !!! list)) =
   match x_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 -> x_1013::append_1010 (xs'_1014, ys_1012))
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1008 n_1017 in
   let ys_1019 = append_1010 (xs_1018, []) in
   if List.nth_1056 ys_1019 i_1016 = List.nth_1056 xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1055 = let arg1_1051 = rand_int () in
                 let arg2_1053 = rand_int () in
                 main_1015 arg1_1051 arg2_1053 in
 ()

copy_poly:
 let List.nth_1056 (x_1057:int list) (x_1058:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) = if n_1009 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1008 (n_1009 - 1) in
 let rec append_1059 (x_1023:(int list * int list)) =
   match x_1023 with
   | (xs_1011, ys_1012) ->
       (match xs_1011 with
        | [] -> ys_1012
        | x_1013::xs'_1014 -> x_1013::append_1059 (xs'_1014, ys_1012))
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1008 n_1017 in
   let ys_1019 = append_1059 (xs_1018, []) in
   if List.nth_1056 ys_1019 i_1016 = List.nth_1056 xs_1018 i_1016 then
     ()
   else
     {fail} ()
 in
 let main_1055 = let arg1_1051 = rand_int () in
                 let arg2_1053 = rand_int () in
                 main_1015 arg1_1051 arg2_1053 in
 ()

encode_list:
 let List.nth_1056 (x_1057:(int -> (bool * int))) (x_1058:int) = rand_int () in
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1123:int) -> (false, 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool * int))) (i_1112:int) =
       if i_1112 = 0 then
         (true, x_1113)
       else
         xs_1114 (i_1112 - 1)
     in
     cons_1117 (rand_int ()) (make_list_1008 (n_1009 - 1))
 in
 let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1011 = fst x_1023 in
   let ys_1012 = snd x_1023 in
   if fst (xs_1011 0) = false then
     ys_1012
   else
     if fst (xs_1011 0) <> false then
       let xs'_1014 (x_1150:int) = xs_1011 (x_1150 + 1) in
       let x_1013 = snd (xs_1011 0) in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) (i_1220:int) =
         if i_1220 = 0 then
           (true, x_1221)
         else
           xs_1222 (i_1220 - 1)
       in
       cons_1225 x_1013 (append_1059 (xs'_1014, ys_1012))
     else
       _|_
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let xs_1018 = make_list_1008 n_1017 in
   let ys_1019 = append_1059 (xs_1018, (fun (x_1329:int) -> (false, 0))) in
   if (let x_1379 = ys_1019 i_1016 in
       if fst x_1379 <> false then
         snd x_1379
       else
         _|_)
      = (let x_1369 = xs_1018 i_1016 in
         if fst x_1369 <> false then
           snd x_1369
         else
           _|_) then
     ()
   else
     {fail} ()
 in
 let main_1055 = let arg1_1051 = rand_int () in
                 let arg2_1053 = rand_int () in
                 main_1015 arg1_1051 arg2_1053 in
 ()

ret_fun:
 let List.nth_1056 (x_1057:(int -> (bool * int))) =
   let x_1580 = fun (x_1058:int) -> (let f_1383 = rand_int in
                                     let n_1385 = f_1383 () in
                                     n_1385) in
   (x_1580, x_1057)
 in
 let rec make_list_1008 (n_1009:int) =
   let b_1386 = n_1009 < 0 in
   if b_1386 then
     fun (x_1123:int) -> (false, 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool * int))) =
       let x_1592 =
         fun (i_1112:int) ->
           (let b_1393 = i_1112 = 0 in
            if b_1393 then
              (true, x_1113)
            else
              let n_1403 = i_1112 - 1 in
              let p_1404 = xs_1114 n_1403 in
              let xs_1525 (n_1526:int) = if n_1526 = n_1403 then
                                           p_1404
                                         else
                                           xs_1114 n_1526 in
              p_1404)
       in
       (x_1592, xs_1114)
     in
     let n_1411 = n_1009 - 1 in
     let f_1412 = make_list_1008 n_1411 in
     let f_1405 = rand_int in
     let n_1407 = f_1405 () in
     let f_1416 = cons_1117 n_1407 in
     let p_1536 = f_1416 f_1412 in
     let f_1417 = fst p_1536 in
     let f_1537 = snd p_1536 in
     f_1417
 in
 let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1011 = fst x_1023 in
   let ys_1012 = snd x_1023 in
   let p_1424 = xs_1011 0 in
   let b_1426 = fst p_1424 in
   let b_1421 = b_1426 = false in
   if b_1421 then
     let x_1654 = fst x_1023 in
     let x_1655 = snd x_1023 in
     (ys_1012, x_1654, x_1655)
   else
     let p_1431 = xs_1011 0 in
     let b_1433 = fst p_1431 in
     let b_1435 = b_1433 = false in
     let b_1428 = not b_1435 in
     if b_1428 then
       let xs'_1014 (x_1150:int) = let n_1439 = x_1150 + 1 in
                                   let p_1440 = xs_1011 n_1439 in
                                   p_1440 in
       let p_1443 = xs_1011 0 in
       let x_1013 = snd p_1443 in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
         let x_1625 (i_1220:int) =
           let b_1445 = i_1220 = 0 in
           if b_1445 then
             (true, x_1221)
           else
             let n_1455 = i_1220 - 1 in
             let p_1456 = xs_1222 n_1455 in
             let xs_1543 (n_1544:int) = if n_1544 = n_1455 then
                                          p_1456
                                        else
                                          xs_1222 n_1544 in
             p_1456
         in
         (x_1625, xs_1222)
       in
       let p_1462 = (xs'_1014, ys_1012) in
       let p_1559 = append_1059 p_1462 in
       let f_1463 = #0 p_1559 in
       let p_1560 = (#1 p_1559, #2 p_1559) in
       let f_1467 = cons_1225 x_1013 in
       let p_1555 = f_1467 f_1463 in
       let f_1468 = fst p_1555 in
       let f_1556 = snd p_1555 in
       let x_1646 = fst x_1023 in
       let x_1647 = snd x_1023 in
       (f_1468, x_1646, x_1647)
     else
       let x_1610 = _|_ in
       let x_1613 = fst x_1023 in
       let x_1614 = snd x_1023 in
       (x_1610, x_1613, x_1614)
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let f_1473 = make_list_1008 n_1017 in
   let f_1479 (x_1329:int) = (false, 0) in
   let p_1483 = (f_1473, f_1479) in
   let p_1576 = append_1059 p_1483 in
   let f_1484 = #0 p_1576 in
   let p_1577 = (#1 p_1576, #2 p_1576) in
   let p_1488 = f_1484 i_1016 in
   let b_1491 = fst p_1488 in
   let b_1493 = b_1491 = false in
   let b_1489 = not b_1493 in
   let n_1504 = if b_1489 then
                  snd p_1488
                else
                  _|_ in
   let p_1497 = f_1473 i_1016 in
   let b_1500 = fst p_1497 in
   let b_1502 = b_1500 = false in
   let b_1498 = not b_1502 in
   let n_1505 = if b_1498 then
                  snd p_1497
                else
                  _|_ in
   let b_1485 = n_1504 = n_1505 in
   if b_1485 then
     ()
   else
     let f_1506 = {fail} in
     let u_1508 = f_1506 () in
     u_1508
 in
 let f_1509 = rand_int in
 let n_1511 = f_1509 () in
 let f_1512 = rand_int in
 let n_1514 = f_1512 () in
 let f_1518 = main_1015 n_1511 in
 let u_1519 = f_1518 n_1514 in
 ()

ref_trans:
 let List.nth_1056 (x_1057:(int -> (bool * int))) =
   let x_1580 (x_1058:int) = rand_int () in
   let x_1716 (xi_3620:((bool * int) * (bool * int))) =
     ((if fst (fst xi_3620) = false then
         (false, 0)
       else
         (true, x_1580 (snd (fst xi_3620)))),
      (if fst (snd xi_3620) = false then
         (false, (true, 0))
       else
         (true, x_1057 (snd (snd xi_3620)))))
   in
   x_1716
 in
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1123:int) -> (false, 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool * int))) =
       let x_1592 (i_1112:int) =
         if i_1112 = 0 then
           (true, x_1113)
         else
           let x_1723 = xs_1114 (i_1112 - 1) in
           let xs_1525 (n_1526:int) = if n_1526 = i_1112 - 1 then
                                        x_1723
                                      else
                                        xs_1114 n_1526 in
           x_1723
       in
       let x_1732 (ii_3523:((bool * int) * (bool * int))) =
         ((if fst (fst ii_3523) = false then
             (false, (true, 0))
           else
             (true, x_1592 (snd (fst ii_3523)))),
          (if fst (snd ii_3523) = false then
             (false, (true, 0))
           else
             (true, xs_1114 (snd (snd ii_3523)))))
       in
       x_1732
     in
     let x_1735 = make_list_1008 (n_1009 - 1) in
     let x_1736 = rand_int () in
     let x_1737 = cons_1117 x_1736 in
     let x_1738 = x_1737 x_1735 in
     let x_1739 (i_3447:int) = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
     let x_1740 (i_3440:int) = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
     x_1739
 in
 let rec append_1059 (x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1746 (i_3310:int) = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
   let x_1747 (i_3303:int) = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
   let x_1748 = let x_3302 = x_1023 ((true, 0), (false, 0)) in
                snd (fst x_3302) in
   let x_1750 = snd x_1748 in
   if fst x_1748 = false then
     let x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) =
       ((if fst (#0 iii_3257) = false then
           (false, (true, 0))
         else
           (true, x_1747 (snd (#0 iii_3257)))),
        (if fst (#1 iii_3257) = false then
           (false, (true, 0))
         else
           (true, x_1746 (snd (#1 iii_3257)))),
        (if fst (#2 iii_3257) = false then
           (false, (true, 0))
         else
           (true, x_1747 (snd (#2 iii_3257)))))
     in
     x_1811
   else
     let x_1753 = let x_3180 = x_1023 ((true, 0), (false, 0)) in
                  snd (fst x_3180) in
     let x_1755 = snd x_1753 in
     if fst x_1753 <> false then
       let xs'_1014 (x_1150:int) =
         let x_1764 = let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                      snd (fst x_3159) in
         let x_1765 = fst x_1764 in
         let x_1766 = snd x_1764 in
         x_1764
       in
       let x_1767 = let x_3138 = x_1023 ((true, 0), (false, 0)) in
                    snd (fst x_3138) in
       let x_1768 = fst x_1767 in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
         let x_1625 (i_1220:int) =
           if i_1220 = 0 then
             (true, x_1221)
           else
             let x_1774 = xs_1222 (i_1220 - 1) in
             let x_1775 = fst x_1774 in
             let x_1776 = snd x_1774 in
             let xs_1543 (n_1544:int) = if n_1544 = i_1220 - 1 then
                                          x_1774
                                        else
                                          xs_1222 n_1544 in
             x_1774
         in
         let x_1785 (ii_3086:((bool * int) * (bool * int))) =
           ((if fst (fst ii_3086) = false then
               (false, (true, 0))
             else
               (true, x_1625 (snd (fst ii_3086)))),
            (if fst (snd ii_3086) = false then
               (false, (true, 0))
             else
               (true, xs_1222 (snd (snd ii_3086)))))
         in
         x_1785
       in
       let x_1788 (ii_3004:((bool * int) * (bool * int))) =
         ((if fst (fst ii_3004) = false then
             (false, (true, 0))
           else
             (true, xs'_1014 (snd (fst ii_3004)))),
          (if fst (snd ii_3004) = false then
             (false, (true, 0))
           else
             (true, x_1747 (snd (snd ii_3004)))))
       in
       let x_1789 (i_2984:int) = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
       let x_1790 (i_2977:int) = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
       let x_1791 = append_1059 x_1788 in
       let x_1792 (i_2966:int) = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
       let x_1793 (i_2956:int) = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
       let x_1794 (i_2946:int) = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
       let x_1797 (ii_2929:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2929) = false then
             (false, (true, 0))
           else
             (true, x_1793 (snd (fst ii_2929)))),
          (if fst (snd ii_2929) = false then
             (false, (true, 0))
           else
             (true, x_1794 (snd (snd ii_2929)))))
       in
       let x_1798 (i_2909:int) = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
       let x_1799 (i_2902:int) = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
       let x_1800 = cons_1225 (snd x_1767) in
       let x_1801 = x_1800 x_1792 in
       let x_1802 (i_2893:int) = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
       let x_1803 (i_2886:int) = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
       let x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_2861) = false then
             (false, (true, 0))
           else
             (true, x_1802 (snd (#0 iii_2861)))),
          (if fst (#1 iii_2861) = false then
             (false, (true, 0))
           else
             (true, x_1746 (snd (#1 iii_2861)))),
          (if fst (#2 iii_2861) = false then
             (false, (true, 0))
           else
             (true, x_1747 (snd (#2 iii_2861)))))
       in
       x_1807
     else
       let x_1610 = _|_ in
       let x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_2432) = false then
             (false, (true, 0))
           else
             (true, x_1610 (snd (#0 iii_2432)))),
          (if fst (#1 iii_2432) = false then
             (false, (true, 0))
           else
             (true, x_1746 (snd (#1 iii_2432)))),
          (if fst (#2 iii_2432) = false then
             (false, (true, 0))
           else
             (true, x_1747 (snd (#2 iii_2432)))))
       in
       x_1761
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_1812 = make_list_1008 n_1017 in
   let f_1479 (x_1329:int) = (false, 0) in
   let x_1820 (ix_2198:((bool * int) * (bool * int))) =
     ((if fst (fst ix_2198) = false then
         (false, (true, 0))
       else
         (true, x_1812 (snd (fst ix_2198)))),
      (if fst (snd ix_2198) = false then
         (false, (true, 0))
       else
         (true, f_1479 (snd (snd ix_2198)))))
   in
   let x_1821 (i_2178:int) = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
   let x_1822 (x_2171:int) = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
   let x_1823 = append_1059 x_1820 in
   let x_1824 (i_2160:int) = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
   let x_1825 (i_2150:int) = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
   let x_1826 (i_2140:int) = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
   let x_1829 (ii_2123:((bool * int) * (bool * int))) =
     ((if fst (fst ii_2123) = false then
         (false, (true, 0))
       else
         (true, x_1825 (snd (fst ii_2123)))),
      (if fst (snd ii_2123) = false then
         (false, (true, 0))
       else
         (true, x_1826 (snd (snd ii_2123)))))
   in
   let x_1830 (i_2103:int) = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
   let x_1831 (i_2096:int) = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
   let x_1832 = let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
                snd (#0 x_2095) in
   let n_1504 = if fst x_1832 <> false then
                  snd x_1832
                else
                  _|_ in
   let x_1837 = x_1812 i_1016 in
   let n_1505 = if fst x_1837 <> false then
                  snd x_1837
                else
                  _|_ in
   if n_1504 = n_1505 then
     ()
   else
     {fail} ()
 in
 let x_1844 = rand_int () in
 let x_1845 = rand_int () in
 let x_1846 = main_1015 x_1844 in
 let x_1847 = x_1846 x_1845 in
 ()

inline_wrapped:
let List.nth_1056 x_1057 =
  let x_1580 x_1058 = rand_int () in
  let x_1716 xi_3620 =
    if fst (fst xi_3620) = false then
      if fst (snd xi_3620) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3620))))
    else
      if fst (snd xi_3620) = false then
        ((true, x_1580 (snd (fst xi_3620))), (false, (true, 0)))
      else
        ((true, x_1580 (snd (fst xi_3620))), (true, x_1057 (snd (snd xi_3620))))
  in
  x_1716
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_1748 = let x_3302 = x_1023 ((true, 0), (false, 0)) in
               snd (fst x_3302) in
  let x_1750 = snd x_1748 in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_1753 = let x_3180 = x_1023 ((true, 0), (false, 0)) in
                 snd (fst x_3180) in
    let x_1755 = snd x_1753 in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_1764 = let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                     snd (fst x_3159) in
        let x_1765 = fst x_1764 in
        let x_1766 = snd x_1764 in
        x_1764
      in
      let x_1767 = let x_3138 = x_1023 ((true, 0), (false, 0)) in
                   snd (fst x_3138) in
      let x_1768 = fst x_1767 in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let x_1775 = fst x_1774 in
            let x_1776 = snd x_1774 in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_1832 = let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
               snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let x_1837 = x_1812 i_1016 in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

flatten_let:
let List.nth_1056 x_1057 =
  let x_1580 x_1058 = rand_int () in
  let x_1716 xi_3620 =
    if fst (fst xi_3620) = false then
      if fst (snd xi_3620) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3620))))
    else
      if fst (snd xi_3620) = false then
        ((true, x_1580 (snd (fst xi_3620))), (false, (true, 0)))
      else
        ((true, x_1580 (snd (fst xi_3620))), (true, x_1057 (snd (snd xi_3620))))
  in
  x_1716
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  let x_1750 = snd x_1748 in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_3180 = x_1023 ((true, 0), (false, 0)) in
    let x_1753 = snd (fst x_3180) in
    let x_1755 = snd x_1753 in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        let x_1765 = fst x_1764 in
        let x_1766 = snd x_1764 in
        x_1764
      in
      let x_3138 = x_1023 ((true, 0), (false, 0)) in
      let x_1767 = snd (fst x_3138) in
      let x_1768 = fst x_1767 in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let x_1775 = fst x_1774 in
            let x_1776 = snd x_1774 in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let x_1837 = x_1812 i_1016 in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

NORMALIZE: n_1504
[x_1837]
NORMALIZE: x_1832
[x_1837]
normalize let:
let List.nth_1056 x_1057 =
  let x_1580 x_1058 = rand_int () in
  let x_1716 xi_3620 =
    if fst (fst xi_3620) = false then
      if fst (snd xi_3620) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3620))))
    else
      if fst (snd xi_3620) = false then
        ((true, x_1580 (snd (fst xi_3620))), (false, (true, 0)))
      else
        ((true, x_1580 (snd (fst xi_3620))), (true, x_1057 (snd (snd xi_3620))))
  in
  x_1716
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  let x_1750 = snd x_1748 in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_3180 = x_1023 ((true, 0), (false, 0)) in
    let x_1753 = snd (fst x_3180) in
    let x_1755 = snd x_1753 in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        let x_1765 = fst x_1764 in
        let x_1766 = snd x_1764 in
        x_1764
      in
      let x_3138 = x_1023 ((true, 0), (false, 0)) in
      let x_1767 = snd (fst x_3138) in
      let x_1768 = fst x_1767 in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let x_1775 = fst x_1774 in
            let x_1776 = snd x_1774 in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1837 = x_1812 i_1016 in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_1844; is_subsumed: 
rand_int (), x_1846 x_1845; is_subsumed: make_list_1008 n_1017, append_1059 x_1820; is_subsumed: 
make_list_1008 n_1017, x_1823 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_1823 ((true, i_1016), (false, 0), (false, 0)), x_1812 i_1016; is_subsumed: 
append_1059 x_1820, x_1812 i_1016; is_subsumed: x_1812 i_1016, snd (#0 x_2095); is_subsumed: 
append_1059 x_1820, snd (#0 x_2095); is_subsumed: make_list_1008 n_1017, 
snd (#0 x_2095); is_subsumed: x_1812 i_1016, if fst x_1832 <> false then
                                               snd x_1832
                                             else
                                               _|_; is_subsumed: x_1823 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_1832 <> false then
  snd x_1832
else
  _|_; is_subsumed: append_1059 x_1820, if fst x_1832 <> false then
                                          snd x_1832
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_1832 <> false then
  snd x_1832
else
  _|_; is_subsumed: if fst x_1832 <> false then
                      snd x_1832
                    else
                      _|_, if fst x_1837 <> false then
                             snd x_1837
                           else
                             _|_; is_subsumed: snd (#0 x_2095), if fst x_1837 <> false then
                                                                  snd x_1837
                                                                else
                                                                  _|_; is_subsumed: 
x_1823 ((true, i_1016), (false, 0), (false, 0)), if fst x_1837 <> false then
                                                   snd x_1837
                                                 else
                                                   _|_; is_subsumed: 
append_1059 x_1820, if fst x_1837 <> false then
                      snd x_1837
                    else
                      _|_; is_subsumed: make_list_1008 n_1017, if fst x_1837 <> false then
                                                                 snd x_1837
                                                               else
                                                                 _|_; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1748; is_subsumed: snd x_1748, 
x_1023 ((true, 0), (false, 0)); is_subsumed: snd (fst x_3302), x_1023 ((true, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, 0), (false, 0)); x_3302 |-> x_3180
is_subsumed: snd x_1748, snd (fst x_3180); is_subsumed: snd (fst x_3302), 
snd (fst x_3180); is_subsumed: x_1023 ((true, 0), (false, 0)), snd (fst x_3180); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1753; is_subsumed: snd x_1748, 
snd x_1753; is_subsumed: snd (fst x_3302), snd x_1753; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1753; is_subsumed: snd x_1753, _|_; is_subsumed: snd (fst x_3180), _|_; is_subsumed: 
x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: snd x_1748, _|_; is_subsumed: 
snd (fst x_3302), _|_; is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: 
snd x_1753, x_1023 ((true, 0), (false, 0)); is_subsumed: snd (fst x_3180), 
x_1023 ((true, 0), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); is_subsumed: snd x_1748, x_1023 ((true, 0), (false, 0)); is_subsumed: 
snd (fst x_3302), x_1023 ((true, 0), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_3180 |-> x_3138
x_3302 |-> x_3138
is_subsumed: snd x_1753, snd (fst x_3138); is_subsumed: snd (fst x_3180), 
snd (fst x_3138); is_subsumed: x_1023 ((true, 0), (false, 0)), snd (fst x_3138); is_subsumed: 
snd x_1748, snd (fst x_3138); is_subsumed: snd (fst x_3302), snd (fst x_3138); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd (fst x_3138); is_subsumed: x_1023 ((true, 0), (false, 0)), 
fst x_1767; is_subsumed: snd x_1753, fst x_1767; is_subsumed: snd (fst x_3180), 
fst x_1767; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1767; is_subsumed: 
snd x_1748, fst x_1767; is_subsumed: snd (fst x_3302), fst x_1767; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1767; is_subsumed: fst x_1767, 
append_1059 x_1788; is_subsumed: snd (fst x_3138), append_1059 x_1788; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: snd x_1753, 
append_1059 x_1788; is_subsumed: snd (fst x_3180), append_1059 x_1788; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: snd x_1748, 
append_1059 x_1788; is_subsumed: snd (fst x_3302), append_1059 x_1788; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: append_1059 x_1788, 
cons_1225 (snd x_1767); is_subsumed: fst x_1767, cons_1225 (snd x_1767); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd x_1767); is_subsumed: 
snd x_1753, cons_1225 (snd x_1767); is_subsumed: snd (fst x_3180), cons_1225 (snd x_1767); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd x_1767); is_subsumed: 
snd x_1748, cons_1225 (snd x_1767); is_subsumed: snd (fst x_3302), cons_1225 (snd x_1767); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd x_1767); is_subsumed: 
append_1059 x_1788, x_1800 x_1792; is_subsumed: fst x_1767, x_1800 x_1792; is_subsumed: 
snd (fst x_3138), x_1800 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1800 x_1792; is_subsumed: snd x_1753, x_1800 x_1792; is_subsumed: snd (fst x_3180), 
x_1800 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), x_1800 x_1792; is_subsumed: 
snd x_1748, x_1800 x_1792; is_subsumed: snd (fst x_3302), x_1800 x_1792; is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1800 x_1792; is_subsumed: fst x_1767, 
xs_1222 (i_1220 - 1); is_subsumed: snd (fst x_3138), xs_1222 (i_1220 - 1); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: snd x_1753, 
xs_1222 (i_1220 - 1); is_subsumed: snd (fst x_3180), xs_1222 (i_1220 - 1); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: snd x_1748, 
xs_1222 (i_1220 - 1); is_subsumed: snd (fst x_3302), xs_1222 (i_1220 - 1); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: fst x_1767, 
fst x_1774; is_subsumed: snd (fst x_3138), fst x_1774; is_subsumed: x_1023 ((true, 0), (false, 0)), 
fst x_1774; is_subsumed: snd x_1753, fst x_1774; is_subsumed: snd (fst x_3180), 
fst x_1774; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1774; is_subsumed: 
snd x_1748, fst x_1774; is_subsumed: snd (fst x_3302), fst x_1774; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1774; is_subsumed: fst x_1774, 
snd x_1774; is_subsumed: fst x_1767, snd x_1774; is_subsumed: snd (fst x_3138), 
snd x_1774; is_subsumed: x_1023 ((true, 0), (false, 0)), snd x_1774; is_subsumed: 
snd x_1753, snd x_1774; is_subsumed: snd (fst x_3180), snd x_1774; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1774; is_subsumed: snd x_1748, 
snd x_1774; is_subsumed: snd (fst x_3302), snd x_1774; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1774; is_subsumed: snd x_1753, x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
snd (fst x_3180), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
snd x_1748, x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: snd (fst x_3302), 
x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: snd x_1753, snd (fst x_3159); is_subsumed: 
snd (fst x_3180), snd (fst x_3159); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (fst x_3159); is_subsumed: snd x_1748, snd (fst x_3159); is_subsumed: 
snd (fst x_3302), snd (fst x_3159); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (fst x_3159); is_subsumed: x_1023 ((true, x_1150 + 1), (false, 0)), 
fst x_1764; is_subsumed: snd x_1753, fst x_1764; is_subsumed: snd (fst x_3180), 
fst x_1764; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1764; is_subsumed: 
snd x_1748, fst x_1764; is_subsumed: snd (fst x_3302), fst x_1764; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1764; is_subsumed: fst x_1764, 
snd x_1764; is_subsumed: x_1023 ((true, x_1150 + 1), (false, 0)), snd x_1764; is_subsumed: 
snd x_1753, snd x_1764; is_subsumed: snd (fst x_3180), snd x_1764; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1764; is_subsumed: snd x_1748, 
snd x_1764; is_subsumed: snd (fst x_3302), snd x_1764; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1764; is_subsumed: make_list_1008 (n_1009 - 1), rand_int (); is_subsumed: 
make_list_1008 (n_1009 - 1), cons_1117 x_1736; is_subsumed: rand_int (), 
x_1737 x_1735; x_3302; x_3180; x_3302
x_3180 |-> x_3302
x_3138 |-> x_3302
elim_same_app:
let List.nth_1056 x_1057 =
  let x_1580 x_1058 = rand_int () in
  let x_1716 xi_3620 =
    if fst (fst xi_3620) = false then
      if fst (snd xi_3620) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3620))))
    else
      if fst (snd xi_3620) = false then
        ((true, x_1580 (snd (fst xi_3620))), (false, (true, 0)))
      else
        ((true, x_1580 (snd (fst xi_3620))), (true, x_1057 (snd (snd xi_3620))))
  in
  x_1716
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  let x_1750 = snd x_1748 in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_1753 = snd (fst x_3302) in
    let x_1755 = snd x_1753 in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        let x_1765 = fst x_1764 in
        let x_1766 = snd x_1764 in
        x_1764
      in
      let x_1767 = snd (fst x_3302) in
      let x_1768 = fst x_1767 in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let x_1775 = fst x_1774 in
            let x_1776 = snd x_1774 in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1837 = x_1812 i_1016 in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

elim_unused_branch:
let List.nth_1056 x_1057 =
  let x_1580 x_1058 = rand_int () in
  let x_1716 xi_3620 =
    if fst (fst xi_3620) = false then
      if fst (snd xi_3620) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3620))))
    else
      if fst (snd xi_3620) = false then
        ((true, x_1580 (snd (fst xi_3620))), (false, (true, 0)))
      else
        ((true, x_1580 (snd (fst xi_3620))), (true, x_1057 (snd (snd xi_3620))))
  in
  x_1716
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  let x_1750 = snd x_1748 in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_1753 = snd (fst x_3302) in
    let x_1755 = snd x_1753 in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        let x_1765 = fst x_1764 in
        let x_1766 = snd x_1764 in
        x_1764
      in
      let x_1767 = snd (fst x_3302) in
      let x_1768 = fst x_1767 in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let x_1775 = fst x_1774 in
            let x_1776 = snd x_1774 in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1837 = x_1812 i_1016 in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

elim_unused_let:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257))))
    in
    x_1811
  else
    let x_1753 = snd (fst x_3302) in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        x_1764
      in
      let x_1767 = snd (fst x_3302) in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004))))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            ((true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929))))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), 
               (true, x_1747 (snd (#2 iii_2861))))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        ((true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123))))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1837 = x_1812 i_1016 in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

TUPLE: (true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523)))
x_1592
xs_1114
TUPLE: (true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432)))
x_1610
TUPLE: (true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0))
x_1610
TUPLE: (true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432)))
x_1610
TUPLE: (false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (true, x_1747 (snd (#2 iii_2432)))
x_1746
x_1747
compose: x_1746, snd
                 (fst
                  (x_1023
                    (let x1_3804 = let x1_3796 = true in
                                   let x2_3797 = x_3794 in
                                   (x1_3796, x2_3797) in
                     let x2_3805 = let x1_3800 = false in
                                   let x2_3801 = 0 in
                                   (x1_3800, x2_3801) in
                     (x1_3804, x2_3805)))); x_1747, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_3816 =
                                                          let x1_3808 = false in
                                                          let x2_3809 = 0 in
                                                          (x1_3808, x2_3809)
                                                        in
                                                        let x2_3817 =
                                                          let x1_3812 = true in
                                                          let x2_3813 = x_3795 in
                                                          (x1_3812, x2_3813)
                                                        in
                                                        (x1_3816, x2_3817)))); 
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3804 = let x1_3796 = true in
                         let x2_3797 = x_3794 in
                         (x1_3796, x2_3797) in
           let x2_3805 = let x1_3800 = false in
                         let x2_3801 = 0 in
                         (x1_3800, x2_3801) in
           (x1_3804, x2_3805))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3816 = let x1_3808 = false in
                         let x2_3809 = 0 in
                         (x1_3808, x2_3809) in
           let x2_3817 = let x1_3812 = true in
                         let x2_3813 = x_3795 in
                         (x1_3812, x2_3813) in
           (x1_3816, x2_3817))))
compose_let
x_1746:snd
       (fst
        (x_1023
          (let x1_3804 = let x1_3796 = true in
                         let x2_3797 = x_3794 in
                         (x1_3796, x2_3797) in
           let x2_3805 = let x1_3800 = false in
                         let x2_3801 = 0 in
                         (x1_3800, x2_3801) in
           (x1_3804, x2_3805))))

x_1747:snd
       (snd
        (x_1023
          (let x1_3816 = let x1_3808 = false in
                         let x2_3809 = 0 in
                         (x1_3808, x2_3809) in
           let x2_3817 = let x1_3812 = true in
                         let x2_3813 = x_3795 in
                         (x1_3812, x2_3813) in
           (x1_3816, x2_3817))))

ADD: (x_x_3820:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086)))
x_1625
xs_1222
TUPLE: (true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004)))
xs'_1014
x_1747
compose: xs'_1014, let x_3159 =
                     x_1023
                       (let x1_3851 = let x1_3843 = true in
                                      let x2_3844 = x_3841 + 1 in
                                      (x1_3843, x2_3844) in
                        let x2_3852 = let x1_3847 = false in
                                      let x2_3848 = 0 in
                                      (x1_3847, x2_3848) in
                        (x1_3851, x2_3852))
                   in
                   let x_1764 = snd (fst x_3159) in
                   x_1764; x_1747, snd
                                   (snd
                                    (x_1023
                                      (let x1_3863 = let x1_3855 = false in
                                                     let x2_3856 = 0 in
                                                     (x1_3855, x2_3856) in
                                       let x2_3864 = let x1_3859 = true in
                                                     let x2_3860 = x_3842 in
                                                     (x1_3859, x2_3860) in
                                       (x1_3863, x2_3864)))); 
PB: x:xs'_1014
CHECK: x_1764
CHECK: snd (fst x_3159)
CHECK: x_1023
         (let x1_3851 = let x1_3843 = true in
                        let x2_3844 = x_3841 + 1 in
                        (x1_3843, x2_3844) in
          let x2_3852 = let x1_3847 = false in
                        let x2_3848 = 0 in
                        (x1_3847, x2_3848) in
          (x1_3851, x2_3852))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3863 = let x1_3855 = false in
                         let x2_3856 = 0 in
                         (x1_3855, x2_3856) in
           let x2_3864 = let x1_3859 = true in
                         let x2_3860 = x_3842 in
                         (x1_3859, x2_3860) in
           (x1_3863, x2_3864))))
compose_let
xs'_1014:let x_3159 =
           x_1023
             (let x1_3851 = let x1_3843 = true in
                            let x2_3844 = x_3841 + 1 in
                            (x1_3843, x2_3844) in
              let x2_3852 = let x1_3847 = false in
                            let x2_3848 = 0 in
                            (x1_3847, x2_3848) in
              (x1_3851, x2_3852))
         in
         let x_1764 = snd (fst x_3159) in
         x_1764

x_1747:snd
       (snd
        (x_1023
          (let x1_3863 = let x1_3855 = false in
                         let x2_3856 = 0 in
                         (x1_3855, x2_3856) in
           let x2_3864 = let x1_3859 = true in
                         let x2_3860 = x_3842 in
                         (x1_3859, x2_3860) in
           (x1_3863, x2_3864))))

ADD: (xs'_x_3867:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929)))
x_1793
x_1794
compose: x_1793, snd
                 (#1
                  (x_1791
                    (let x1_3895 = let x1_3883 = false in
                                   let x2_3884 = 0 in
                                   (x1_3883, x2_3884) in
                     let x2_3896 = let x1_3887 = true in
                                   let x2_3888 = x_3881 in
                                   (x1_3887, x2_3888) in
                     let x3_3897 = let x1_3891 = false in
                                   let x2_3892 = 0 in
                                   (x1_3891, x2_3892) in
                     (x1_3895, x2_3896, x3_3897)))); x_1794, snd
                                                             (#2
                                                              (x_1791
                                                                (let x1_3913 =
                                                                   let x1_3901 = false in
                                                                   let x2_3902 = 0 in
                                                                   (x1_3901, x2_3902)
                                                                 in
                                                                 let x2_3914 =
                                                                   let x1_3905 = false in
                                                                   let x2_3906 = 0 in
                                                                   (x1_3905, x2_3906)
                                                                 in
                                                                 let x3_3915 =
                                                                   let x1_3909 = true in
                                                                   let x2_3910 = x_3882 in
                                                                   (x1_3909, x2_3910)
                                                                 in
                                                                 (x1_3913, x2_3914, x3_3915)))); 
PB: x:x_1793
CHECK: snd
       (#1
        (x_1791
          (let x1_3895 = let x1_3883 = false in
                         let x2_3884 = 0 in
                         (x1_3883, x2_3884) in
           let x2_3896 = let x1_3887 = true in
                         let x2_3888 = x_3881 in
                         (x1_3887, x2_3888) in
           let x3_3897 = let x1_3891 = false in
                         let x2_3892 = 0 in
                         (x1_3891, x2_3892) in
           (x1_3895, x2_3896, x3_3897))))
PB: x:x_1794
CHECK: snd
       (#2
        (x_1791
          (let x1_3913 = let x1_3901 = false in
                         let x2_3902 = 0 in
                         (x1_3901, x2_3902) in
           let x2_3914 = let x1_3905 = false in
                         let x2_3906 = 0 in
                         (x1_3905, x2_3906) in
           let x3_3915 = let x1_3909 = true in
                         let x2_3910 = x_3882 in
                         (x1_3909, x2_3910) in
           (x1_3913, x2_3914, x3_3915))))
compose_let
x_1793:snd
       (#1
        (x_1791
          (let x1_3895 = let x1_3883 = false in
                         let x2_3884 = 0 in
                         (x1_3883, x2_3884) in
           let x2_3896 = let x1_3887 = true in
                         let x2_3888 = x_3881 in
                         (x1_3887, x2_3888) in
           let x3_3897 = let x1_3891 = false in
                         let x2_3892 = 0 in
                         (x1_3891, x2_3892) in
           (x1_3895, x2_3896, x3_3897))))

x_1794:snd
       (#2
        (x_1791
          (let x1_3913 = let x1_3901 = false in
                         let x2_3902 = 0 in
                         (x1_3901, x2_3902) in
           let x2_3914 = let x1_3905 = false in
                         let x2_3906 = 0 in
                         (x1_3905, x2_3906) in
           let x3_3915 = let x1_3909 = true in
                         let x2_3910 = x_3882 in
                         (x1_3909, x2_3910) in
           (x1_3913, x2_3914, x3_3915))))

ADD: (x_x_3919:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861)))
x_1802
x_1746
x_1747
compose: x_1802, snd
                 (fst
                  (x_1801
                    (let x1_3944 = let x1_3936 = true in
                                   let x2_3937 = x_3933 in
                                   (x1_3936, x2_3937) in
                     let x2_3945 = let x1_3940 = false in
                                   let x2_3941 = 0 in
                                   (x1_3940, x2_3941) in
                     (x1_3944, x2_3945)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_3956 =
                                                          let x1_3948 = true in
                                                          let x2_3949 = x_3934 in
                                                          (x1_3948, x2_3949)
                                                        in
                                                        let x2_3957 =
                                                          let x1_3952 = false in
                                                          let x2_3953 = 0 in
                                                          (x1_3952, x2_3953)
                                                        in
                                                        (x1_3956, x2_3957)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_3968 = let x1_3960 = false in
                  let x2_3961 = 0 in
                  (x1_3960, x2_3961) in
    let x2_3969 = let x1_3964 = true in
                  let x2_3965 = x_3935 in
                  (x1_3964, x2_3965) in
    (x1_3968, x2_3969)))); 
PB: x:x_1802
CHECK: snd
       (fst
        (x_1801
          (let x1_3944 = let x1_3936 = true in
                         let x2_3937 = x_3933 in
                         (x1_3936, x2_3937) in
           let x2_3945 = let x1_3940 = false in
                         let x2_3941 = 0 in
                         (x1_3940, x2_3941) in
           (x1_3944, x2_3945))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3956 = let x1_3948 = true in
                         let x2_3949 = x_3934 in
                         (x1_3948, x2_3949) in
           let x2_3957 = let x1_3952 = false in
                         let x2_3953 = 0 in
                         (x1_3952, x2_3953) in
           (x1_3956, x2_3957))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3968 = let x1_3960 = false in
                         let x2_3961 = 0 in
                         (x1_3960, x2_3961) in
           let x2_3969 = let x1_3964 = true in
                         let x2_3965 = x_3935 in
                         (x1_3964, x2_3965) in
           (x1_3968, x2_3969))))
compose_let
x_1802:snd
       (fst
        (x_1801
          (let x1_3944 = let x1_3936 = true in
                         let x2_3937 = x_3933 in
                         (x1_3936, x2_3937) in
           let x2_3945 = let x1_3940 = false in
                         let x2_3941 = 0 in
                         (x1_3940, x2_3941) in
           (x1_3944, x2_3945))))

x_1746:snd
       (fst
        (x_1023
          (let x1_3956 = let x1_3948 = true in
                         let x2_3949 = x_3934 in
                         (x1_3948, x2_3949) in
           let x2_3957 = let x1_3952 = false in
                         let x2_3953 = 0 in
                         (x1_3952, x2_3953) in
           (x1_3956, x2_3957))))

x_1747:snd
       (snd
        (x_1023
          (let x1_3968 = let x1_3960 = false in
                         let x2_3961 = 0 in
                         (x1_3960, x2_3961) in
           let x2_3969 = let x1_3964 = true in
                         let x2_3965 = x_3935 in
                         (x1_3964, x2_3965) in
           (x1_3968, x2_3969))))

ADD: (x_x_x_3972:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0))
x_1802
x_1746
compose: x_1802, snd
                 (fst
                  (x_1801
                    (let x1_4002 = let x1_3994 = true in
                                   let x2_3995 = x_3992 in
                                   (x1_3994, x2_3995) in
                     let x2_4003 = let x1_3998 = false in
                                   let x2_3999 = 0 in
                                   (x1_3998, x2_3999) in
                     (x1_4002, x2_4003)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_4014 =
                                                          let x1_4006 = true in
                                                          let x2_4007 = x_3993 in
                                                          (x1_4006, x2_4007)
                                                        in
                                                        let x2_4015 =
                                                          let x1_4010 = false in
                                                          let x2_4011 = 0 in
                                                          (x1_4010, x2_4011)
                                                        in
                                                        (x1_4014, x2_4015)))); 
PB: x:x_1802
CHECK: snd
       (fst
        (x_1801
          (let x1_4002 = let x1_3994 = true in
                         let x2_3995 = x_3992 in
                         (x1_3994, x2_3995) in
           let x2_4003 = let x1_3998 = false in
                         let x2_3999 = 0 in
                         (x1_3998, x2_3999) in
           (x1_4002, x2_4003))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_4014 = let x1_4006 = true in
                         let x2_4007 = x_3993 in
                         (x1_4006, x2_4007) in
           let x2_4015 = let x1_4010 = false in
                         let x2_4011 = 0 in
                         (x1_4010, x2_4011) in
           (x1_4014, x2_4015))))
compose_let
x_1802:snd
       (fst
        (x_1801
          (let x1_4002 = let x1_3994 = true in
                         let x2_3995 = x_3992 in
                         (x1_3994, x2_3995) in
           let x2_4003 = let x1_3998 = false in
                         let x2_3999 = 0 in
                         (x1_3998, x2_3999) in
           (x1_4002, x2_4003))))

x_1746:snd
       (fst
        (x_1023
          (let x1_4014 = let x1_4006 = true in
                         let x2_4007 = x_3993 in
                         (x1_4006, x2_4007) in
           let x2_4015 = let x1_4010 = false in
                         let x2_4011 = 0 in
                         (x1_4010, x2_4011) in
           (x1_4014, x2_4015))))

ADD: (x_x_4018:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861)))
x_1802
x_1747
compose: x_1802, snd
                 (fst
                  (x_1801
                    (let x1_4047 = let x1_4039 = true in
                                   let x2_4040 = x_4037 in
                                   (x1_4039, x2_4040) in
                     let x2_4048 = let x1_4043 = false in
                                   let x2_4044 = 0 in
                                   (x1_4043, x2_4044) in
                     (x1_4047, x2_4048)))); x_1747, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_4059 =
                                                          let x1_4051 = false in
                                                          let x2_4052 = 0 in
                                                          (x1_4051, x2_4052)
                                                        in
                                                        let x2_4060 =
                                                          let x1_4055 = true in
                                                          let x2_4056 = x_4038 in
                                                          (x1_4055, x2_4056)
                                                        in
                                                        (x1_4059, x2_4060)))); 
PB: x:x_1802
CHECK: snd
       (fst
        (x_1801
          (let x1_4047 = let x1_4039 = true in
                         let x2_4040 = x_4037 in
                         (x1_4039, x2_4040) in
           let x2_4048 = let x1_4043 = false in
                         let x2_4044 = 0 in
                         (x1_4043, x2_4044) in
           (x1_4047, x2_4048))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4059 = let x1_4051 = false in
                         let x2_4052 = 0 in
                         (x1_4051, x2_4052) in
           let x2_4060 = let x1_4055 = true in
                         let x2_4056 = x_4038 in
                         (x1_4055, x2_4056) in
           (x1_4059, x2_4060))))
compose_let
x_1802:snd
       (fst
        (x_1801
          (let x1_4047 = let x1_4039 = true in
                         let x2_4040 = x_4037 in
                         (x1_4039, x2_4040) in
           let x2_4048 = let x1_4043 = false in
                         let x2_4044 = 0 in
                         (x1_4043, x2_4044) in
           (x1_4047, x2_4048))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4059 = let x1_4051 = false in
                         let x2_4052 = 0 in
                         (x1_4051, x2_4052) in
           let x2_4060 = let x1_4055 = true in
                         let x2_4056 = x_4038 in
                         (x1_4055, x2_4056) in
           (x1_4059, x2_4060))))

ADD: (x_x_4063:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861)))
x_1746
x_1747
compose: x_1746, snd
                 (fst
                  (x_1023
                    (let x1_4092 = let x1_4084 = true in
                                   let x2_4085 = x_4082 in
                                   (x1_4084, x2_4085) in
                     let x2_4093 = let x1_4088 = false in
                                   let x2_4089 = 0 in
                                   (x1_4088, x2_4089) in
                     (x1_4092, x2_4093)))); x_1747, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_4104 =
                                                          let x1_4096 = false in
                                                          let x2_4097 = 0 in
                                                          (x1_4096, x2_4097)
                                                        in
                                                        let x2_4105 =
                                                          let x1_4100 = true in
                                                          let x2_4101 = x_4083 in
                                                          (x1_4100, x2_4101)
                                                        in
                                                        (x1_4104, x2_4105)))); 
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_4092 = let x1_4084 = true in
                         let x2_4085 = x_4082 in
                         (x1_4084, x2_4085) in
           let x2_4093 = let x1_4088 = false in
                         let x2_4089 = 0 in
                         (x1_4088, x2_4089) in
           (x1_4092, x2_4093))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4104 = let x1_4096 = false in
                         let x2_4097 = 0 in
                         (x1_4096, x2_4097) in
           let x2_4105 = let x1_4100 = true in
                         let x2_4101 = x_4083 in
                         (x1_4100, x2_4101) in
           (x1_4104, x2_4105))))
compose_let
x_1746:snd
       (fst
        (x_1023
          (let x1_4092 = let x1_4084 = true in
                         let x2_4085 = x_4082 in
                         (x1_4084, x2_4085) in
           let x2_4093 = let x1_4088 = false in
                         let x2_4089 = 0 in
                         (x1_4088, x2_4089) in
           (x1_4092, x2_4093))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4104 = let x1_4096 = false in
                         let x2_4097 = 0 in
                         (x1_4096, x2_4097) in
           let x2_4105 = let x1_4100 = true in
                         let x2_4101 = x_4083 in
                         (x1_4100, x2_4101) in
           (x1_4104, x2_4105))))

ADD: (x_x_4108:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257)))
x_1747
x_1746
x_1747
compose: x_1747, snd
                 (snd
                  (x_1023
                    (let x1_4138 = let x1_4130 = false in
                                   let x2_4131 = 0 in
                                   (x1_4130, x2_4131) in
                     let x2_4139 = let x1_4134 = true in
                                   let x2_4135 = x_4127 in
                                   (x1_4134, x2_4135) in
                     (x1_4138, x2_4139)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_4150 =
                                                          let x1_4142 = true in
                                                          let x2_4143 = x_4128 in
                                                          (x1_4142, x2_4143)
                                                        in
                                                        let x2_4151 =
                                                          let x1_4146 = false in
                                                          let x2_4147 = 0 in
                                                          (x1_4146, x2_4147)
                                                        in
                                                        (x1_4150, x2_4151)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_4162 = let x1_4154 = false in
                  let x2_4155 = 0 in
                  (x1_4154, x2_4155) in
    let x2_4163 = let x1_4158 = true in
                  let x2_4159 = x_4129 in
                  (x1_4158, x2_4159) in
    (x1_4162, x2_4163)))); 
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4138 = let x1_4130 = false in
                         let x2_4131 = 0 in
                         (x1_4130, x2_4131) in
           let x2_4139 = let x1_4134 = true in
                         let x2_4135 = x_4127 in
                         (x1_4134, x2_4135) in
           (x1_4138, x2_4139))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_4150 = let x1_4142 = true in
                         let x2_4143 = x_4128 in
                         (x1_4142, x2_4143) in
           let x2_4151 = let x1_4146 = false in
                         let x2_4147 = 0 in
                         (x1_4146, x2_4147) in
           (x1_4150, x2_4151))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4162 = let x1_4154 = false in
                         let x2_4155 = 0 in
                         (x1_4154, x2_4155) in
           let x2_4163 = let x1_4158 = true in
                         let x2_4159 = x_4129 in
                         (x1_4158, x2_4159) in
           (x1_4162, x2_4163))))
compose_let
x_1747:snd
       (snd
        (x_1023
          (let x1_4138 = let x1_4130 = false in
                         let x2_4131 = 0 in
                         (x1_4130, x2_4131) in
           let x2_4139 = let x1_4134 = true in
                         let x2_4135 = x_4127 in
                         (x1_4134, x2_4135) in
           (x1_4138, x2_4139))))

x_1746:snd
       (fst
        (x_1023
          (let x1_4150 = let x1_4142 = true in
                         let x2_4143 = x_4128 in
                         (x1_4142, x2_4143) in
           let x2_4151 = let x1_4146 = false in
                         let x2_4147 = 0 in
                         (x1_4146, x2_4147) in
           (x1_4150, x2_4151))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4162 = let x1_4154 = false in
                         let x2_4155 = 0 in
                         (x1_4154, x2_4155) in
           let x2_4163 = let x1_4158 = true in
                         let x2_4159 = x_4129 in
                         (x1_4158, x2_4159) in
           (x1_4162, x2_4163))))

ADD: (x_x_x_4166:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0))
x_1747
x_1746
compose: x_1747, snd
                 (snd
                  (x_1023
                    (let x1_4196 = let x1_4188 = false in
                                   let x2_4189 = 0 in
                                   (x1_4188, x2_4189) in
                     let x2_4197 = let x1_4192 = true in
                                   let x2_4193 = x_4186 in
                                   (x1_4192, x2_4193) in
                     (x1_4196, x2_4197)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_4208 =
                                                          let x1_4200 = true in
                                                          let x2_4201 = x_4187 in
                                                          (x1_4200, x2_4201)
                                                        in
                                                        let x2_4209 =
                                                          let x1_4204 = false in
                                                          let x2_4205 = 0 in
                                                          (x1_4204, x2_4205)
                                                        in
                                                        (x1_4208, x2_4209)))); 
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4196 = let x1_4188 = false in
                         let x2_4189 = 0 in
                         (x1_4188, x2_4189) in
           let x2_4197 = let x1_4192 = true in
                         let x2_4193 = x_4186 in
                         (x1_4192, x2_4193) in
           (x1_4196, x2_4197))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_4208 = let x1_4200 = true in
                         let x2_4201 = x_4187 in
                         (x1_4200, x2_4201) in
           let x2_4209 = let x1_4204 = false in
                         let x2_4205 = 0 in
                         (x1_4204, x2_4205) in
           (x1_4208, x2_4209))))
compose_let
x_1747:snd
       (snd
        (x_1023
          (let x1_4196 = let x1_4188 = false in
                         let x2_4189 = 0 in
                         (x1_4188, x2_4189) in
           let x2_4197 = let x1_4192 = true in
                         let x2_4193 = x_4186 in
                         (x1_4192, x2_4193) in
           (x1_4196, x2_4197))))

x_1746:snd
       (fst
        (x_1023
          (let x1_4208 = let x1_4200 = true in
                         let x2_4201 = x_4187 in
                         (x1_4200, x2_4201) in
           let x2_4209 = let x1_4204 = false in
                         let x2_4205 = 0 in
                         (x1_4204, x2_4205) in
           (x1_4208, x2_4209))))

ADD: (x_x_4212:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257)))
x_1747
x_1747
compose: x_1747, snd
                 (snd
                  (x_1023
                    (let x1_4241 = let x1_4233 = false in
                                   let x2_4234 = 0 in
                                   (x1_4233, x2_4234) in
                     let x2_4242 = let x1_4237 = true in
                                   let x2_4238 = x_4231 in
                                   (x1_4237, x2_4238) in
                     (x1_4241, x2_4242)))); x_1747, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_4253 =
                                                          let x1_4245 = false in
                                                          let x2_4246 = 0 in
                                                          (x1_4245, x2_4246)
                                                        in
                                                        let x2_4254 =
                                                          let x1_4249 = true in
                                                          let x2_4250 = x_4232 in
                                                          (x1_4249, x2_4250)
                                                        in
                                                        (x1_4253, x2_4254)))); 
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4241 = let x1_4233 = false in
                         let x2_4234 = 0 in
                         (x1_4233, x2_4234) in
           let x2_4242 = let x1_4237 = true in
                         let x2_4238 = x_4231 in
                         (x1_4237, x2_4238) in
           (x1_4241, x2_4242))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4253 = let x1_4245 = false in
                         let x2_4246 = 0 in
                         (x1_4245, x2_4246) in
           let x2_4254 = let x1_4249 = true in
                         let x2_4250 = x_4232 in
                         (x1_4249, x2_4250) in
           (x1_4253, x2_4254))))
compose_let
x_1747:snd
       (snd
        (x_1023
          (let x1_4241 = let x1_4233 = false in
                         let x2_4234 = 0 in
                         (x1_4233, x2_4234) in
           let x2_4242 = let x1_4237 = true in
                         let x2_4238 = x_4231 in
                         (x1_4237, x2_4238) in
           (x1_4241, x2_4242))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4253 = let x1_4245 = false in
                         let x2_4246 = 0 in
                         (x1_4245, x2_4246) in
           let x2_4254 = let x1_4249 = true in
                         let x2_4250 = x_4232 in
                         (x1_4249, x2_4250) in
           (x1_4253, x2_4254))))

ADD: (x_x_4257:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257)))
x_1746
x_1747
compose: x_1746, snd
                 (fst
                  (x_1023
                    (let x1_4286 = let x1_4278 = true in
                                   let x2_4279 = x_4276 in
                                   (x1_4278, x2_4279) in
                     let x2_4287 = let x1_4282 = false in
                                   let x2_4283 = 0 in
                                   (x1_4282, x2_4283) in
                     (x1_4286, x2_4287)))); x_1747, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_4298 =
                                                          let x1_4290 = false in
                                                          let x2_4291 = 0 in
                                                          (x1_4290, x2_4291)
                                                        in
                                                        let x2_4299 =
                                                          let x1_4294 = true in
                                                          let x2_4295 = x_4277 in
                                                          (x1_4294, x2_4295)
                                                        in
                                                        (x1_4298, x2_4299)))); 
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_4286 = let x1_4278 = true in
                         let x2_4279 = x_4276 in
                         (x1_4278, x2_4279) in
           let x2_4287 = let x1_4282 = false in
                         let x2_4283 = 0 in
                         (x1_4282, x2_4283) in
           (x1_4286, x2_4287))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4298 = let x1_4290 = false in
                         let x2_4291 = 0 in
                         (x1_4290, x2_4291) in
           let x2_4299 = let x1_4294 = true in
                         let x2_4295 = x_4277 in
                         (x1_4294, x2_4295) in
           (x1_4298, x2_4299))))
compose_let
x_1746:snd
       (fst
        (x_1023
          (let x1_4286 = let x1_4278 = true in
                         let x2_4279 = x_4276 in
                         (x1_4278, x2_4279) in
           let x2_4287 = let x1_4282 = false in
                         let x2_4283 = 0 in
                         (x1_4282, x2_4283) in
           (x1_4286, x2_4287))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4298 = let x1_4290 = false in
                         let x2_4291 = 0 in
                         (x1_4290, x2_4291) in
           let x2_4299 = let x1_4294 = true in
                         let x2_4295 = x_4277 in
                         (x1_4294, x2_4295) in
           (x1_4298, x2_4299))))

ADD: (x_x_4302:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198)))
x_1812
TUPLE: (true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123)))
x_1825
x_1826
compose: x_1825, snd
                 (#1
                  (x_1823
                    (let x1_4337 = let x1_4325 = false in
                                   let x2_4326 = 0 in
                                   (x1_4325, x2_4326) in
                     let x2_4338 = let x1_4329 = true in
                                   let x2_4330 = x_4323 in
                                   (x1_4329, x2_4330) in
                     let x3_4339 = let x1_4333 = false in
                                   let x2_4334 = 0 in
                                   (x1_4333, x2_4334) in
                     (x1_4337, x2_4338, x3_4339)))); x_1826, snd
                                                             (#2
                                                              (x_1823
                                                                (let x1_4355 =
                                                                   let x1_4343 = false in
                                                                   let x2_4344 = 0 in
                                                                   (x1_4343, x2_4344)
                                                                 in
                                                                 let x2_4356 =
                                                                   let x1_4347 = false in
                                                                   let x2_4348 = 0 in
                                                                   (x1_4347, x2_4348)
                                                                 in
                                                                 let x3_4357 =
                                                                   let x1_4351 = true in
                                                                   let x2_4352 = x_4324 in
                                                                   (x1_4351, x2_4352)
                                                                 in
                                                                 (x1_4355, x2_4356, x3_4357)))); 
PB: x:x_1825
CHECK: snd
       (#1
        (x_1823
          (let x1_4337 = let x1_4325 = false in
                         let x2_4326 = 0 in
                         (x1_4325, x2_4326) in
           let x2_4338 = let x1_4329 = true in
                         let x2_4330 = x_4323 in
                         (x1_4329, x2_4330) in
           let x3_4339 = let x1_4333 = false in
                         let x2_4334 = 0 in
                         (x1_4333, x2_4334) in
           (x1_4337, x2_4338, x3_4339))))
PB: x:x_1826
CHECK: snd
       (#2
        (x_1823
          (let x1_4355 = let x1_4343 = false in
                         let x2_4344 = 0 in
                         (x1_4343, x2_4344) in
           let x2_4356 = let x1_4347 = false in
                         let x2_4348 = 0 in
                         (x1_4347, x2_4348) in
           let x3_4357 = let x1_4351 = true in
                         let x2_4352 = x_4324 in
                         (x1_4351, x2_4352) in
           (x1_4355, x2_4356, x3_4357))))
compose_let
x_1825:snd
       (#1
        (x_1823
          (let x1_4337 = let x1_4325 = false in
                         let x2_4326 = 0 in
                         (x1_4325, x2_4326) in
           let x2_4338 = let x1_4329 = true in
                         let x2_4330 = x_4323 in
                         (x1_4329, x2_4330) in
           let x3_4339 = let x1_4333 = false in
                         let x2_4334 = 0 in
                         (x1_4333, x2_4334) in
           (x1_4337, x2_4338, x3_4339))))

x_1826:snd
       (#2
        (x_1823
          (let x1_4355 = let x1_4343 = false in
                         let x2_4344 = 0 in
                         (x1_4343, x2_4344) in
           let x2_4356 = let x1_4347 = false in
                         let x2_4348 = 0 in
                         (x1_4347, x2_4348) in
           let x3_4357 = let x1_4351 = true in
                         let x2_4352 = x_4324 in
                         (x1_4351, x2_4352) in
           (x1_4355, x2_4356, x3_4357))))

ADD: (x_x_4361:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
tupled:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_1723 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_1723
                               else
                                 xs_1114 n_1526 in
          x_1723
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            ((true, x_1592 (snd (fst ii_3523))), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_1735 = make_list_1008 (n_1009 - 1) in
    let x_1736 = rand_int () in
    let x_1737 = cons_1117 x_1736 in
    let x_1738 = x_1737 x_1735 in
    let x_1739 i_3447 = snd (fst (x_1738 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_1738 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let rec x_x_4257 x_4231 x_4232 =
    let r_4261 =
      snd
      (snd
       (x_1023
         (let x1_4241 = let x1_4233 = false in
                        let x2_4234 = 0 in
                        (x1_4233, x2_4234) in
          let x2_4242 = let x1_4237 = true in
                        let x2_4238 = x_4231 in
                        (x1_4237, x2_4238) in
          (x1_4241, x2_4242))))
    in
    let r_4262 =
      snd
      (snd
       (x_1023
         (let x1_4253 = let x1_4245 = false in
                        let x2_4246 = 0 in
                        (x1_4245, x2_4246) in
          let x2_4254 = let x1_4249 = true in
                        let x2_4250 = x_4232 in
                        (x1_4249, x2_4250) in
          (x1_4253, x2_4254))))
    in
    (r_4261, r_4262)
  in
  let rec x_x_3820 x_3794 x_3795 =
    let r_3824 =
      snd
      (fst
       (x_1023
         (let x1_3804 = let x1_3796 = true in
                        let x2_3797 = x_3794 in
                        (x1_3796, x2_3797) in
          let x2_3805 = let x1_3800 = false in
                        let x2_3801 = 0 in
                        (x1_3800, x2_3801) in
          (x1_3804, x2_3805))))
    in
    let r_3825 =
      snd
      (snd
       (x_1023
         (let x1_3816 = let x1_3808 = false in
                        let x2_3809 = 0 in
                        (x1_3808, x2_3809) in
          let x2_3817 = let x1_3812 = true in
                        let x2_3813 = x_3795 in
                        (x1_3812, x2_3813) in
          (x1_3816, x2_3817))))
    in
    (r_3824, r_3825)
  in
  let rec x_x_4108 x_4082 x_4083 =
    let r_4112 =
      snd
      (fst
       (x_1023
         (let x1_4092 = let x1_4084 = true in
                        let x2_4085 = x_4082 in
                        (x1_4084, x2_4085) in
          let x2_4093 = let x1_4088 = false in
                        let x2_4089 = 0 in
                        (x1_4088, x2_4089) in
          (x1_4092, x2_4093))))
    in
    let r_4113 =
      snd
      (snd
       (x_1023
         (let x1_4104 = let x1_4096 = false in
                        let x2_4097 = 0 in
                        (x1_4096, x2_4097) in
          let x2_4105 = let x1_4100 = true in
                        let x2_4101 = x_4083 in
                        (x1_4100, x2_4101) in
          (x1_4104, x2_4105))))
    in
    (r_4112, r_4113)
  in
  let rec x_x_x_4166 x_4127 x_4128 x_4129 =
    let r_4171 =
      snd
      (snd
       (x_1023
         (let x1_4138 = let x1_4130 = false in
                        let x2_4131 = 0 in
                        (x1_4130, x2_4131) in
          let x2_4139 = let x1_4134 = true in
                        let x2_4135 = x_4127 in
                        (x1_4134, x2_4135) in
          (x1_4138, x2_4139))))
    in
    let r_4172 =
      snd
      (fst
       (x_1023
         (let x1_4150 = let x1_4142 = true in
                        let x2_4143 = x_4128 in
                        (x1_4142, x2_4143) in
          let x2_4151 = let x1_4146 = false in
                        let x2_4147 = 0 in
                        (x1_4146, x2_4147) in
          (x1_4150, x2_4151))))
    in
    let r_4173 =
      snd
      (snd
       (x_1023
         (let x1_4162 = let x1_4154 = false in
                        let x2_4155 = 0 in
                        (x1_4154, x2_4155) in
          let x2_4163 = let x1_4158 = true in
                        let x2_4159 = x_4129 in
                        (x1_4158, x2_4159) in
          (x1_4162, x2_4163))))
    in
    (r_4171, r_4172, r_4173)
  in
  let rec x_x_4212 x_4186 x_4187 =
    let r_4216 =
      snd
      (snd
       (x_1023
         (let x1_4196 = let x1_4188 = false in
                        let x2_4189 = 0 in
                        (x1_4188, x2_4189) in
          let x2_4197 = let x1_4192 = true in
                        let x2_4193 = x_4186 in
                        (x1_4192, x2_4193) in
          (x1_4196, x2_4197))))
    in
    let r_4217 =
      snd
      (fst
       (x_1023
         (let x1_4208 = let x1_4200 = true in
                        let x2_4201 = x_4187 in
                        (x1_4200, x2_4201) in
          let x2_4209 = let x1_4204 = false in
                        let x2_4205 = 0 in
                        (x1_4204, x2_4205) in
          (x1_4208, x2_4209))))
    in
    (r_4216, r_4217)
  in
  let rec x_x_4302 x_4276 x_4277 =
    let r_4306 =
      snd
      (fst
       (x_1023
         (let x1_4286 = let x1_4278 = true in
                        let x2_4279 = x_4276 in
                        (x1_4278, x2_4279) in
          let x2_4287 = let x1_4282 = false in
                        let x2_4283 = 0 in
                        (x1_4282, x2_4283) in
          (x1_4286, x2_4287))))
    in
    let r_4307 =
      snd
      (snd
       (x_1023
         (let x1_4298 = let x1_4290 = false in
                        let x2_4291 = 0 in
                        (x1_4290, x2_4291) in
          let x2_4299 = let x1_4294 = true in
                        let x2_4295 = x_4277 in
                        (x1_4294, x2_4295) in
          (x1_4298, x2_4299))))
    in
    (r_4306, r_4307)
  in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            let r_4303 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, fst r_4303), (true, snd r_4303))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_4258 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, fst r_4258), (false, (true, 0)), (true, snd r_4258))
        else
          if fst (#2 iii_3257) = false then
            let r_4213 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, fst r_4213), (true, snd r_4213), (false, (true, 0)))
          else
            let r_4167 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 r_4167), (true, #1 r_4167), (true, #2 r_4167))
    in
    x_1811
  else
    let x_1753 = snd (fst x_3302) in
    if fst x_1753 <> false then
      let xs'_1014 x_1150 =
        let x_3159 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_1764 = snd (fst x_3159) in
        x_1764
      in
      let rec xs'_x_3867 x_3841 x_3842 =
        let x_3159 =
          x_1023
            (let x1_3851 = let x1_3843 = true in
                           let x2_3844 = x_3841 + 1 in
                           (x1_3843, x2_3844) in
             let x2_3852 = let x1_3847 = false in
                           let x2_3848 = 0 in
                           (x1_3847, x2_3848) in
             (x1_3851, x2_3852))
        in
        let x_1764 = snd (fst x_3159) in
        let r_3871 = x_1764 in
        let r_3872 =
          snd
          (snd
           (x_1023
             (let x1_3863 = let x1_3855 = false in
                            let x2_3856 = 0 in
                            (x1_3855, x2_3856) in
              let x2_3864 = let x1_3859 = true in
                            let x2_3860 = x_3842 in
                            (x1_3859, x2_3860) in
              (x1_3863, x2_3864))))
        in
        (r_3871, r_3872)
      in
      let x_1767 = snd (fst x_3302) in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_1774 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_1774
                                 else
                                   xs_1222 n_1544 in
            x_1774
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              ((true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            let r_3868 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst r_3868), (true, snd r_3868))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3919 x_3881 x_3882 =
        let r_3923 =
          snd
          (#1
           (x_1791
             (let x1_3895 = let x1_3883 = false in
                            let x2_3884 = 0 in
                            (x1_3883, x2_3884) in
              let x2_3896 = let x1_3887 = true in
                            let x2_3888 = x_3881 in
                            (x1_3887, x2_3888) in
              let x3_3897 = let x1_3891 = false in
                            let x2_3892 = 0 in
                            (x1_3891, x2_3892) in
              (x1_3895, x2_3896, x3_3897))))
        in
        let r_3924 =
          snd
          (#2
           (x_1791
             (let x1_3913 = let x1_3901 = false in
                            let x2_3902 = 0 in
                            (x1_3901, x2_3902) in
              let x2_3914 = let x1_3905 = false in
                            let x2_3906 = 0 in
                            (x1_3905, x2_3906) in
              let x3_3915 = let x1_3909 = true in
                            let x2_3910 = x_3882 in
                            (x1_3909, x2_3910) in
              (x1_3913, x2_3914, x3_3915))))
        in
        (r_3923, r_3924)
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            let r_3920 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst r_3920), (true, snd r_3920))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let rec x_x_4018 x_3992 x_3993 =
        let r_4022 =
          snd
          (fst
           (x_1801
             (let x1_4002 = let x1_3994 = true in
                            let x2_3995 = x_3992 in
                            (x1_3994, x2_3995) in
              let x2_4003 = let x1_3998 = false in
                            let x2_3999 = 0 in
                            (x1_3998, x2_3999) in
              (x1_4002, x2_4003))))
        in
        let r_4023 =
          snd
          (fst
           (x_1023
             (let x1_4014 = let x1_4006 = true in
                            let x2_4007 = x_3993 in
                            (x1_4006, x2_4007) in
              let x2_4015 = let x1_4010 = false in
                            let x2_4011 = 0 in
                            (x1_4010, x2_4011) in
              (x1_4014, x2_4015))))
        in
        (r_4022, r_4023)
      in
      let rec x_x_4063 x_4037 x_4038 =
        let r_4067 =
          snd
          (fst
           (x_1801
             (let x1_4047 = let x1_4039 = true in
                            let x2_4040 = x_4037 in
                            (x1_4039, x2_4040) in
              let x2_4048 = let x1_4043 = false in
                            let x2_4044 = 0 in
                            (x1_4043, x2_4044) in
              (x1_4047, x2_4048))))
        in
        let r_4068 =
          snd
          (snd
           (x_1023
             (let x1_4059 = let x1_4051 = false in
                            let x2_4052 = 0 in
                            (x1_4051, x2_4052) in
              let x2_4060 = let x1_4055 = true in
                            let x2_4056 = x_4038 in
                            (x1_4055, x2_4056) in
              (x1_4059, x2_4060))))
        in
        (r_4067, r_4068)
      in
      let rec x_x_x_3972 x_3933 x_3934 x_3935 =
        let r_3977 =
          snd
          (fst
           (x_1801
             (let x1_3944 = let x1_3936 = true in
                            let x2_3937 = x_3933 in
                            (x1_3936, x2_3937) in
              let x2_3945 = let x1_3940 = false in
                            let x2_3941 = 0 in
                            (x1_3940, x2_3941) in
              (x1_3944, x2_3945))))
        in
        let r_3978 =
          snd
          (fst
           (x_1023
             (let x1_3956 = let x1_3948 = true in
                            let x2_3949 = x_3934 in
                            (x1_3948, x2_3949) in
              let x2_3957 = let x1_3952 = false in
                            let x2_3953 = 0 in
                            (x1_3952, x2_3953) in
              (x1_3956, x2_3957))))
        in
        let r_3979 =
          snd
          (snd
           (x_1023
             (let x1_3968 = let x1_3960 = false in
                            let x2_3961 = 0 in
                            (x1_3960, x2_3961) in
              let x2_3969 = let x1_3964 = true in
                            let x2_3965 = x_3935 in
                            (x1_3964, x2_3965) in
              (x1_3968, x2_3969))))
        in
        (r_3977, r_3978, r_3979)
      in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              let r_4109 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, fst r_4109), (true, snd r_4109))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_4064 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, fst r_4064), (false, (true, 0)), (true, snd r_4064))
          else
            if fst (#2 iii_2861) = false then
              let r_4019 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, fst r_4019), (true, snd r_4019), (false, (true, 0)))
            else
              let r_3973 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 r_3973), (true, #1 r_3973), (true, #2 r_3973))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              let r_3821 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, fst r_3821), (true, snd r_3821))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              ((true, x_1610 (snd (#0 iii_2432))), (true, x_1746 (snd (#1 iii_2432))), 
               (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_1812 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_1812 (snd (fst ix_2198))), (false, (true, 0)))
      else
        ((true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_1823 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_1823 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_1823 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_1823 ((false, 0), (false, 0), (true, i_2140)))) in
  let rec x_x_4361 x_4323 x_4324 =
    let r_4365 =
      snd
      (#1
       (x_1823
         (let x1_4337 = let x1_4325 = false in
                        let x2_4326 = 0 in
                        (x1_4325, x2_4326) in
          let x2_4338 = let x1_4329 = true in
                        let x2_4330 = x_4323 in
                        (x1_4329, x2_4330) in
          let x3_4339 = let x1_4333 = false in
                        let x2_4334 = 0 in
                        (x1_4333, x2_4334) in
          (x1_4337, x2_4338, x3_4339))))
    in
    let r_4366 =
      snd
      (#2
       (x_1823
         (let x1_4355 = let x1_4343 = false in
                        let x2_4344 = 0 in
                        (x1_4343, x2_4344) in
          let x2_4356 = let x1_4347 = false in
                        let x2_4348 = 0 in
                        (x1_4347, x2_4348) in
          let x3_4357 = let x1_4351 = true in
                        let x2_4352 = x_4324 in
                        (x1_4351, x2_4352) in
          (x1_4355, x2_4356, x3_4357))))
    in
    (r_4365, r_4366)
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        let r_4362 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst r_4362), (true, snd r_4362))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_2095 = x_1823 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1837 = x_1812 i_1016 in
  let x_1832 = snd (#0 x_2095) in
  let n_1504 = if fst x_1832 <> false then
                 snd x_1832
               else
                 _|_ in
  let n_1505 = if fst x_1837 <> false then
                 snd x_1837
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_1844 = rand_int () in
let x_1845 = rand_int () in
let x_1846 = main_1015 x_1844 in
let x_1847 = x_1846 x_1845 in
()

normalize:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_4377 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4377
                               else
                                 xs_1114 n_1526 in
          x_4377
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4459 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4459))
        else
          if fst (snd ii_3523) = false then
            let x_4418 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4418), (false, (true, 0)))
          else
            let x_4395 = x_1592 (snd (fst ii_3523)) in
            let x_4405 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4395), (true, x_4405))
      in
      x_1732
    in
    let x_4505 = make_list_1008 (n_1009 - 1) in
    let x_4507 = rand_int () in
    let x_4508 = cons_1117 x_4507 in
    let x_4509 = x_4508 x_4505 in
    let x_1739 i_3447 = let x_4526 = x_4509 ((true, i_3447), (false, 0)) in
                        snd (fst x_4526) in
    let x_1740 i_3440 = let x_4545 = x_4509 ((false, 0), (true, i_3440)) in
                        snd (snd x_4545) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_4571 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_4571) in
  let x_1747 i_3303 = let x_4590 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_4590) in
  let rec x_x_4257 x_4231 x_4232 =
    let x_4604 = x_1023 ((false, 0), (true, x_4231)) in
    let x_4618 = x_1023 ((false, 0), (true, x_4232)) in
    (snd (snd x_4604), snd (snd x_4618))
  in
  let rec x_x_3820 x_3794 x_3795 =
    let x_4635 = x_1023 ((true, x_3794), (false, 0)) in
    let x_4649 = x_1023 ((false, 0), (true, x_3795)) in
    (snd (fst x_4635), snd (snd x_4649))
  in
  let rec x_x_4108 x_4082 x_4083 =
    let x_4666 = x_1023 ((true, x_4082), (false, 0)) in
    let x_4680 = x_1023 ((false, 0), (true, x_4083)) in
    (snd (fst x_4666), snd (snd x_4680))
  in
  let rec x_x_x_4166 x_4127 x_4128 x_4129 =
    let x_4697 = x_1023 ((false, 0), (true, x_4127)) in
    let x_4711 = x_1023 ((true, x_4128), (false, 0)) in
    let x_4725 = x_1023 ((false, 0), (true, x_4129)) in
    (snd (snd x_4697), snd (fst x_4711), snd (snd x_4725))
  in
  let rec x_x_4212 x_4186 x_4187 =
    let x_4743 = x_1023 ((false, 0), (true, x_4186)) in
    let x_4757 = x_1023 ((true, x_4187), (false, 0)) in
    (snd (snd x_4743), snd (fst x_4757))
  in
  let rec x_x_4302 x_4276 x_4277 =
    let x_4774 = x_1023 ((true, x_4276), (false, 0)) in
    let x_4788 = x_1023 ((false, 0), (true, x_4277)) in
    (snd (fst x_4774), snd (snd x_4788))
  in
  let x_4809 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_4809)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6501 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6501))
        else
          if fst (#2 iii_3257) = false then
            let x_6448 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_6448), (false, (true, 0)))
          else
            let x_6401 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, fst x_6401), (true, snd x_6401))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_6353 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_6353), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6317 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, fst x_6317), (false, (true, 0)), (true, snd x_6317))
        else
          if fst (#2 iii_3257) = false then
            let x_6275 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, fst x_6275), (true, snd x_6275), (false, (true, 0)))
          else
            let x_6243 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_6243), (true, #1 x_6243), (true, #2 x_6243))
    in
    x_1811
  else
    if fst (snd (fst x_4809)) <> false then
      let xs'_1014 x_1150 = let x_5160 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_5160) in
      let rec xs'_x_3867 x_3841 x_3842 =
        let x_5175 = x_1023 ((true, x_3841 + 1), (false, 0)) in
        let x_5190 = x_1023 ((false, 0), (true, x_3842)) in
        (snd (fst x_5175), snd (snd x_5190))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_5201 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_5201
                                 else
                                   xs_1222 n_1544 in
            x_5201
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_5283 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_5283))
          else
            if fst (snd ii_3086) = false then
              let x_5242 = x_1625 (snd (fst ii_3086)) in
              ((true, x_5242), (false, (true, 0)))
            else
              let x_5219 = x_1625 (snd (fst ii_3086)) in
              let x_5229 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_5219), (true, x_5229))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5398 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_5398))
        else
          if fst (snd ii_3004) = false then
            let x_5357 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_5357), (false, (true, 0)))
          else
            let x_5333 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_5333), (true, snd x_5333))
      in
      let x_1789 i_2984 = let x_5458 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_5458) in
      let x_1790 i_2977 = let x_5477 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_5477) in
      let x_5480 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_5504 = x_5480 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_5504) in
      let x_1793 i_2956 = let x_5530 = x_5480 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_5530) in
      let x_1794 i_2946 = let x_5556 = x_5480 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_5556) in
      let rec x_x_3919 x_3881 x_3882 =
        let x_5574 = x_5480 ((false, 0), (true, x_3881), (false, 0)) in
        let x_5592 = x_5480 ((false, 0), (false, 0), (true, x_3882)) in
        (snd (#1 x_5574), snd (#2 x_5592))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5669 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5669))
        else
          if fst (snd ii_2929) = false then
            let x_5628 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5628), (false, (true, 0)))
          else
            let x_5604 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5604), (true, snd x_5604))
      in
      let x_1798 i_2909 = let x_5729 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_5729) in
      let x_1799 i_2902 = let x_5748 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_5748) in
      let x_5752 = cons_1225 (snd (snd (fst x_4809))) in
      let x_5753 = x_5752 x_1792 in
      let x_1802 i_2893 = let x_5770 = x_5753 ((true, i_2893), (false, 0)) in
                          snd (fst x_5770) in
      let rec x_x_4018 x_3992 x_3993 =
        let x_5784 = x_5753 ((true, x_3992), (false, 0)) in
        let x_5798 = x_1023 ((true, x_3993), (false, 0)) in
        (snd (fst x_5784), snd (fst x_5798))
      in
      let rec x_x_4063 x_4037 x_4038 =
        let x_5815 = x_5753 ((true, x_4037), (false, 0)) in
        let x_5829 = x_1023 ((false, 0), (true, x_4038)) in
        (snd (fst x_5815), snd (snd x_5829))
      in
      let rec x_x_x_3972 x_3933 x_3934 x_3935 =
        let x_5846 = x_5753 ((true, x_3933), (false, 0)) in
        let x_5860 = x_1023 ((true, x_3934), (false, 0)) in
        let x_5874 = x_1023 ((false, 0), (true, x_3935)) in
        (snd (fst x_5846), snd (fst x_5860), snd (snd x_5874))
      in
      let x_1803 i_2886 = let x_5897 = x_5753 ((false, 0), (true, i_2886)) in
                          snd (snd x_5897) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6167 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6167))
          else
            if fst (#2 iii_2861) = false then
              let x_6114 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_6114), (false, (true, 0)))
            else
              let x_6067 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, fst x_6067), (true, snd x_6067))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_6019 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_6019), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5983 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, fst x_5983), (false, (true, 0)), (true, snd x_5983))
          else
            if fst (#2 iii_2861) = false then
              let x_5941 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, fst x_5941), (true, snd x_5941), (false, (true, 0)))
            else
              let x_5909 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5909), (true, #1 x_5909), (true, #2 x_5909))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5080 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5080))
          else
            if fst (#2 iii_2432) = false then
              let x_5027 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_5027), (false, (true, 0)))
            else
              let x_4980 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, fst x_4980), (true, snd x_4980))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4932 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4932), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4897 = x_1610 (snd (#0 iii_2432)) in
              let x_4918 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4897), (false, (true, 0)), (true, x_4918))
          else
            if fst (#2 iii_2432) = false then
              let x_4856 = x_1610 (snd (#0 iii_2432)) in
              let x_4866 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4856), (true, x_4866), (false, (true, 0)))
            else
              let x_4822 = x_1610 (snd (#0 iii_2432)) in
              let x_4832 = x_1746 (snd (#1 iii_2432)) in
              let x_4842 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4822), (true, x_4832), (true, x_4842))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6568 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6644 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6644))
    else
      if fst (snd ix_2198) = false then
        let x_6603 = x_6568 (snd (fst ix_2198)) in
        ((true, x_6603), (false, (true, 0)))
      else
        let x_6580 = x_6568 (snd (fst ix_2198)) in
        let x_6590 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6580), (true, x_6590))
  in
  let x_1821 i_2178 = let x_6704 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_6704) in
  let x_1822 x_2171 = let x_6723 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_6723) in
  let x_6726 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_6750 = x_6726 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_6750) in
  let x_1825 i_2150 = let x_6776 = x_6726 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_6776) in
  let x_1826 i_2140 = let x_6802 = x_6726 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_6802) in
  let rec x_x_4361 x_4323 x_4324 =
    let x_6820 = x_6726 ((false, 0), (true, x_4323), (false, 0)) in
    let x_6838 = x_6726 ((false, 0), (false, 0), (true, x_4324)) in
    (snd (#1 x_6820), snd (#2 x_6838))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6915 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6915))
    else
      if fst (snd ii_2123) = false then
        let x_6874 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6874), (false, (true, 0)))
      else
        let x_6850 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6850), (true, snd x_6850))
  in
  let x_1830 i_2103 = let x_6975 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_6975) in
  let x_1831 i_2096 = let x_6994 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6994) in
  let x_7018 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7019 = x_6568 i_1016 in
  let n_1504 = if fst (snd (#0 x_7018)) <> false then
                 snd (snd (#0 x_7018))
               else
                 _|_ in
  let n_1505 = if fst x_7019 <> false then
                 snd x_7019
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_7037 = rand_int () in
let x_7039 = rand_int () in
let x_7040 = main_1015 x_7037 in
let x_7041 = x_7040 x_7039 in
let x_1847 = x_7041 in
()

replace[1]: x_7018
APPS: x_7018 = x_6726 ...0... i_1016 ...
USED: x_7018 = x_6726 ...0... i_1016 ...
MUST: x_7018 = x_6726 ...0... i_1016 ...
NEW: x_7042 = x_6726 ((true, i_1016), (false, 0), (false, 0))
replace[1]: x_6994
APPS: x_6994 = x_1829 ...1... i_2096 ...
USED: x_6994 = x_1829 ...1... i_2096 ...
MUST: x_6994 = x_1829 ...1... i_2096 ...
NEW: x_7053 = x_1829 ((false, 0), (true, i_2096))
replace[1]: x_6975
APPS: x_6975 = x_1829 ...0... i_2103 ...
USED: x_6975 = x_1829 ...0... i_2103 ...
MUST: x_6975 = x_1829 ...0... i_2103 ...
NEW: x_7061 = x_1829 ((true, i_2103), (false, 0))
replace[2]: x_6820
APPS: x_6838 = x_6726 ...2... x_4324 ...
APPS: x_6820 = x_6726 ...1... x_4323 ...
USED: x_6838 = x_6726 ...2... x_4324 ...
USED: x_6820 = x_6726 ...1... x_4323 ...
MUST: x_6820 = x_6726 ...1... x_4323 ...
MUST: x_6838 = x_6726 ...2... x_4324 ...
NEW: x_7069 = x_6726 ((false, 0), (true, x_4323), (true, x_4324))
replace[1]: x_6802
APPS: x_6802 = x_6726 ...2... i_2140 ...
USED: x_6802 = x_6726 ...2... i_2140 ...
MUST: x_6802 = x_6726 ...2... i_2140 ...
NEW: x_7081 = x_6726 ((false, 0), (false, 0), (true, i_2140))
replace[1]: x_6776
APPS: x_6776 = x_6726 ...1... i_2150 ...
USED: x_6776 = x_6726 ...1... i_2150 ...
MUST: x_6776 = x_6726 ...1... i_2150 ...
NEW: x_7092 = x_6726 ((false, 0), (true, i_2150), (false, 0))
replace[1]: x_6750
APPS: x_6750 = x_6726 ...0... i_2160 ...
USED: x_6750 = x_6726 ...0... i_2160 ...
MUST: x_6750 = x_6726 ...0... i_2160 ...
NEW: x_7103 = x_6726 ((true, i_2160), (false, 0), (false, 0))
replace[1]: x_6723
APPS: x_6723 = x_1820 ...1... x_2171 ...
USED: x_6723 = x_1820 ...1... x_2171 ...
MUST: x_6723 = x_1820 ...1... x_2171 ...
NEW: x_7114 = x_1820 ((false, 0), (true, x_2171))
replace[1]: x_6704
APPS: x_6704 = x_1820 ...0... i_2178 ...
USED: x_6704 = x_1820 ...0... i_2178 ...
MUST: x_6704 = x_1820 ...0... i_2178 ...
NEW: x_7122 = x_1820 ((true, i_2178), (false, 0))
replace[1]: x_5897
APPS: x_5897 = x_5753 ...1... i_2886 ...
USED: x_5897 = x_5753 ...1... i_2886 ...
MUST: x_5897 = x_5753 ...1... i_2886 ...
NEW: x_7130 = x_5753 ((false, 0), (true, i_2886))
replace[2]: x_5860
APPS: x_5874 = x_1023 ...1... x_3935 ...
APPS: x_5860 = x_1023 ...0... x_3934 ...
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_5874 = x_1023 ...1... x_3935 ...
USED: x_5860 = x_1023 ...0... x_3934 ...
MUST: x_5860 = x_1023 ...0... x_3934 ...
MUST: x_5874 = x_1023 ...1... x_3935 ...
NEW: x_7138 = x_1023 ((true, x_3934), (true, x_3935))
replace[1]: x_5846
APPS: x_5846 = x_5753 ...0... x_3933 ...
USED: x_5846 = x_5753 ...0... x_3933 ...
MUST: x_5846 = x_5753 ...0... x_3933 ...
NEW: x_7147 = x_5753 ((true, x_3933), (false, 0))
replace[1]: x_5829
APPS: x_5829 = x_1023 ...1... x_4038 ...
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_5829 = x_1023 ...1... x_4038 ...
MUST: x_5829 = x_1023 ...1... x_4038 ...
NEW: x_7155 = x_1023 ((false, 0), (true, x_4038))
replace[1]: x_5815
APPS: x_5815 = x_5753 ...0... x_4037 ...
USED: x_5815 = x_5753 ...0... x_4037 ...
MUST: x_5815 = x_5753 ...0... x_4037 ...
NEW: x_7163 = x_5753 ((true, x_4037), (false, 0))
replace[1]: x_5798
APPS: x_5798 = x_1023 ...0... x_3993 ...
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_5798 = x_1023 ...0... x_3993 ...
MUST: x_5798 = x_1023 ...0... x_3993 ...
NEW: x_7171 = x_1023 ((true, x_3993), (false, 0))
replace[1]: x_5784
APPS: x_5784 = x_5753 ...0... x_3992 ...
USED: x_5784 = x_5753 ...0... x_3992 ...
MUST: x_5784 = x_5753 ...0... x_3992 ...
NEW: x_7179 = x_5753 ((true, x_3992), (false, 0))
replace[1]: x_5770
APPS: x_5770 = x_5753 ...0... i_2893 ...
USED: x_5770 = x_5753 ...0... i_2893 ...
MUST: x_5770 = x_5753 ...0... i_2893 ...
NEW: x_7187 = x_5753 ((true, i_2893), (false, 0))
replace[1]: x_5748
APPS: x_5748 = x_1797 ...1... i_2902 ...
USED: x_5748 = x_1797 ...1... i_2902 ...
MUST: x_5748 = x_1797 ...1... i_2902 ...
NEW: x_7195 = x_1797 ((false, 0), (true, i_2902))
replace[1]: x_5729
APPS: x_5729 = x_1797 ...0... i_2909 ...
USED: x_5729 = x_1797 ...0... i_2909 ...
MUST: x_5729 = x_1797 ...0... i_2909 ...
NEW: x_7203 = x_1797 ((true, i_2909), (false, 0))
replace[2]: x_5574
APPS: x_5592 = x_5480 ...2... x_3882 ...
APPS: x_5574 = x_5480 ...1... x_3881 ...
USED: x_5592 = x_5480 ...2... x_3882 ...
USED: x_5574 = x_5480 ...1... x_3881 ...
MUST: x_5574 = x_5480 ...1... x_3881 ...
MUST: x_5592 = x_5480 ...2... x_3882 ...
NEW: x_7211 = x_5480 ((false, 0), (true, x_3881), (true, x_3882))
replace[1]: x_5556
APPS: x_5556 = x_5480 ...2... i_2946 ...
USED: x_5556 = x_5480 ...2... i_2946 ...
MUST: x_5556 = x_5480 ...2... i_2946 ...
NEW: x_7223 = x_5480 ((false, 0), (false, 0), (true, i_2946))
replace[1]: x_5530
APPS: x_5530 = x_5480 ...1... i_2956 ...
USED: x_5530 = x_5480 ...1... i_2956 ...
MUST: x_5530 = x_5480 ...1... i_2956 ...
NEW: x_7234 = x_5480 ((false, 0), (true, i_2956), (false, 0))
replace[1]: x_5504
APPS: x_5504 = x_5480 ...0... i_2966 ...
USED: x_5504 = x_5480 ...0... i_2966 ...
MUST: x_5504 = x_5480 ...0... i_2966 ...
NEW: x_7245 = x_5480 ((true, i_2966), (false, 0), (false, 0))
replace[1]: x_5477
APPS: x_5477 = x_1788 ...1... i_2977 ...
USED: x_5477 = x_1788 ...1... i_2977 ...
MUST: x_5477 = x_1788 ...1... i_2977 ...
NEW: x_7256 = x_1788 ((false, 0), (true, i_2977))
replace[1]: x_5458
APPS: x_5458 = x_1788 ...0... i_2984 ...
USED: x_5458 = x_1788 ...0... i_2984 ...
MUST: x_5458 = x_1788 ...0... i_2984 ...
NEW: x_7264 = x_1788 ((true, i_2984), (false, 0))
replace[2]: x_5175
APPS: x_5190 = x_1023 ...1... x_3842 ...
APPS: x_5175 = x_1023 ...0... x_3841 + 1 ...
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_5190 = x_1023 ...1... x_3842 ...
USED: x_5175 = x_1023 ...0... x_3841 + 1 ...
MUST: x_5175 = x_1023 ...0... x_3841 + 1 ...
MUST: x_5190 = x_1023 ...1... x_3842 ...
NEW: x_7272 = x_1023 ((true, x_3841 + 1), (true, x_3842))
replace[1]: x_5160
APPS: x_5160 = x_1023 ...0... x_1150 + 1 ...
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_5160 = x_1023 ...0... x_1150 + 1 ...
MUST: x_5160 = x_1023 ...0... x_1150 + 1 ...
NEW: x_7281 = x_1023 ((true, x_1150 + 1), (false, 0))
replace[1]: x_4809
APPS: x_4809 = x_1023 ...0... 0 ...
USED: x_4809 = x_1023 ...0... 0 ...
MUST: x_4809 = x_1023 ...0... 0 ...
NEW: x_7289 = x_1023 ((true, 0), (false, 0))
replace[2]: x_4774
APPS: x_4788 = x_1023 ...1... x_4277 ...
APPS: x_4774 = x_1023 ...0... x_4276 ...
USED: x_4788 = x_1023 ...1... x_4277 ...
USED: x_4774 = x_1023 ...0... x_4276 ...
MUST: x_4774 = x_1023 ...0... x_4276 ...
MUST: x_4788 = x_1023 ...1... x_4277 ...
NEW: x_7297 = x_1023 ((true, x_4276), (true, x_4277))
replace[2]: x_4743
APPS: x_4757 = x_1023 ...0... x_4187 ...
APPS: x_4743 = x_1023 ...1... x_4186 ...
USED: x_4757 = x_1023 ...0... x_4187 ...
USED: x_4743 = x_1023 ...1... x_4186 ...
MUST: x_4743 = x_1023 ...1... x_4186 ...
MUST: x_4757 = x_1023 ...0... x_4187 ...
NEW: x_7306 = x_1023 ((true, x_4187), (true, x_4186))
replace[3]: x_4697
APPS: x_4725 = x_1023 ...1... x_4129 ...
APPS: x_4711 = x_1023 ...0... x_4128 ...
APPS: x_4697 = x_1023 ...1... x_4127 ...
USED: x_4725 = x_1023 ...1... x_4129 ...
USED: x_4711 = x_1023 ...0... x_4128 ...
USED: x_4697 = x_1023 ...1... x_4127 ...
MUST: x_4697 = x_1023 ...1... x_4127 ...
MUST: x_4711 = x_1023 ...0... x_4128 ...
MUST: x_4725 = x_1023 ...1... x_4129 ...
replace[2]: x_4711
APPS: x_4725 = x_1023 ...1... x_4129 ...
APPS: x_4711 = x_1023 ...0... x_4128 ...
USED: x_4725 = x_1023 ...1... x_4129 ...
USED: x_4711 = x_1023 ...0... x_4128 ...
MUST: x_4711 = x_1023 ...0... x_4128 ...
MUST: x_4725 = x_1023 ...1... x_4129 ...
NEW: x_7316 = x_1023 ((true, x_4128), (true, x_4129))
replace[2]: x_4666
APPS: x_4680 = x_1023 ...1... x_4083 ...
APPS: x_4666 = x_1023 ...0... x_4082 ...
USED: x_4680 = x_1023 ...1... x_4083 ...
USED: x_4666 = x_1023 ...0... x_4082 ...
MUST: x_4666 = x_1023 ...0... x_4082 ...
MUST: x_4680 = x_1023 ...1... x_4083 ...
NEW: x_7325 = x_1023 ((true, x_4082), (true, x_4083))
replace[2]: x_4635
APPS: x_4649 = x_1023 ...1... x_3795 ...
APPS: x_4635 = x_1023 ...0... x_3794 ...
USED: x_4649 = x_1023 ...1... x_3795 ...
USED: x_4635 = x_1023 ...0... x_3794 ...
MUST: x_4635 = x_1023 ...0... x_3794 ...
MUST: x_4649 = x_1023 ...1... x_3795 ...
NEW: x_7334 = x_1023 ((true, x_3794), (true, x_3795))
replace[2]: x_4604
APPS: x_4618 = x_1023 ...1... x_4232 ...
APPS: x_4604 = x_1023 ...1... x_4231 ...
USED: x_4618 = x_1023 ...1... x_4232 ...
USED: x_4604 = x_1023 ...1... x_4231 ...
MUST: x_4604 = x_1023 ...1... x_4231 ...
MUST: x_4618 = x_1023 ...1... x_4232 ...
replace[1]: x_4618
APPS: x_4618 = x_1023 ...1... x_4232 ...
USED: x_4618 = x_1023 ...1... x_4232 ...
MUST: x_4618 = x_1023 ...1... x_4232 ...
NEW: x_7344 = x_1023 ((false, 0), (true, x_4232))
replace[1]: x_4590
APPS: x_4590 = x_1023 ...1... i_3303 ...
USED: x_4590 = x_1023 ...1... i_3303 ...
MUST: x_4590 = x_1023 ...1... i_3303 ...
NEW: x_7352 = x_1023 ((false, 0), (true, i_3303))
replace[1]: x_4571
APPS: x_4571 = x_1023 ...0... i_3310 ...
USED: x_4571 = x_1023 ...0... i_3310 ...
MUST: x_4571 = x_1023 ...0... i_3310 ...
NEW: x_7360 = x_1023 ((true, i_3310), (false, 0))
replace[1]: x_4545
APPS: x_4545 = x_4509 ...1... i_3440 ...
USED: x_4545 = x_4509 ...1... i_3440 ...
MUST: x_4545 = x_4509 ...1... i_3440 ...
NEW: x_7368 = x_4509 ((false, 0), (true, i_3440))
replace[1]: x_4526
APPS: x_4526 = x_4509 ...0... i_3447 ...
USED: x_4526 = x_4509 ...0... i_3447 ...
MUST: x_4526 = x_4509 ...0... i_3447 ...
NEW: x_7376 = x_4509 ((true, i_3447), (false, 0))
replace_app:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_4377 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4377
                               else
                                 xs_1114 n_1526 in
          x_4377
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4459 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4459))
        else
          if fst (snd ii_3523) = false then
            let x_4418 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4418), (false, (true, 0)))
          else
            let x_4395 = x_1592 (snd (fst ii_3523)) in
            let x_4405 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4395), (true, x_4405))
      in
      x_1732
    in
    let x_4505 = make_list_1008 (n_1009 - 1) in
    let x_4507 = rand_int () in
    let x_4508 = cons_1117 x_4507 in
    let x_4509 = x_4508 x_4505 in
    let x_1739 i_3447 =
      let x_4526 = x_4509 ((true, i_3447), (false, 0)) in
      let x_7376 = x_4509 ((true, i_3447), (false, 0)) in
      snd (fst x_7376)
    in
    let x_1740 i_3440 =
      let x_4545 = x_4509 ((false, 0), (true, i_3440)) in
      let x_7368 = x_4509 ((false, 0), (true, i_3440)) in
      snd (snd x_7368)
    in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 =
    let x_4571 = x_1023 ((true, i_3310), (false, 0)) in
    let x_7360 = x_1023 ((true, i_3310), (false, 0)) in
    snd (fst x_7360)
  in
  let x_1747 i_3303 =
    let x_4590 = x_1023 ((false, 0), (true, i_3303)) in
    let x_7352 = x_1023 ((false, 0), (true, i_3303)) in
    snd (snd x_7352)
  in
  let rec x_x_4257 x_4231 x_4232 =
    let x_4604 = x_1023 ((false, 0), (true, x_4231)) in
    let x_4618 = x_1023 ((false, 0), (true, x_4232)) in
    let x_7344 = x_1023 ((false, 0), (true, x_4232)) in
    (snd (snd x_4604), snd (snd x_7344))
  in
  let rec x_x_3820 x_3794 x_3795 =
    let x_4635 = x_1023 ((true, x_3794), (false, 0)) in
    let x_4649 = x_1023 ((false, 0), (true, x_3795)) in
    let x_7334 = x_1023 ((true, x_3794), (true, x_3795)) in
    (snd (fst x_7334), snd (snd x_7334))
  in
  let rec x_x_4108 x_4082 x_4083 =
    let x_4666 = x_1023 ((true, x_4082), (false, 0)) in
    let x_4680 = x_1023 ((false, 0), (true, x_4083)) in
    let x_7325 = x_1023 ((true, x_4082), (true, x_4083)) in
    (snd (fst x_7325), snd (snd x_7325))
  in
  let rec x_x_x_4166 x_4127 x_4128 x_4129 =
    let x_4697 = x_1023 ((false, 0), (true, x_4127)) in
    let x_4711 = x_1023 ((true, x_4128), (false, 0)) in
    let x_4725 = x_1023 ((false, 0), (true, x_4129)) in
    let x_7316 = x_1023 ((true, x_4128), (true, x_4129)) in
    (snd (snd x_4697), snd (fst x_7316), snd (snd x_7316))
  in
  let rec x_x_4212 x_4186 x_4187 =
    let x_4743 = x_1023 ((false, 0), (true, x_4186)) in
    let x_4757 = x_1023 ((true, x_4187), (false, 0)) in
    let x_7306 = x_1023 ((true, x_4187), (true, x_4186)) in
    (snd (snd x_7306), snd (fst x_7306))
  in
  let rec x_x_4302 x_4276 x_4277 =
    let x_4774 = x_1023 ((true, x_4276), (false, 0)) in
    let x_4788 = x_1023 ((false, 0), (true, x_4277)) in
    let x_7297 = x_1023 ((true, x_4276), (true, x_4277)) in
    (snd (fst x_7297), snd (snd x_7297))
  in
  let x_4809 = x_1023 ((true, 0), (false, 0)) in
  let x_7289 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7289)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6501 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6501))
        else
          if fst (#2 iii_3257) = false then
            let x_6448 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_6448), (false, (true, 0)))
          else
            let x_6401 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, fst x_6401), (true, snd x_6401))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_6353 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_6353), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6317 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, fst x_6317), (false, (true, 0)), (true, snd x_6317))
        else
          if fst (#2 iii_3257) = false then
            let x_6275 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, fst x_6275), (true, snd x_6275), (false, (true, 0)))
          else
            let x_6243 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_6243), (true, #1 x_6243), (true, #2 x_6243))
    in
    x_1811
  else
    if fst (snd (fst x_7289)) <> false then
      let xs'_1014 x_1150 =
        let x_5160 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_7281 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        snd (fst x_7281)
      in
      let rec xs'_x_3867 x_3841 x_3842 =
        let x_5175 = x_1023 ((true, x_3841 + 1), (false, 0)) in
        let x_5190 = x_1023 ((false, 0), (true, x_3842)) in
        let x_7272 = x_1023 ((true, x_3841 + 1), (true, x_3842)) in
        (snd (fst x_7272), snd (snd x_7272))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_5201 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_5201
                                 else
                                   xs_1222 n_1544 in
            x_5201
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_5283 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_5283))
          else
            if fst (snd ii_3086) = false then
              let x_5242 = x_1625 (snd (fst ii_3086)) in
              ((true, x_5242), (false, (true, 0)))
            else
              let x_5219 = x_1625 (snd (fst ii_3086)) in
              let x_5229 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_5219), (true, x_5229))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5398 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_5398))
        else
          if fst (snd ii_3004) = false then
            let x_5357 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_5357), (false, (true, 0)))
          else
            let x_5333 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_5333), (true, snd x_5333))
      in
      let x_1789 i_2984 =
        let x_5458 = x_1788 ((true, i_2984), (false, 0)) in
        let x_7264 = x_1788 ((true, i_2984), (false, 0)) in
        snd (fst x_7264)
      in
      let x_1790 i_2977 =
        let x_5477 = x_1788 ((false, 0), (true, i_2977)) in
        let x_7256 = x_1788 ((false, 0), (true, i_2977)) in
        snd (snd x_7256)
      in
      let x_5480 = append_1059 x_1788 in
      let x_1792 i_2966 =
        let x_5504 = x_5480 ((true, i_2966), (false, 0), (false, 0)) in
        let x_7245 = x_5480 ((true, i_2966), (false, 0), (false, 0)) in
        snd (#0 x_7245)
      in
      let x_1793 i_2956 =
        let x_5530 = x_5480 ((false, 0), (true, i_2956), (false, 0)) in
        let x_7234 = x_5480 ((false, 0), (true, i_2956), (false, 0)) in
        snd (#1 x_7234)
      in
      let x_1794 i_2946 =
        let x_5556 = x_5480 ((false, 0), (false, 0), (true, i_2946)) in
        let x_7223 = x_5480 ((false, 0), (false, 0), (true, i_2946)) in
        snd (#2 x_7223)
      in
      let rec x_x_3919 x_3881 x_3882 =
        let x_5574 = x_5480 ((false, 0), (true, x_3881), (false, 0)) in
        let x_5592 = x_5480 ((false, 0), (false, 0), (true, x_3882)) in
        let x_7211 = x_5480 ((false, 0), (true, x_3881), (true, x_3882)) in
        (snd (#1 x_7211), snd (#2 x_7211))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5669 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5669))
        else
          if fst (snd ii_2929) = false then
            let x_5628 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5628), (false, (true, 0)))
          else
            let x_5604 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5604), (true, snd x_5604))
      in
      let x_1798 i_2909 =
        let x_5729 = x_1797 ((true, i_2909), (false, 0)) in
        let x_7203 = x_1797 ((true, i_2909), (false, 0)) in
        snd (fst x_7203)
      in
      let x_1799 i_2902 =
        let x_5748 = x_1797 ((false, 0), (true, i_2902)) in
        let x_7195 = x_1797 ((false, 0), (true, i_2902)) in
        snd (snd x_7195)
      in
      let x_5752 = cons_1225 (snd (snd (fst x_7289))) in
      let x_5753 = x_5752 x_1792 in
      let x_1802 i_2893 =
        let x_5770 = x_5753 ((true, i_2893), (false, 0)) in
        let x_7187 = x_5753 ((true, i_2893), (false, 0)) in
        snd (fst x_7187)
      in
      let rec x_x_4018 x_3992 x_3993 =
        let x_5784 = x_5753 ((true, x_3992), (false, 0)) in
        let x_7179 = x_5753 ((true, x_3992), (false, 0)) in
        let x_5798 = x_1023 ((true, x_3993), (false, 0)) in
        let x_7171 = x_1023 ((true, x_3993), (false, 0)) in
        (snd (fst x_7179), snd (fst x_7171))
      in
      let rec x_x_4063 x_4037 x_4038 =
        let x_5815 = x_5753 ((true, x_4037), (false, 0)) in
        let x_7163 = x_5753 ((true, x_4037), (false, 0)) in
        let x_5829 = x_1023 ((false, 0), (true, x_4038)) in
        let x_7155 = x_1023 ((false, 0), (true, x_4038)) in
        (snd (fst x_7163), snd (snd x_7155))
      in
      let rec x_x_x_3972 x_3933 x_3934 x_3935 =
        let x_5846 = x_5753 ((true, x_3933), (false, 0)) in
        let x_7147 = x_5753 ((true, x_3933), (false, 0)) in
        let x_5860 = x_1023 ((true, x_3934), (false, 0)) in
        let x_5874 = x_1023 ((false, 0), (true, x_3935)) in
        let x_7138 = x_1023 ((true, x_3934), (true, x_3935)) in
        (snd (fst x_7147), snd (fst x_7138), snd (snd x_7138))
      in
      let x_1803 i_2886 =
        let x_5897 = x_5753 ((false, 0), (true, i_2886)) in
        let x_7130 = x_5753 ((false, 0), (true, i_2886)) in
        snd (snd x_7130)
      in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6167 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6167))
          else
            if fst (#2 iii_2861) = false then
              let x_6114 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_6114), (false, (true, 0)))
            else
              let x_6067 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, fst x_6067), (true, snd x_6067))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_6019 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_6019), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5983 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, fst x_5983), (false, (true, 0)), (true, snd x_5983))
          else
            if fst (#2 iii_2861) = false then
              let x_5941 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, fst x_5941), (true, snd x_5941), (false, (true, 0)))
            else
              let x_5909 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5909), (true, #1 x_5909), (true, #2 x_5909))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5080 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5080))
          else
            if fst (#2 iii_2432) = false then
              let x_5027 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_5027), (false, (true, 0)))
            else
              let x_4980 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, fst x_4980), (true, snd x_4980))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4932 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4932), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4897 = x_1610 (snd (#0 iii_2432)) in
              let x_4918 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4897), (false, (true, 0)), (true, x_4918))
          else
            if fst (#2 iii_2432) = false then
              let x_4856 = x_1610 (snd (#0 iii_2432)) in
              let x_4866 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4856), (true, x_4866), (false, (true, 0)))
            else
              let x_4822 = x_1610 (snd (#0 iii_2432)) in
              let x_4832 = x_1746 (snd (#1 iii_2432)) in
              let x_4842 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4822), (true, x_4832), (true, x_4842))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6568 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6644 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6644))
    else
      if fst (snd ix_2198) = false then
        let x_6603 = x_6568 (snd (fst ix_2198)) in
        ((true, x_6603), (false, (true, 0)))
      else
        let x_6580 = x_6568 (snd (fst ix_2198)) in
        let x_6590 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6580), (true, x_6590))
  in
  let x_1821 i_2178 =
    let x_6704 = x_1820 ((true, i_2178), (false, 0)) in
    let x_7122 = x_1820 ((true, i_2178), (false, 0)) in
    snd (fst x_7122)
  in
  let x_1822 x_2171 =
    let x_6723 = x_1820 ((false, 0), (true, x_2171)) in
    let x_7114 = x_1820 ((false, 0), (true, x_2171)) in
    snd (snd x_7114)
  in
  let x_6726 = append_1059 x_1820 in
  let x_1824 i_2160 =
    let x_6750 = x_6726 ((true, i_2160), (false, 0), (false, 0)) in
    let x_7103 = x_6726 ((true, i_2160), (false, 0), (false, 0)) in
    snd (#0 x_7103)
  in
  let x_1825 i_2150 =
    let x_6776 = x_6726 ((false, 0), (true, i_2150), (false, 0)) in
    let x_7092 = x_6726 ((false, 0), (true, i_2150), (false, 0)) in
    snd (#1 x_7092)
  in
  let x_1826 i_2140 =
    let x_6802 = x_6726 ((false, 0), (false, 0), (true, i_2140)) in
    let x_7081 = x_6726 ((false, 0), (false, 0), (true, i_2140)) in
    snd (#2 x_7081)
  in
  let rec x_x_4361 x_4323 x_4324 =
    let x_6820 = x_6726 ((false, 0), (true, x_4323), (false, 0)) in
    let x_6838 = x_6726 ((false, 0), (false, 0), (true, x_4324)) in
    let x_7069 = x_6726 ((false, 0), (true, x_4323), (true, x_4324)) in
    (snd (#1 x_7069), snd (#2 x_7069))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6915 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6915))
    else
      if fst (snd ii_2123) = false then
        let x_6874 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6874), (false, (true, 0)))
      else
        let x_6850 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6850), (true, snd x_6850))
  in
  let x_1830 i_2103 =
    let x_6975 = x_1829 ((true, i_2103), (false, 0)) in
    let x_7061 = x_1829 ((true, i_2103), (false, 0)) in
    snd (fst x_7061)
  in
  let x_1831 i_2096 =
    let x_6994 = x_1829 ((false, 0), (true, i_2096)) in
    let x_7053 = x_1829 ((false, 0), (true, i_2096)) in
    snd (snd x_7053)
  in
  let x_7018 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7042 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7019 = x_6568 i_1016 in
  let n_1504 = if fst (snd (#0 x_7042)) <> false then
                 snd (snd (#0 x_7042))
               else
                 _|_ in
  let n_1505 = if fst x_7019 <> false then
                 snd x_7019
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_7037 = rand_int () in
let x_7039 = rand_int () in
let x_7040 = main_1015 x_7037 in
let x_7041 = x_7040 x_7039 in
let x_1847 = x_7041 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_7037; is_subsumed: 
rand_int (), x_7040 x_7039; is_subsumed: main_1015 x_7037, x_7041; is_subsumed: 
rand_int (), x_7041; is_subsumed: rand_int (), x_7041; is_subsumed: make_list_1008 n_1017, 
append_1059 x_1820; is_subsumed: make_list_1008 n_1017, x_6726 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_6726 ((true, i_1016), (false, 0), (false, 0)), x_6726 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6726 ((true, i_1016), (false, 0), (false, 0)); x_7018 |-> x_7042
is_subsumed: x_6726 ((true, i_1016), (false, 0), (false, 0)), x_6568 i_1016; is_subsumed: 
x_6726 ((true, i_1016), (false, 0), (false, 0)), x_6568 i_1016; is_subsumed: 
append_1059 x_1820, x_6568 i_1016; is_subsumed: x_6568 i_1016, if fst (snd (#0 x_7042)) <> false then
                                                                 snd (snd (#0 x_7042))
                                                               else
                                                                 _|_; is_subsumed: 
x_6726 ((true, i_1016), (false, 0), (false, 0)), if fst (snd (#0 x_7042)) <> false then
                                                   snd (snd (#0 x_7042))
                                                 else
                                                   _|_; is_subsumed: 
append_1059 x_1820, if fst (snd (#0 x_7042)) <> false then
                      snd (snd (#0 x_7042))
                    else
                      _|_; is_subsumed: make_list_1008 n_1017, if fst (snd (#0 x_7042)) <> false then
                                                                 snd (snd (#0 x_7042))
                                                               else
                                                                 _|_; is_subsumed: 
if fst (snd (#0 x_7042)) <> false then
  snd (snd (#0 x_7042))
else
  _|_, if fst x_7019 <> false then
         snd x_7019
       else
         _|_; is_subsumed: x_6726 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_7019 <> false then
  snd x_7019
else
  _|_; is_subsumed: x_6726 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_7019 <> false then
  snd x_7019
else
  _|_; is_subsumed: append_1059 x_1820, if fst x_7019 <> false then
                                          snd x_7019
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_7019 <> false then
  snd x_7019
else
  _|_; is_subsumed: append_1059 x_1820, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
x_1829 ((false, 0), (true, i_2096)), x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
append_1059 x_1820, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((false, 0), (true, i_2096)); x_6994 |-> x_7053
is_subsumed: append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
x_1829 ((true, i_2103), (false, 0)), x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); x_6975 |-> x_7061
is_subsumed: append_1059 x_1820, x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
make_list_1008 n_1017, x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
append_1059 x_1820, x_1825 (snd (fst ii_2123)); is_subsumed: make_list_1008 n_1017, 
x_1825 (snd (fst ii_2123)); is_subsumed: append_1059 x_1820, x_1826 (snd (snd ii_2123)); is_subsumed: 
make_list_1008 n_1017, x_1826 (snd (snd ii_2123)); is_subsumed: make_list_1008 n_1017, 
x_6726 ((false, 0), (true, x_4323), (false, 0)); is_subsumed: x_6726 ((false, 0), (true, x_4323), (false, 0)), 
x_6726 ((false, 0), (false, 0), (true, x_4324)); is_subsumed: make_list_1008 n_1017, 
x_6726 ((false, 0), (false, 0), (true, x_4324)); is_subsumed: x_6726 ((false, 0), (false, 0), (true, x_4324)), 
x_6726 ((false, 0), (true, x_4323), (true, x_4324)); is_subsumed: x_6726 ((false, 0), (true, x_4323), (false, 0)), 
x_6726 ((false, 0), (true, x_4323), (true, x_4324)); is_subsumed: make_list_1008 n_1017, 
x_6726 ((false, 0), (true, x_4323), (true, x_4324)); x_6838 |-> x_7069
x_6820 |-> x_7069
is_subsumed: make_list_1008 n_1017, x_6726 ((false, 0), (false, 0), (true, i_2140)); is_subsumed: 
x_6726 ((false, 0), (false, 0), (true, i_2140)), x_6726 ((false, 0), (false, 0), (true, i_2140)); is_subsumed: 
make_list_1008 n_1017, x_6726 ((false, 0), (false, 0), (true, i_2140)); x_6802 |-> x_7081
is_subsumed: make_list_1008 n_1017, x_6726 ((false, 0), (true, i_2150), (false, 0)); is_subsumed: 
x_6726 ((false, 0), (true, i_2150), (false, 0)), x_6726 ((false, 0), (true, i_2150), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6726 ((false, 0), (true, i_2150), (false, 0)); x_6776 |-> x_7092
is_subsumed: make_list_1008 n_1017, x_6726 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: 
x_6726 ((true, i_2160), (false, 0), (false, 0)), x_6726 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6726 ((true, i_2160), (false, 0), (false, 0)); x_6750 |-> x_7103
is_subsumed: make_list_1008 n_1017, x_1820 ((false, 0), (true, x_2171)); is_subsumed: 
x_1820 ((false, 0), (true, x_2171)), x_1820 ((false, 0), (true, x_2171)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((false, 0), (true, x_2171)); x_6723 |-> x_7114
is_subsumed: make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
x_1820 ((true, i_2178), (false, 0)), x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); x_6704 |-> x_7122
is_subsumed: x_6568 (snd (fst ix_2198)), f_1479 (snd (snd ix_2198)); is_subsumed: 
make_list_1008 n_1017, f_1479 (snd (snd ix_2198)); is_subsumed: make_list_1008 n_1017, 
f_1479 (snd (snd ix_2198)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_4809 |-> x_7289
is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: 
x_1610 (snd (#0 iii_2432)), x_1746 (snd (#1 iii_2432)); is_subsumed: _|_, 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1746 (snd (#1 iii_2432)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: x_1610 (snd (#0 iii_2432)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: _|_, x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: 
x_1610 (snd (#0 iii_2432)), x_1746 (snd (#1 iii_2432)); is_subsumed: _|_, 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1610 (snd (#0 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1610 (snd (#0 iii_2432)); is_subsumed: x_1610 (snd (#0 iii_2432)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: _|_, x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1610 (snd (#0 iii_2432)); is_subsumed: _|_, 
x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)); is_subsumed: _|_, x_1746 (snd (#1 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2432)); is_subsumed: _|_, 
x_1747 (snd (#2 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
append_1059 x_1788; is_subsumed: x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: 
append_1059 x_1788, cons_1225 (snd (snd (fst x_7289))); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd (snd (fst x_7289))); is_subsumed: 
append_1059 x_1788, x_5752 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_5752 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), x_5752 x_1792; is_subsumed: 
x_5752 x_1792, x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_5752 x_1792, x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)); is_subsumed: 
x_5752 x_1792, x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_5752 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_5752 x_1792, x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_5752 x_1792, x_1746 (snd (#1 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1746 (snd (#1 iii_2861)); is_subsumed: append_1059 x_1788, x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5752 x_1792, x_1747 (snd (#2 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1747 (snd (#2 iii_2861)); is_subsumed: append_1059 x_1788, x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
append_1059 x_1788, x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
x_5753 ((false, 0), (true, i_2886)), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
append_1059 x_1788, x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((false, 0), (true, i_2886)); x_5897 |-> x_7130
is_subsumed: cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
x_5753 ((true, x_3933), (false, 0)), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3933), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3933), (false, 0)); x_5846 |-> x_7147
is_subsumed: x_5753 ((true, x_3933), (false, 0)), x_1023 ((true, x_3934), (false, 0)); is_subsumed: 
x_5753 ((true, x_3933), (false, 0)), x_1023 ((true, x_3934), (false, 0)); is_subsumed: 
x_5752 x_1792, x_1023 ((true, x_3934), (false, 0)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1023 ((true, x_3934), (false, 0)); is_subsumed: append_1059 x_1788, 
x_1023 ((true, x_3934), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3934), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3934), (false, 0)); is_subsumed: x_1023 ((true, x_3934), (false, 0)), 
x_1023 ((false, 0), (true, x_3935)); is_subsumed: x_5753 ((true, x_3933), (false, 0)), 
x_1023 ((false, 0), (true, x_3935)); is_subsumed: x_5753 ((true, x_3933), (false, 0)), 
x_1023 ((false, 0), (true, x_3935)); is_subsumed: x_5752 x_1792, x_1023 ((false, 0), (true, x_3935)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_1023 ((false, 0), (true, x_3935)); is_subsumed: 
append_1059 x_1788, x_1023 ((false, 0), (true, x_3935)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3935)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3935)); is_subsumed: 
x_1023 ((false, 0), (true, x_3935)), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_1023 ((true, x_3934), (false, 0)), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_5753 ((true, x_3933), (false, 0)), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_5753 ((true, x_3933), (false, 0)), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_5752 x_1792, x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
append_1059 x_1788, x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3934), (true, x_3935)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3934), (true, x_3935)); x_5874 |-> x_7138
x_5860 |-> x_7138
is_subsumed: cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
x_5753 ((true, x_4037), (false, 0)), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_4037), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_4037), (false, 0)); x_5815 |-> x_7163
is_subsumed: x_5753 ((true, x_4037), (false, 0)), x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
x_5753 ((true, x_4037), (false, 0)), x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
x_5752 x_1792, x_1023 ((false, 0), (true, x_4038)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: append_1059 x_1788, 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_1023 ((false, 0), (true, x_4038)), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_5753 ((true, x_4037), (false, 0)), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_5753 ((true, x_4037), (false, 0)), 
x_1023 ((false, 0), (true, x_4038)); is_subsumed: x_5752 x_1792, x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
append_1059 x_1788, x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4038)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4038)); x_5829 |-> x_7155
is_subsumed: cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
x_5753 ((true, x_3992), (false, 0)), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3992), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, x_3992), (false, 0)); x_5784 |-> x_7179
is_subsumed: x_5753 ((true, x_3992), (false, 0)), x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
x_5753 ((true, x_3992), (false, 0)), x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
x_5752 x_1792, x_1023 ((true, x_3993), (false, 0)); is_subsumed: cons_1225 (snd (snd (fst x_7289))), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: append_1059 x_1788, 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_1023 ((true, x_3993), (false, 0)), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_5753 ((true, x_3992), (false, 0)), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_5753 ((true, x_3992), (false, 0)), 
x_1023 ((true, x_3993), (false, 0)); is_subsumed: x_5752 x_1792, x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
append_1059 x_1788, x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3993), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3993), (false, 0)); x_5798 |-> x_7171
is_subsumed: cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
x_5753 ((true, i_2893), (false, 0)), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_7289))), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5753 ((true, i_2893), (false, 0)); x_5770 |-> x_7187
is_subsumed: append_1059 x_1788, x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1797 ((false, 0), (true, i_2902)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
append_1059 x_1788, x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); x_5748 |-> x_7195
is_subsumed: append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1797 ((true, i_2909), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); x_5729 |-> x_7203
is_subsumed: append_1059 x_1788, x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
append_1059 x_1788, x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: append_1059 x_1788, x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, x_3881), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, x_3881), (false, 0)); is_subsumed: 
x_5480 ((false, 0), (true, x_3881), (false, 0)), x_5480 ((false, 0), (false, 0), (true, x_3882)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, x_3882)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, x_3882)); is_subsumed: 
x_5480 ((false, 0), (false, 0), (true, x_3882)), x_5480 ((false, 0), (true, x_3881), (true, x_3882)); is_subsumed: 
x_5480 ((false, 0), (true, x_3881), (false, 0)), x_5480 ((false, 0), (true, x_3881), (true, x_3882)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, x_3881), (true, x_3882)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, x_3881), (true, x_3882)); x_5592 |-> x_7211
x_5574 |-> x_7211
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_5480 ((false, 0), (false, 0), (true, i_2946)), x_5480 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (false, 0), (true, i_2946)); x_5556 |-> x_7223
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_5480 ((false, 0), (true, i_2956), (false, 0)), x_5480 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((false, 0), (true, i_2956), (false, 0)); x_5530 |-> x_7234
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5480 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_5480 ((true, i_2966), (false, 0), (false, 0)), x_5480 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5480 ((true, i_2966), (false, 0), (false, 0)); x_5504 |-> x_7245
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1788 ((false, 0), (true, i_2977)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); x_5477 |-> x_7256
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1788 ((true, i_2984), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); x_5458 |-> x_7264
is_subsumed: x_1023 ((true, 0), (false, 0)), xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1625 (snd (fst ii_3086)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: x_1023 ((true, 0), (false, 0)), 
xs_1222 (i_1220 - 1); is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3841 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3841 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_3841 + 1), (false, 0)), x_1023 ((false, 0), (true, x_3842)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3842)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3842)); is_subsumed: 
x_1023 ((false, 0), (true, x_3842)), x_1023 ((true, x_3841 + 1), (true, x_3842)); is_subsumed: 
x_1023 ((true, x_3841 + 1), (false, 0)), x_1023 ((true, x_3841 + 1), (true, x_3842)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3841 + 1), (true, x_3842)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3841 + 1), (true, x_3842)); x_5190 |-> x_7272
x_5175 |-> x_7272
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_1150 + 1), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); x_5160 |-> x_7281
is_subsumed: x_1023 ((true, 0), (false, 0)), x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, x_4276), (false, 0)), x_1023 ((false, 0), (true, x_4277)); is_subsumed: 
x_1023 ((false, 0), (true, x_4277)), x_1023 ((true, x_4276), (true, x_4277)); is_subsumed: 
x_1023 ((true, x_4276), (false, 0)), x_1023 ((true, x_4276), (true, x_4277)); x_4788 |-> x_7297
x_4774 |-> x_7297
is_subsumed: x_1023 ((false, 0), (true, x_4186)), x_1023 ((true, x_4187), (false, 0)); is_subsumed: 
x_1023 ((true, x_4187), (false, 0)), x_1023 ((true, x_4187), (true, x_4186)); is_subsumed: 
x_1023 ((false, 0), (true, x_4186)), x_1023 ((true, x_4187), (true, x_4186)); x_4757 |-> x_7306
x_4743 |-> x_7306
is_subsumed: x_1023 ((false, 0), (true, x_4127)), x_1023 ((true, x_4128), (false, 0)); is_subsumed: 
x_1023 ((true, x_4128), (false, 0)), x_1023 ((false, 0), (true, x_4129)); is_subsumed: 
x_1023 ((false, 0), (true, x_4127)), x_1023 ((false, 0), (true, x_4129)); is_subsumed: 
x_1023 ((false, 0), (true, x_4129)), x_1023 ((true, x_4128), (true, x_4129)); is_subsumed: 
x_1023 ((true, x_4128), (false, 0)), x_1023 ((true, x_4128), (true, x_4129)); is_subsumed: 
x_1023 ((false, 0), (true, x_4127)), x_1023 ((true, x_4128), (true, x_4129)); x_4725 |-> x_7316
x_4711 |-> x_7316
is_subsumed: x_1023 ((true, x_4082), (false, 0)), x_1023 ((false, 0), (true, x_4083)); is_subsumed: 
x_1023 ((false, 0), (true, x_4083)), x_1023 ((true, x_4082), (true, x_4083)); is_subsumed: 
x_1023 ((true, x_4082), (false, 0)), x_1023 ((true, x_4082), (true, x_4083)); x_4680 |-> x_7325
x_4666 |-> x_7325
is_subsumed: x_1023 ((true, x_3794), (false, 0)), x_1023 ((false, 0), (true, x_3795)); is_subsumed: 
x_1023 ((false, 0), (true, x_3795)), x_1023 ((true, x_3794), (true, x_3795)); is_subsumed: 
x_1023 ((true, x_3794), (false, 0)), x_1023 ((true, x_3794), (true, x_3795)); x_4649 |-> x_7334
x_4635 |-> x_7334
is_subsumed: x_1023 ((false, 0), (true, x_4231)), x_1023 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4232)), x_1023 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4231)), x_1023 ((false, 0), (true, x_4232)); x_4618 |-> x_7344
is_subsumed: x_1023 ((false, 0), (true, i_3303)), x_1023 ((false, 0), (true, i_3303)); x_4590 |-> x_7352
is_subsumed: x_1023 ((true, i_3310), (false, 0)), x_1023 ((true, i_3310), (false, 0)); x_4571 |-> x_7360
is_subsumed: make_list_1008 (n_1009 - 1), rand_int (); is_subsumed: make_list_1008 (n_1009 - 1), 
cons_1117 x_4507; is_subsumed: rand_int (), x_4508 x_4505; is_subsumed: 
cons_1117 x_4507, x_4509 ((false, 0), (true, i_3440)); is_subsumed: rand_int (), 
x_4509 ((false, 0), (true, i_3440)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4509 ((false, 0), (true, i_3440)); is_subsumed: x_4509 ((false, 0), (true, i_3440)), 
x_4509 ((false, 0), (true, i_3440)); is_subsumed: cons_1117 x_4507, x_4509 ((false, 0), (true, i_3440)); is_subsumed: 
rand_int (), x_4509 ((false, 0), (true, i_3440)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4509 ((false, 0), (true, i_3440)); x_4545 |-> x_7368
is_subsumed: cons_1117 x_4507, x_4509 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4509 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4509 ((true, i_3447), (false, 0)); is_subsumed: x_4509 ((true, i_3447), (false, 0)), 
x_4509 ((true, i_3447), (false, 0)); is_subsumed: cons_1117 x_4507, x_4509 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4509 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4509 ((true, i_3447), (false, 0)); x_4526 |-> x_7376
is_subsumed: x_1592 (snd (fst ii_3523)), xs_1114 (snd (snd ii_3523)); 
x_4545; x_4526; x_4571; x_4590; x_4618; x_4635; x_4649; x_4666; x_4680; x_4725; x_4711; x_4743; x_4757; x_4774; 
x_4788; x_5160; x_5175; x_5190; x_5458; x_5477; x_5504; x_5530; x_5556; x_5574; x_5592; x_5729; x_5748; x_5770; 
x_5784; x_5798; x_5815; x_5829; x_5860; x_5874; x_5846; x_5897; x_4809; x_7018; x_6994; x_6975; x_6838; x_6820; 
x_6802; x_6776; x_6750; x_6723; x_6704
elim_unnecessary:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_4377 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4377
                               else
                                 xs_1114 n_1526 in
          x_4377
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4459 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4459))
        else
          if fst (snd ii_3523) = false then
            let x_4418 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4418), (false, (true, 0)))
          else
            let x_4395 = x_1592 (snd (fst ii_3523)) in
            let x_4405 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4395), (true, x_4405))
      in
      x_1732
    in
    let x_4505 = make_list_1008 (n_1009 - 1) in
    let x_4507 = rand_int () in
    let x_4508 = cons_1117 x_4507 in
    let x_4509 = x_4508 x_4505 in
    let x_1739 i_3447 = let x_7376 = x_4509 ((true, i_3447), (false, 0)) in
                        snd (fst x_7376) in
    let x_1740 i_3440 = let x_7368 = x_4509 ((false, 0), (true, i_3440)) in
                        snd (snd x_7368) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_7360 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_7360) in
  let x_1747 i_3303 = let x_7352 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_7352) in
  let rec x_x_4257 x_4231 x_4232 =
    let x_4604 = x_1023 ((false, 0), (true, x_4231)) in
    let x_7344 = x_1023 ((false, 0), (true, x_4232)) in
    (snd (snd x_4604), snd (snd x_7344))
  in
  let rec x_x_3820 x_3794 x_3795 =
    let x_7334 = x_1023 ((true, x_3794), (true, x_3795)) in
    (snd (fst x_7334), snd (snd x_7334))
  in
  let rec x_x_4108 x_4082 x_4083 =
    let x_7325 = x_1023 ((true, x_4082), (true, x_4083)) in
    (snd (fst x_7325), snd (snd x_7325))
  in
  let rec x_x_x_4166 x_4127 x_4128 x_4129 =
    let x_4697 = x_1023 ((false, 0), (true, x_4127)) in
    let x_7316 = x_1023 ((true, x_4128), (true, x_4129)) in
    (snd (snd x_4697), snd (fst x_7316), snd (snd x_7316))
  in
  let rec x_x_4212 x_4186 x_4187 =
    let x_7306 = x_1023 ((true, x_4187), (true, x_4186)) in
    (snd (snd x_7306), snd (fst x_7306))
  in
  let rec x_x_4302 x_4276 x_4277 =
    let x_7297 = x_1023 ((true, x_4276), (true, x_4277)) in
    (snd (fst x_7297), snd (snd x_7297))
  in
  let x_7289 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7289)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6501 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6501))
        else
          if fst (#2 iii_3257) = false then
            let x_6448 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_6448), (false, (true, 0)))
          else
            let x_6401 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, fst x_6401), (true, snd x_6401))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_6353 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_6353), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6317 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, fst x_6317), (false, (true, 0)), (true, snd x_6317))
        else
          if fst (#2 iii_3257) = false then
            let x_6275 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, fst x_6275), (true, snd x_6275), (false, (true, 0)))
          else
            let x_6243 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_6243), (true, #1 x_6243), (true, #2 x_6243))
    in
    x_1811
  else
    if fst (snd (fst x_7289)) <> false then
      let xs'_1014 x_1150 = let x_7281 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_7281) in
      let rec xs'_x_3867 x_3841 x_3842 =
        let x_7272 = x_1023 ((true, x_3841 + 1), (true, x_3842)) in
        (snd (fst x_7272), snd (snd x_7272))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_5201 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_5201
                                 else
                                   xs_1222 n_1544 in
            x_5201
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_5283 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_5283))
          else
            if fst (snd ii_3086) = false then
              let x_5242 = x_1625 (snd (fst ii_3086)) in
              ((true, x_5242), (false, (true, 0)))
            else
              let x_5219 = x_1625 (snd (fst ii_3086)) in
              let x_5229 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_5219), (true, x_5229))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5398 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_5398))
        else
          if fst (snd ii_3004) = false then
            let x_5357 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_5357), (false, (true, 0)))
          else
            let x_5333 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_5333), (true, snd x_5333))
      in
      let x_1789 i_2984 = let x_7264 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_7264) in
      let x_1790 i_2977 = let x_7256 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_7256) in
      let x_5480 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_7245 = x_5480 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_7245) in
      let x_1793 i_2956 = let x_7234 = x_5480 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_7234) in
      let x_1794 i_2946 = let x_7223 = x_5480 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_7223) in
      let rec x_x_3919 x_3881 x_3882 =
        let x_7211 = x_5480 ((false, 0), (true, x_3881), (true, x_3882)) in
        (snd (#1 x_7211), snd (#2 x_7211))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5669 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5669))
        else
          if fst (snd ii_2929) = false then
            let x_5628 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5628), (false, (true, 0)))
          else
            let x_5604 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5604), (true, snd x_5604))
      in
      let x_1798 i_2909 = let x_7203 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_7203) in
      let x_1799 i_2902 = let x_7195 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_7195) in
      let x_5752 = cons_1225 (snd (snd (fst x_7289))) in
      let x_5753 = x_5752 x_1792 in
      let x_1802 i_2893 = let x_7187 = x_5753 ((true, i_2893), (false, 0)) in
                          snd (fst x_7187) in
      let rec x_x_4018 x_3992 x_3993 =
        let x_7179 = x_5753 ((true, x_3992), (false, 0)) in
        let x_7171 = x_1023 ((true, x_3993), (false, 0)) in
        (snd (fst x_7179), snd (fst x_7171))
      in
      let rec x_x_4063 x_4037 x_4038 =
        let x_7163 = x_5753 ((true, x_4037), (false, 0)) in
        let x_7155 = x_1023 ((false, 0), (true, x_4038)) in
        (snd (fst x_7163), snd (snd x_7155))
      in
      let rec x_x_x_3972 x_3933 x_3934 x_3935 =
        let x_7147 = x_5753 ((true, x_3933), (false, 0)) in
        let x_7138 = x_1023 ((true, x_3934), (true, x_3935)) in
        (snd (fst x_7147), snd (fst x_7138), snd (snd x_7138))
      in
      let x_1803 i_2886 = let x_7130 = x_5753 ((false, 0), (true, i_2886)) in
                          snd (snd x_7130) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6167 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6167))
          else
            if fst (#2 iii_2861) = false then
              let x_6114 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_6114), (false, (true, 0)))
            else
              let x_6067 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, fst x_6067), (true, snd x_6067))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_6019 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_6019), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5983 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, fst x_5983), (false, (true, 0)), (true, snd x_5983))
          else
            if fst (#2 iii_2861) = false then
              let x_5941 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, fst x_5941), (true, snd x_5941), (false, (true, 0)))
            else
              let x_5909 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5909), (true, #1 x_5909), (true, #2 x_5909))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5080 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5080))
          else
            if fst (#2 iii_2432) = false then
              let x_5027 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_5027), (false, (true, 0)))
            else
              let x_4980 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, fst x_4980), (true, snd x_4980))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4932 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4932), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4897 = x_1610 (snd (#0 iii_2432)) in
              let x_4918 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4897), (false, (true, 0)), (true, x_4918))
          else
            if fst (#2 iii_2432) = false then
              let x_4856 = x_1610 (snd (#0 iii_2432)) in
              let x_4866 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4856), (true, x_4866), (false, (true, 0)))
            else
              let x_4822 = x_1610 (snd (#0 iii_2432)) in
              let x_4832 = x_1746 (snd (#1 iii_2432)) in
              let x_4842 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4822), (true, x_4832), (true, x_4842))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6568 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6644 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6644))
    else
      if fst (snd ix_2198) = false then
        let x_6603 = x_6568 (snd (fst ix_2198)) in
        ((true, x_6603), (false, (true, 0)))
      else
        let x_6580 = x_6568 (snd (fst ix_2198)) in
        let x_6590 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6580), (true, x_6590))
  in
  let x_1821 i_2178 = let x_7122 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_7122) in
  let x_1822 x_2171 = let x_7114 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_7114) in
  let x_6726 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_7103 = x_6726 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_7103) in
  let x_1825 i_2150 = let x_7092 = x_6726 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_7092) in
  let x_1826 i_2140 = let x_7081 = x_6726 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_7081) in
  let rec x_x_4361 x_4323 x_4324 =
    let x_7069 = x_6726 ((false, 0), (true, x_4323), (true, x_4324)) in
    (snd (#1 x_7069), snd (#2 x_7069))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6915 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6915))
    else
      if fst (snd ii_2123) = false then
        let x_6874 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6874), (false, (true, 0)))
      else
        let x_6850 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6850), (true, snd x_6850))
  in
  let x_1830 i_2103 = let x_7061 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_7061) in
  let x_1831 i_2096 = let x_7053 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_7053) in
  let x_7042 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7019 = x_6568 i_1016 in
  let n_1504 = if fst (snd (#0 x_7042)) <> false then
                 snd (snd (#0 x_7042))
               else
                 _|_ in
  let n_1505 = if fst x_7019 <> false then
                 snd x_7019
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_7037 = rand_int () in
let x_7039 = rand_int () in
let x_7040 = main_1015 x_7037 in
let x_7041 = x_7040 x_7039 in
let x_1847 = x_7041 in
()

inline_next_redex:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1123 -> (false, 0)
  else
    let cons_1117 x_1113 xs_1114 =
      let x_1592 i_1112 =
        if i_1112 = 0 then
          (true, x_1113)
        else
          let x_4377 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4377
                               else
                                 xs_1114 n_1526 in
          x_4377
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            let x_4395 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4395), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_4505 = make_list_1008 (n_1009 - 1) in
    let x_4507 = rand_int () in
    let x_4508 = cons_1117 x_4507 in
    let x_4509 = x_4508 x_4505 in
    let x_1739 i_3447 = snd (fst (x_4509 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_4509 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let rec x_x_4257 x_4231 x_4232 =
    let x_4604 = x_1023 ((false, 0), (true, x_4231)) in
    (snd (snd x_4604), snd (snd (x_1023 ((false, 0), (true, x_4232)))))
  in
  let rec x_x_3820 x_3794 x_3795 =
    let x_7334 = x_1023 ((true, x_3794), (true, x_3795)) in
    (snd (fst x_7334), snd (snd x_7334))
  in
  let rec x_x_4108 x_4082 x_4083 =
    let x_7325 = x_1023 ((true, x_4082), (true, x_4083)) in
    (snd (fst x_7325), snd (snd x_7325))
  in
  let rec x_x_x_4166 x_4127 x_4128 x_4129 =
    let x_4697 = x_1023 ((false, 0), (true, x_4127)) in
    let x_7316 = x_1023 ((true, x_4128), (true, x_4129)) in
    (snd (snd x_4697), snd (fst x_7316), snd (snd x_7316))
  in
  let rec x_x_4212 x_4186 x_4187 =
    let x_7306 = x_1023 ((true, x_4187), (true, x_4186)) in
    (snd (snd x_7306), snd (fst x_7306))
  in
  let rec x_x_4302 x_4276 x_4277 =
    let x_7297 = x_1023 ((true, x_4276), (true, x_4277)) in
    (snd (fst x_7297), snd (snd x_7297))
  in
  let x_7289 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7289)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            let x_6401 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, fst x_6401), (true, snd x_6401))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6317 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, fst x_6317), (false, (true, 0)), (true, snd x_6317))
        else
          if fst (#2 iii_3257) = false then
            let x_6275 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, fst x_6275), (true, snd x_6275), (false, (true, 0)))
          else
            let x_6243 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_6243), (true, #1 x_6243), (true, #2 x_6243))
    in
    x_1811
  else
    if fst (snd (fst x_7289)) <> false then
      let xs'_1014 x_1150 = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
      let rec xs'_x_3867 x_3841 x_3842 =
        let x_7272 = x_1023 ((true, x_3841 + 1), (true, x_3842)) in
        (snd (fst x_7272), snd (snd x_7272))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_5201 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_5201
                                 else
                                   xs_1222 n_1544 in
            x_5201
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              let x_5219 = x_1625 (snd (fst ii_3086)) in
              ((true, x_5219), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            let x_5333 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_5333), (true, snd x_5333))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_5480 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_5480 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_5480 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_5480 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3919 x_3881 x_3882 =
        let x_7211 = x_5480 ((false, 0), (true, x_3881), (true, x_3882)) in
        (snd (#1 x_7211), snd (#2 x_7211))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            let x_5604 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5604), (true, snd x_5604))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_5752 = cons_1225 (snd (snd (fst x_7289))) in
      let x_5753 = x_5752 x_1792 in
      let x_1802 i_2893 = snd (fst (x_5753 ((true, i_2893), (false, 0)))) in
      let rec x_x_4018 x_3992 x_3993 =
        let x_7179 = x_5753 ((true, x_3992), (false, 0)) in
        (snd (fst x_7179), snd (fst (x_1023 ((true, x_3993), (false, 0)))))
      in
      let rec x_x_4063 x_4037 x_4038 =
        let x_7163 = x_5753 ((true, x_4037), (false, 0)) in
        (snd (fst x_7163), snd (snd (x_1023 ((false, 0), (true, x_4038)))))
      in
      let rec x_x_x_3972 x_3933 x_3934 x_3935 =
        let x_7147 = x_5753 ((true, x_3933), (false, 0)) in
        let x_7138 = x_1023 ((true, x_3934), (true, x_3935)) in
        (snd (fst x_7147), snd (fst x_7138), snd (snd x_7138))
      in
      let x_1803 i_2886 = snd (snd (x_5753 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              let x_6067 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, fst x_6067), (true, snd x_6067))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5983 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, fst x_5983), (false, (true, 0)), (true, snd x_5983))
          else
            if fst (#2 iii_2861) = false then
              let x_5941 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, fst x_5941), (true, snd x_5941), (false, (true, 0)))
            else
              let x_5909 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5909), (true, #1 x_5909), (true, #2 x_5909))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              let x_4980 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, fst x_4980), (true, snd x_4980))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4897 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4897), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              let x_4856 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4856), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              let x_4822 = x_1610 (snd (#0 iii_2432)) in
              let x_4832 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4822), (true, x_4832), (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6568 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_6568 (snd (fst ix_2198))), (false, (true, 0)))
      else
        let x_6580 = x_6568 (snd (fst ix_2198)) in
        ((true, x_6580), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_6726 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_6726 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_6726 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_6726 ((false, 0), (false, 0), (true, i_2140)))) in
  let rec x_x_4361 x_4323 x_4324 =
    let x_7069 = x_6726 ((false, 0), (true, x_4323), (true, x_4324)) in
    (snd (#1 x_7069), snd (#2 x_7069))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        let x_6850 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6850), (true, snd x_6850))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_7042 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7019 = x_6568 i_1016 in
  let n_1504 = if fst (snd (#0 x_7042)) <> false then
                 snd (snd (#0 x_7042))
               else
                 _|_ in
  let n_1505 = if fst x_7019 <> false then
                 snd x_7019
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_7037 = rand_int () in
let x_7039 = rand_int () in
let x_7040 = main_1015 x_7037 in
let x_7041 = x_7040 x_7039 in
let x_1847 = x_7041 in
()

tupling:
 let rec make_list_1008 (n_1009:int) =
   if n_1009 < 0 then
     fun (x_1123:int) -> (false, 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool * int))) =
       let x_1592 (i_1112:int) =
         if i_1112 = 0 then
           (true, x_1113)
         else
           let x_4377 = xs_1114 (i_1112 - 1) in
           let xs_1525 (n_1526:int) = if n_1526 = i_1112 - 1 then
                                        x_4377
                                      else
                                        xs_1114 n_1526 in
           x_4377
       in
       let x_1732 (ii_3523:((bool * int) * (bool * int))) =
         if fst (fst ii_3523) = false then
           if fst (snd ii_3523) = false then
             ((false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (true, xs_1114 (snd (snd ii_3523))))
         else
           if fst (snd ii_3523) = false then
             ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
           else
             let x_4395 = x_1592 (snd (fst ii_3523)) in
             ((true, x_4395), (true, xs_1114 (snd (snd ii_3523))))
       in
       x_1732
     in
     let x_4505 = make_list_1008 (n_1009 - 1) in
     let x_4507 = rand_int () in
     let x_4508 = cons_1117 x_4507 in
     let x_4509 = x_4508 x_4505 in
     let x_1739 (i_3447:int) = snd (fst (x_4509 ((true, i_3447), (false, 0)))) in
     let x_1740 (i_3440:int) = snd (snd (x_4509 ((false, 0), (true, i_3440)))) in
     x_1739
 in
 let rec append_1059 (x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1746 (i_3310:int) = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
   let x_1747 (i_3303:int) = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
   let rec x_x_4257 (x_4231:int) (x_4232:int) =
     let x_4604 = x_1023 ((false, 0), (true, x_4231)) in
     (snd (snd x_4604), snd (snd (x_1023 ((false, 0), (true, x_4232)))))
   in
   let rec x_x_3820 (x_3794:int) (x_3795:int) =
     let x_7334 = x_1023 ((true, x_3794), (true, x_3795)) in
     (snd (fst x_7334), snd (snd x_7334))
   in
   let rec x_x_4108 (x_4082:int) (x_4083:int) =
     let x_7325 = x_1023 ((true, x_4082), (true, x_4083)) in
     (snd (fst x_7325), snd (snd x_7325))
   in
   let rec x_x_x_4166 (x_4127:int) (x_4128:int) (x_4129:int) =
     let x_4697 = x_1023 ((false, 0), (true, x_4127)) in
     let x_7316 = x_1023 ((true, x_4128), (true, x_4129)) in
     (snd (snd x_4697), snd (fst x_7316), snd (snd x_7316))
   in
   let rec x_x_4212 (x_4186:int) (x_4187:int) =
     let x_7306 = x_1023 ((true, x_4187), (true, x_4186)) in
     (snd (snd x_7306), snd (fst x_7306))
   in
   let rec x_x_4302 (x_4276:int) (x_4277:int) =
     let x_7297 = x_1023 ((true, x_4276), (true, x_4277)) in
     (snd (fst x_7297), snd (snd x_7297))
   in
   let x_7289 = x_1023 ((true, 0), (false, 0)) in
   if fst (snd (fst x_7289)) = false then
     let x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) =
       if fst (#0 iii_3257) = false then
         if fst (#1 iii_3257) = false then
           if fst (#2 iii_3257) = false then
             ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
         else
           if fst (#2 iii_3257) = false then
             ((false, (true, 0)), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
           else
             let x_6401 = x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
             ((false, (true, 0)), (true, fst x_6401), (true, snd x_6401))
       else
         if fst (#1 iii_3257) = false then
           if fst (#2 iii_3257) = false then
             ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
           else
             let x_6317 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
             ((true, fst x_6317), (false, (true, 0)), (true, snd x_6317))
         else
           if fst (#2 iii_3257) = false then
             let x_6275 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
             ((true, fst x_6275), (true, snd x_6275), (false, (true, 0)))
           else
             let x_6243 = x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
             ((true, #0 x_6243), (true, #1 x_6243), (true, #2 x_6243))
     in
     x_1811
   else
     if fst (snd (fst x_7289)) <> false then
       let xs'_1014 (x_1150:int) = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
       let rec xs'_x_3867 (x_3841:int) (x_3842:int) =
         let x_7272 = x_1023 ((true, x_3841 + 1), (true, x_3842)) in
         (snd (fst x_7272), snd (snd x_7272))
       in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
         let x_1625 (i_1220:int) =
           if i_1220 = 0 then
             (true, x_1221)
           else
             let x_5201 = xs_1222 (i_1220 - 1) in
             let xs_1543 (n_1544:int) = if n_1544 = i_1220 - 1 then
                                          x_5201
                                        else
                                          xs_1222 n_1544 in
             x_5201
         in
         let x_1785 (ii_3086:((bool * int) * (bool * int))) =
           if fst (fst ii_3086) = false then
             if fst (snd ii_3086) = false then
               ((false, (true, 0)), (false, (true, 0)))
             else
               ((false, (true, 0)), (true, xs_1222 (snd (snd ii_3086))))
           else
             if fst (snd ii_3086) = false then
               ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
             else
               let x_5219 = x_1625 (snd (fst ii_3086)) in
               ((true, x_5219), (true, xs_1222 (snd (snd ii_3086))))
         in
         x_1785
       in
       let x_1788 (ii_3004:((bool * int) * (bool * int))) =
         if fst (fst ii_3004) = false then
           if fst (snd ii_3004) = false then
             ((false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (true, x_1747 (snd (snd ii_3004))))
         else
           if fst (snd ii_3004) = false then
             ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
           else
             let x_5333 = xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) in
             ((true, fst x_5333), (true, snd x_5333))
       in
       let x_1789 (i_2984:int) = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
       let x_1790 (i_2977:int) = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
       let x_5480 = append_1059 x_1788 in
       let x_1792 (i_2966:int) = snd (#0 (x_5480 ((true, i_2966), (false, 0), (false, 0)))) in
       let x_1793 (i_2956:int) = snd (#1 (x_5480 ((false, 0), (true, i_2956), (false, 0)))) in
       let x_1794 (i_2946:int) = snd (#2 (x_5480 ((false, 0), (false, 0), (true, i_2946)))) in
       let rec x_x_3919 (x_3881:int) (x_3882:int) =
         let x_7211 = x_5480 ((false, 0), (true, x_3881), (true, x_3882)) in
         (snd (#1 x_7211), snd (#2 x_7211))
       in
       let x_1797 (ii_2929:((bool * int) * (bool * int))) =
         if fst (fst ii_2929) = false then
           if fst (snd ii_2929) = false then
             ((false, (true, 0)), (false, (true, 0)))
           else
             ((false, (true, 0)), (true, x_1794 (snd (snd ii_2929))))
         else
           if fst (snd ii_2929) = false then
             ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
           else
             let x_5604 = x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) in
             ((true, fst x_5604), (true, snd x_5604))
       in
       let x_1798 (i_2909:int) = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
       let x_1799 (i_2902:int) = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
       let x_5752 = cons_1225 (snd (snd (fst x_7289))) in
       let x_5753 = x_5752 x_1792 in
       let x_1802 (i_2893:int) = snd (fst (x_5753 ((true, i_2893), (false, 0)))) in
       let rec x_x_4018 (x_3992:int) (x_3993:int) =
         let x_7179 = x_5753 ((true, x_3992), (false, 0)) in
         (snd (fst x_7179), snd (fst (x_1023 ((true, x_3993), (false, 0)))))
       in
       let rec x_x_4063 (x_4037:int) (x_4038:int) =
         let x_7163 = x_5753 ((true, x_4037), (false, 0)) in
         (snd (fst x_7163), snd (snd (x_1023 ((false, 0), (true, x_4038)))))
       in
       let rec x_x_x_3972 (x_3933:int) (x_3934:int) (x_3935:int) =
         let x_7147 = x_5753 ((true, x_3933), (false, 0)) in
         let x_7138 = x_1023 ((true, x_3934), (true, x_3935)) in
         (snd (fst x_7147), snd (fst x_7138), snd (snd x_7138))
       in
       let x_1803 (i_2886:int) = snd (snd (x_5753 ((false, 0), (true, i_2886)))) in
       let x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_2861) = false then
           if fst (#1 iii_2861) = false then
             if fst (#2 iii_2861) = false then
               ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
             else
               ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
           else
             if fst (#2 iii_2861) = false then
               ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
             else
               let x_6067 = x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
               ((false, (true, 0)), (true, fst x_6067), (true, snd x_6067))
         else
           if fst (#1 iii_2861) = false then
             if fst (#2 iii_2861) = false then
               ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
             else
               let x_5983 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
               ((true, fst x_5983), (false, (true, 0)), (true, snd x_5983))
           else
             if fst (#2 iii_2861) = false then
               let x_5941 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
               ((true, fst x_5941), (true, snd x_5941), (false, (true, 0)))
             else
               let x_5909 = x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
               ((true, #0 x_5909), (true, #1 x_5909), (true, #2 x_5909))
       in
       x_1807
     else
       let x_1610 = _|_ in
       let x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_2432) = false then
           if fst (#1 iii_2432) = false then
             if fst (#2 iii_2432) = false then
               ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
             else
               ((false, (true, 0)), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
           else
             if fst (#2 iii_2432) = false then
               ((false, (true, 0)), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
             else
               let x_4980 = x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) in
               ((false, (true, 0)), (true, fst x_4980), (true, snd x_4980))
         else
           if fst (#1 iii_2432) = false then
             if fst (#2 iii_2432) = false then
               ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
             else
               let x_4897 = x_1610 (snd (#0 iii_2432)) in
               ((true, x_4897), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
           else
             if fst (#2 iii_2432) = false then
               let x_4856 = x_1610 (snd (#0 iii_2432)) in
               ((true, x_4856), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
             else
               let x_4822 = x_1610 (snd (#0 iii_2432)) in
               let x_4832 = x_1746 (snd (#1 iii_2432)) in
               ((true, x_4822), (true, x_4832), (true, x_1747 (snd (#2 iii_2432))))
       in
       x_1761
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_6568 = make_list_1008 n_1017 in
   let f_1479 (x_1329:int) = (false, 0) in
   let x_1820 (ix_2198:((bool * int) * (bool * int))) =
     if fst (fst ix_2198) = false then
       if fst (snd ix_2198) = false then
         ((false, (true, 0)), (false, (true, 0)))
       else
         ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
     else
       if fst (snd ix_2198) = false then
         ((true, x_6568 (snd (fst ix_2198))), (false, (true, 0)))
       else
         let x_6580 = x_6568 (snd (fst ix_2198)) in
         ((true, x_6580), (true, f_1479 (snd (snd ix_2198))))
   in
   let x_1821 (i_2178:int) = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
   let x_1822 (x_2171:int) = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
   let x_6726 = append_1059 x_1820 in
   let x_1824 (i_2160:int) = snd (#0 (x_6726 ((true, i_2160), (false, 0), (false, 0)))) in
   let x_1825 (i_2150:int) = snd (#1 (x_6726 ((false, 0), (true, i_2150), (false, 0)))) in
   let x_1826 (i_2140:int) = snd (#2 (x_6726 ((false, 0), (false, 0), (true, i_2140)))) in
   let rec x_x_4361 (x_4323:int) (x_4324:int) =
     let x_7069 = x_6726 ((false, 0), (true, x_4323), (true, x_4324)) in
     (snd (#1 x_7069), snd (#2 x_7069))
   in
   let x_1829 (ii_2123:((bool * int) * (bool * int))) =
     if fst (fst ii_2123) = false then
       if fst (snd ii_2123) = false then
         ((false, (true, 0)), (false, (true, 0)))
       else
         ((false, (true, 0)), (true, x_1826 (snd (snd ii_2123))))
     else
       if fst (snd ii_2123) = false then
         ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
       else
         let x_6850 = x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) in
         ((true, fst x_6850), (true, snd x_6850))
   in
   let x_1830 (i_2103:int) = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
   let x_1831 (i_2096:int) = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
   let x_7042 = x_6726 ((true, i_1016), (false, 0), (false, 0)) in
   let x_7019 = x_6568 i_1016 in
   let n_1504 = if fst (snd (#0 x_7042)) <> false then
                  snd (snd (#0 x_7042))
                else
                  _|_ in
   let n_1505 = if fst x_7019 <> false then
                  snd x_7019
                else
                  _|_ in
   if n_1504 = n_1505 then
     ()
   else
     {fail} ()
 in
 let x_7037 = rand_int () in
 let x_7039 = rand_int () in
 let x_7040 = main_1015 x_7037 in
 let x_7041 = x_7040 x_7039 in
 let x_1847 = x_7041 in
 ()

CPS:
 let rec make_list_1008 (n_1009:int) (k_make_list_7396:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_7396 (fun (x_1123:int) -> fun (k_make_list_7398:((bool * int) -> X)) -> k_make_list_7398 (false, 0))
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> ((bool * int) -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_7417:((bool * int) -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_7417 (true, x_1113)
         else
           let x_4377 (k_make_list_cons_x_x_7430:((bool * int) -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_7430 in
           x_4377 (fun (x_7455:(bool * int)) -> k_make_list_cons_x_7417 x_7455)
       in
       let
         x_1732 (ii_3523:((bool * int) * (bool * int))) 
               (k_make_list_cons_x_7464:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
         if fst (fst ii_3523) = false then
           if fst (snd ii_3523) = false then
             k_make_list_cons_x_7464 ((false, (true, 0)), (false, (true, 0)))
           else
             xs_1114 (snd (snd ii_3523))
               (fun (x_11984:(bool * int)) -> k_make_list_cons_x_7464 ((false, (true, 0)), (true, x_11984)))
         else
           if fst (snd ii_3523) = false then
             x_1592 (snd (fst ii_3523))
               (fun (x_11981:(bool * int)) -> k_make_list_cons_x_7464 ((true, x_11981), (false, (true, 0))))
           else
             let x_4395 (k_make_list_cons_x_x_7573:((bool * int) -> X)) =
               x_1592 (snd (fst ii_3523)) k_make_list_cons_x_x_7573
             in
             x_4395
               (fun (x_7607:(bool * int)) ->
                  xs_1114 (snd (snd ii_3523))
                    (fun (x_11963:(bool * int)) -> k_make_list_cons_x_7464 ((true, x_7607), (true, x_11963))))
       in
       x_1732
     in
     let x_4505 (k_make_list_x_7642:((int -> ((bool * int) -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_7642
     in
     x_4505
       (fun (x_7782:(int -> ((bool * int) -> X) -> X)) ->
          (let x_4507 (k_make_list_x_7663:(int -> X)) = rand_int_cps () k_make_list_x_7663 in
           x_4507
             (fun (x_7778:int) ->
                k_make_list_7396
                  (let x_1739 (i_3447:int) (k_make_list_x_7701:((bool * int) -> X)) =
                     ((cons_1117 x_7778) x_7782) ((true, i_3447), (false, 0))
                       (fun (p_12018:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_make_list_x_7701 (snd (fst p_12018)))
                   in
                   x_1739))))
 in
 let rec
   append_1059 (x_1023:(((bool * int) * (bool * int)) -> (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) 
              (k_append_7814:((((bool * int) * (bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                -> X)) =
   let x_1746 (i_3310:int) (k_append_x_7821:((bool * int) -> X)) =
     x_1023 ((true, i_3310), (false, 0))
       (fun (p_12038:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7821 (snd (fst p_12038)))
   in
   let x_1747 (i_3303:int) (k_append_x_7865:((bool * int) -> X)) =
     x_1023 ((false, 0), (true, i_3303))
       (fun (p_12048:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7865 (snd (snd p_12048)))
   in
   let rec x_x_4257 (x_4231:int) (x_4232:int) (k_append_x_x_7909:(((bool * int) * (bool * int)) -> X)) =
     let x_4604 (k_append_x_x_x_7934:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((false, 0), (true, x_4231)) k_append_x_x_x_7934
     in
     x_4604
       (fun (x_7980:((bool * (bool * int)) * (bool * (bool * int)))) ->
          x_1023 ((false, 0), (true, x_4232))
            (fun (p_12066:((bool * (bool * int)) * (bool * (bool * int)))) ->
               k_append_x_x_7909 (snd (snd x_7980), snd (snd p_12066))))
   in
   let rec x_x_3820 (x_3794:int) (x_3795:int) (k_append_x_x_7991:(((bool * int) * (bool * int)) -> X)) =
     let x_7334 (k_append_x_x_x_8016:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((true, x_3794), (true, x_3795)) k_append_x_x_x_8016
     in
     x_7334
       (fun (x_8028:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_x_x_7991 (snd (fst x_8028), snd (snd x_8028)))
   in
   let rec x_x_4108 (x_4082:int) (x_4083:int) (k_append_x_x_8039:(((bool * int) * (bool * int)) -> X)) =
     let x_7325 (k_append_x_x_x_8064:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((true, x_4082), (true, x_4083)) k_append_x_x_x_8064
     in
     x_7325
       (fun (x_8076:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_x_x_8039 (snd (fst x_8076), snd (snd x_8076)))
   in
   let rec
     x_x_x_4166 (x_4127:int) (x_4128:int) (x_4129:int) 
               (k_append_x_x_x_8087:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     let x_4697 (k_append_x_x_x_x_8112:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((false, 0), (true, x_4127)) k_append_x_x_x_x_8112
     in
     x_4697
       (fun (x_8157:((bool * (bool * int)) * (bool * (bool * int)))) ->
          (let x_7316 (k_append_x_x_x_x_8142:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
             x_1023 ((true, x_4128), (true, x_4129)) k_append_x_x_x_x_8142
           in
           x_7316
             (fun (x_8156:((bool * (bool * int)) * (bool * (bool * int)))) ->
                k_append_x_x_x_8087 (snd (snd x_8157), snd (fst x_8156), snd (snd x_8156)))))
   in
   let rec x_x_4212 (x_4186:int) (x_4187:int) (k_append_x_x_8169:(((bool * int) * (bool * int)) -> X)) =
     let x_7306 (k_append_x_x_x_8194:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((true, x_4187), (true, x_4186)) k_append_x_x_x_8194
     in
     x_7306
       (fun (x_8206:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_x_x_8169 (snd (snd x_8206), snd (fst x_8206)))
   in
   let rec x_x_4302 (x_4276:int) (x_4277:int) (k_append_x_x_8217:(((bool * int) * (bool * int)) -> X)) =
     let x_7297 (k_append_x_x_x_8242:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((true, x_4276), (true, x_4277)) k_append_x_x_x_8242
     in
     x_7297
       (fun (x_8254:((bool * (bool * int)) * (bool * (bool * int)))) ->
          k_append_x_x_8217 (snd (fst x_8254), snd (snd x_8254)))
   in
   let x_7289 (k_append_x_8286:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
     x_1023 ((true, 0), (false, 0)) k_append_x_8286
   in
   x_7289
     (fun (x_10952:((bool * (bool * int)) * (bool * (bool * int)))) ->
        (if fst (snd (fst x_10952)) = false then
           k_append_7814
             (let
                x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) 
                      (k_append_x_8296:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                if fst (#0 iii_3257) = false then
                  if fst (#1 iii_3257) = false then
                    if fst (#2 iii_3257) = false then
                      k_append_x_8296 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                    else
                      x_1747 (snd (#2 iii_3257))
                        (fun (x_13005:(bool * int)) ->
                           k_append_x_8296 ((false, (true, 0)), (false, (true, 0)), (true, x_13005)))
                  else
                    if fst (#2 iii_3257) = false then
                      x_1746 (snd (#1 iii_3257))
                        (fun (x_12992:(bool * int)) ->
                           k_append_x_8296 ((false, (true, 0)), (true, x_12992), (false, (true, 0))))
                    else
                      let x_6401 (k_append_x_x_8448:(((bool * int) * (bool * int)) -> X)) =
                        x_x_4302 (snd (#1 iii_3257)) (snd (#2 iii_3257)) k_append_x_x_8448
                      in
                      x_6401
                        (fun (x_8486:((bool * int) * (bool * int))) ->
                           k_append_x_8296 ((false, (true, 0)), (true, fst x_8486), (true, snd x_8486)))
                else
                  if fst (#1 iii_3257) = false then
                    if fst (#2 iii_3257) = false then
                      x_1747 (snd (#0 iii_3257))
                        (fun (x_12949:(bool * int)) ->
                           k_append_x_8296 ((true, x_12949), (false, (true, 0)), (false, (true, 0))))
                    else
                      let x_6317 (k_append_x_x_8550:(((bool * int) * (bool * int)) -> X)) =
                        x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) k_append_x_x_8550
                      in
                      x_6317
                        (fun (x_8588:((bool * int) * (bool * int))) ->
                           k_append_x_8296 ((true, fst x_8588), (false, (true, 0)), (true, snd x_8588)))
                  else
                    if fst (#2 iii_3257) = false then
                      let x_6275 (k_append_x_x_8600:(((bool * int) * (bool * int)) -> X)) =
                        x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) k_append_x_x_8600
                      in
                      x_6275
                        (fun (x_8638:((bool * int) * (bool * int))) ->
                           k_append_x_8296 ((true, fst x_8638), (true, snd x_8638), (false, (true, 0))))
                    else
                      let x_6243 (k_append_x_x_8647:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                        x_x_x_4166 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) k_append_x_x_8647
                      in
                      x_6243
                        (fun (x_8679:((bool * int) * (bool * int) * (bool * int))) ->
                           k_append_x_8296 ((true, #0 x_8679), (true, #1 x_8679), (true, #2 x_8679)))
              in
              x_1811)
         else
           if fst (snd (fst x_10952)) <> false then
             let xs'_1014 (x_1150:int) (k_append_xs'_8701:((bool * int) -> X)) =
               x_1023 ((true, x_1150 + 1), (false, 0))
                 (fun (p_12338:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_8701 (snd (fst p_12338)))
             in
             let rec xs'_x_3867 (x_3841:int) (x_3842:int) (k_append_xs'_x_8745:(((bool * int) * (bool * int)) -> X)) =
               let x_7272 (k_append_xs'_x_x_8770:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_1023 ((true, x_3841 + 1), (true, x_3842)) k_append_xs'_x_x_8770
               in
               x_7272
                 (fun (x_8782:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_x_8745 (snd (fst x_8782), snd (snd x_8782)))
             in
             let cons_1225 (x_1221:int) (xs_1222:(int -> ((bool * int) -> X) -> X)) =
               let x_1625 (i_1220:int) (k_append_cons_x_8799:((bool * int) -> X)) =
                 if i_1220 = 0 then
                   k_append_cons_x_8799 (true, x_1221)
                 else
                   let x_5201 (k_append_cons_x_x_8812:((bool * int) -> X)) =
                     xs_1222 (i_1220 - 1) k_append_cons_x_x_8812
                   in
                   x_5201 (fun (x_8837:(bool * int)) -> k_append_cons_x_8799 x_8837)
               in
               let
                 x_1785 (ii_3086:((bool * int) * (bool * int))) 
                       (k_append_cons_x_8846:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_3086) = false then
                   if fst (snd ii_3086) = false then
                     k_append_cons_x_8846 ((false, (true, 0)), (false, (true, 0)))
                   else
                     xs_1222 (snd (snd ii_3086))
                       (fun (x_12369:(bool * int)) -> k_append_cons_x_8846 ((false, (true, 0)), (true, x_12369)))
                 else
                   if fst (snd ii_3086) = false then
                     x_1625 (snd (fst ii_3086))
                       (fun (x_12366:(bool * int)) -> k_append_cons_x_8846 ((true, x_12366), (false, (true, 0))))
                   else
                     let x_5219 (k_append_cons_x_x_8955:((bool * int) -> X)) =
                       x_1625 (snd (fst ii_3086)) k_append_cons_x_x_8955
                     in
                     x_5219
                       (fun (x_8989:(bool * int)) ->
                          xs_1222 (snd (snd ii_3086))
                            (fun (x_12348:(bool * int)) -> k_append_cons_x_8846 ((true, x_8989), (true, x_12348))))
               in
               x_1785
             in
             let
               x_1788 (ii_3004:((bool * int) * (bool * int))) 
                     (k_append_x_9015:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
               if fst (fst ii_3004) = false then
                 if fst (snd ii_3004) = false then
                   k_append_x_9015 ((false, (true, 0)), (false, (true, 0)))
                 else
                   x_1747 (snd (snd ii_3004))
                     (fun (x_12414:(bool * int)) -> k_append_x_9015 ((false, (true, 0)), (true, x_12414)))
               else
                 if fst (snd ii_3004) = false then
                   xs'_1014 (snd (fst ii_3004))
                     (fun (x_12411:(bool * int)) -> k_append_x_9015 ((true, x_12411), (false, (true, 0))))
                 else
                   let x_5333 (k_append_x_x_9125:(((bool * int) * (bool * int)) -> X)) =
                     xs'_x_3867 (snd (fst ii_3004)) (snd (snd ii_3004)) k_append_x_x_9125
                   in
                   x_5333
                     (fun (x_9149:((bool * int) * (bool * int))) ->
                        k_append_x_9015 ((true, fst x_9149), (true, snd x_9149)))
             in
             let
               x_5480
                     (k_append_x_9270:((((bool * int) * (bool * int) * (bool * int)) ->
                                          (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)
                                            -> X) -> X)) = append_1059 x_1788 k_append_x_9270
             in
             x_5480
               (fun (x_10448:(((bool * int) * (bool * int) * (bool * int)) ->
                                (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                  k_append_7814
                    (let x_1792 (i_2966:int) (k_append_x_9292:((bool * int) -> X)) =
                       x_10448 ((true, i_2966), (false, 0), (false, 0))
                         (fun (p_12477:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_9292 (snd (#0 p_12477)))
                     in
                     let x_1793 (i_2956:int) (k_append_x_9339:((bool * int) -> X)) =
                       x_10448 ((false, 0), (true, i_2956), (false, 0))
                         (fun (p_12496:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_9339 (snd (#1 p_12496)))
                     in
                     let x_1794 (i_2946:int) (k_append_x_9386:((bool * int) -> X)) =
                       x_10448 ((false, 0), (false, 0), (true, i_2946))
                         (fun (p_12515:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_9386 (snd (#2 p_12515)))
                     in
                     let rec
                       x_x_3919 (x_3881:int) (x_3882:int) (k_append_x_x_9434:(((bool * int) * (bool * int)) -> X)) =
                       let
                         x_7211
                               (k_append_x_x_x_9467:(((bool * (bool * int)) * (
                                                      bool * (bool * int)) * (
                                                      bool * (bool * int))) -> X)) =
                         x_10448 ((false, 0), (true, x_3881), (true, x_3882)) k_append_x_x_x_9467
                       in
                       x_7211
                         (fun (x_9479:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_x_9434 (snd (#1 x_9479), snd (#2 x_9479)))
                     in
                     let
                       x_1797 (ii_2929:((bool * int) * (bool * int))) 
                             (k_append_x_9484:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       if fst (fst ii_2929) = false then
                         if fst (snd ii_2929) = false then
                           k_append_x_9484 ((false, (true, 0)), (false, (true, 0)))
                         else
                           x_1794 (snd (snd ii_2929))
                             (fun (x_12553:(bool * int)) -> k_append_x_9484 ((false, (true, 0)), (true, x_12553)))
                       else
                         if fst (snd ii_2929) = false then
                           x_1793 (snd (fst ii_2929))
                             (fun (x_12550:(bool * int)) -> k_append_x_9484 ((true, x_12550), (false, (true, 0))))
                         else
                           let x_5604 (k_append_x_x_9594:(((bool * int) * (bool * int)) -> X)) =
                             x_x_3919 (snd (fst ii_2929)) (snd (snd ii_2929)) k_append_x_x_9594
                           in
                           x_5604
                             (fun (x_9618:((bool * int) * (bool * int))) ->
                                k_append_x_9484 ((true, fst x_9618), (true, snd x_9618)))
                     in
                     let x_1802 (i_2893:int) (k_append_x_9737:((bool * int) -> X)) =
                       ((cons_1225 (snd (snd (fst x_10952)))) x_1792) 
                         ((true, i_2893), (false, 0))
                         (fun (p_12607:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_9737 (snd (fst p_12607)))
                     in
                     let rec
                       x_x_4018 (x_3992:int) (x_3993:int) (k_append_x_x_9777:(((bool * int) * (bool * int)) -> X)) =
                       let x_7179 (k_append_x_x_x_9802:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1225 (snd (snd (fst x_10952)))) x_1792) 
                           ((true, x_3992), (false, 0)) k_append_x_x_x_9802
                       in
                       x_7179
                         (fun (x_9848:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            x_1023 ((true, x_3993), (false, 0))
                              (fun (p_12625:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                 k_append_x_x_9777 (snd (fst x_9848), snd (fst p_12625))))
                     in
                     let rec
                       x_x_4063 (x_4037:int) (x_4038:int) (k_append_x_x_9854:(((bool * int) * (bool * int)) -> X)) =
                       let x_7163 (k_append_x_x_x_9879:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1225 (snd (snd (fst x_10952)))) x_1792) 
                           ((true, x_4037), (false, 0)) k_append_x_x_x_9879
                       in
                       x_7163
                         (fun (x_9925:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            x_1023 ((false, 0), (true, x_4038))
                              (fun (p_12645:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                 k_append_x_x_9854 (snd (fst x_9925), snd (snd p_12645))))
                     in
                     let rec
                       x_x_x_3972 (x_3933:int) (x_3934:int) (x_3935:int) 
                                 (k_append_x_x_x_9932:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                       let x_7147 (k_append_x_x_x_x_9957:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1225 (snd (snd (fst x_10952)))) x_1792) 
                           ((true, x_3933), (false, 0)) k_append_x_x_x_x_9957
                       in
                       x_7147
                         (fun (x_10002:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_7138 (k_append_x_x_x_x_9987:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                               x_1023 ((true, x_3934), (true, x_3935)) k_append_x_x_x_x_9987
                             in
                             x_7138
                               (fun (x_10001:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                  k_append_x_x_x_9932 (snd (fst x_10002), snd (fst x_10001), snd (snd x_10001)))))
                     in
                     let
                       x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_10046:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                  -> X)) =
                       if fst (#0 iii_2861) = false then
                         if fst (#1 iii_2861) = false then
                           if fst (#2 iii_2861) = false then
                             k_append_x_10046 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                           else
                             x_1747 (snd (#2 iii_2861))
                               (fun (x_12810:(bool * int)) ->
                                  k_append_x_10046 ((false, (true, 0)), (false, (true, 0)), (true, x_12810)))
                         else
                           if fst (#2 iii_2861) = false then
                             x_1746 (snd (#1 iii_2861))
                               (fun (x_12797:(bool * int)) ->
                                  k_append_x_10046 ((false, (true, 0)), (true, x_12797), (false, (true, 0))))
                           else
                             let x_6067 (k_append_x_x_10198:(((bool * int) * (bool * int)) -> X)) =
                               x_x_4108 (snd (#1 iii_2861)) (snd (#2 iii_2861)) k_append_x_x_10198
                             in
                             x_6067
                               (fun (x_10236:((bool * int) * (bool * int))) ->
                                  k_append_x_10046 ((false, (true, 0)), (true, fst x_10236), (true, snd x_10236)))
                       else
                         if fst (#1 iii_2861) = false then
                           if fst (#2 iii_2861) = false then
                             x_1802 (snd (#0 iii_2861))
                               (fun (x_12754:(bool * int)) ->
                                  k_append_x_10046 ((true, x_12754), (false, (true, 0)), (false, (true, 0))))
                           else
                             let x_5983 (k_append_x_x_10300:(((bool * int) * (bool * int)) -> X)) =
                               x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) k_append_x_x_10300
                             in
                             x_5983
                               (fun (x_10338:((bool * int) * (bool * int))) ->
                                  k_append_x_10046 ((true, fst x_10338), (false, (true, 0)), (true, snd x_10338)))
                         else
                           if fst (#2 iii_2861) = false then
                             let x_5941 (k_append_x_x_10350:(((bool * int) * (bool * int)) -> X)) =
                               x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) k_append_x_x_10350
                             in
                             x_5941
                               (fun (x_10388:((bool * int) * (bool * int))) ->
                                  k_append_x_10046 ((true, fst x_10388), (true, snd x_10388), (false, (true, 0))))
                           else
                             let x_5909 (k_append_x_x_10397:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                               x_x_x_3972 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (
                                 snd (#2 iii_2861)) k_append_x_x_10397
                             in
                             x_5909
                               (fun (x_10429:((bool * int) * (bool * int) * (bool * int))) ->
                                  k_append_x_10046 ((true, #0 x_10429), (true, #1 x_10429), (true, #2 x_10429)))
                     in
                     x_1807))
           else
             let x_1610 (k_append_x_10488:((int -> ((bool * int) -> X) -> X) -> X)) = _|_ in
             x_1610
               (fun (x_10937:(int -> ((bool * int) -> X) -> X)) ->
                  k_append_7814
                    (let
                       x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_10496:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                  -> X)) =
                       if fst (#0 iii_2432) = false then
                         if fst (#1 iii_2432) = false then
                           if fst (#2 iii_2432) = false then
                             k_append_x_10496 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                           else
                             x_1747 (snd (#2 iii_2432))
                               (fun (x_12270:(bool * int)) ->
                                  k_append_x_10496 ((false, (true, 0)), (false, (true, 0)), (true, x_12270)))
                         else
                           if fst (#2 iii_2432) = false then
                             x_1746 (snd (#1 iii_2432))
                               (fun (x_12257:(bool * int)) ->
                                  k_append_x_10496 ((false, (true, 0)), (true, x_12257), (false, (true, 0))))
                           else
                             let x_4980 (k_append_x_x_10648:(((bool * int) * (bool * int)) -> X)) =
                               x_x_3820 (snd (#1 iii_2432)) (snd (#2 iii_2432)) k_append_x_x_10648
                             in
                             x_4980
                               (fun (x_10686:((bool * int) * (bool * int))) ->
                                  k_append_x_10496 ((false, (true, 0)), (true, fst x_10686), (true, snd x_10686)))
                       else
                         if fst (#1 iii_2432) = false then
                           if fst (#2 iii_2432) = false then
                             x_10937 (snd (#0 iii_2432))
                               (fun (x_12214:(bool * int)) ->
                                  k_append_x_10496 ((true, x_12214), (false, (true, 0)), (false, (true, 0))))
                           else
                             let x_4897 (k_append_x_x_10749:((bool * int) -> X)) =
                               x_10937 (snd (#0 iii_2432)) k_append_x_x_10749
                             in
                             x_4897
                               (fun (x_10797:(bool * int)) ->
                                  x_1747 (snd (#2 iii_2432))
                                    (fun (x_12165:(bool * int)) ->
                                       k_append_x_10496 ((true, x_10797), (false, (true, 0)), (true, x_12165))))
                         else
                           if fst (#2 iii_2432) = false then
                             let x_4856 (k_append_x_x_10808:((bool * int) -> X)) =
                               x_10937 (snd (#0 iii_2432)) k_append_x_x_10808
                             in
                             x_4856
                               (fun (x_10856:(bool * int)) ->
                                  x_1746 (snd (#1 iii_2432))
                                    (fun (x_12157:(bool * int)) ->
                                       k_append_x_10496 ((true, x_10856), (true, x_12157), (false, (true, 0)))))
                           else
                             let x_4822 (k_append_x_x_10863:((bool * int) -> X)) =
                               x_10937 (snd (#0 iii_2432)) k_append_x_x_10863
                             in
                             x_4822
                               (fun (x_10918:(bool * int)) ->
                                  (let x_4832 (k_append_x_x_10875:((bool * int) -> X)) =
                                     x_1746 (snd (#1 iii_2432)) k_append_x_x_10875
                                   in
                                   x_4832
                                     (fun (x_10917:(bool * int)) ->
                                        x_1747 (snd (#2 iii_2432))
                                          (fun (x_12126:(bool * int)) ->
                                             k_append_x_10496 ((true, x_10918), (true, x_10917), (true, x_12126))))))
                     in
                     x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_11004:(unit -> X)) =
   let x_6568 (k_main_x_11017:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_11017 in
   x_6568
     (fun (x_11882:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_11032:((bool * int) -> X)) = k_main_f_11032 (false, 0) in
         let
           x_1820 (ix_2198:((bool * int) * (bool * int))) 
                 (k_main_x_11045:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
           if fst (fst ix_2198) = false then
             if fst (snd ix_2198) = false then
               k_main_x_11045 ((false, (true, 0)), (false, (true, 0)))
             else
               f_1479 (snd (snd ix_2198))
                 (fun (x_13086:(bool * int)) -> k_main_x_11045 ((false, (true, 0)), (true, x_13086)))
           else
             if fst (snd ix_2198) = false then
               x_11882 (snd (fst ix_2198))
                 (fun (x_13083:(bool * int)) -> k_main_x_11045 ((true, x_13083), (false, (true, 0))))
             else
               let x_6580 (k_main_x_x_11154:((bool * int) -> X)) = x_11882 (snd (fst ix_2198)) k_main_x_x_11154 in
               x_6580
                 (fun (x_11188:(bool * int)) ->
                    f_1479 (snd (snd ix_2198))
                      (fun (x_13065:(bool * int)) -> k_main_x_11045 ((true, x_11188), (true, x_13065))))
         in
         let
           x_6726
                 (k_main_x_11300:((((bool * int) * (bool * int) * (bool * int)) ->
                                     (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) ->
                                       X) -> X)) = append_1059 x_1820 k_main_x_11300
         in
         x_6726
           (fun (x_11862:(((bool * int) * (bool * int) * (bool * int)) ->
                            (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_11370:((bool * int) -> X)) =
                 x_11862 ((false, 0), (true, i_2150), (false, 0))
                   (fun (p_13168:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_11370 (snd (#1 p_13168)))
               in
               let x_1826 (i_2140:int) (k_main_x_11419:((bool * int) -> X)) =
                 x_11862 ((false, 0), (false, 0), (true, i_2140))
                   (fun (p_13187:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_11419 (snd (#2 p_13187)))
               in
               let rec x_x_4361 (x_4323:int) (x_4324:int) (k_main_x_x_11468:(((bool * int) * (bool * int)) -> X)) =
                 let
                   x_7069
                         (k_main_x_x_x_11501:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                -> X)) =
                   x_11862 ((false, 0), (true, x_4323), (true, x_4324)) k_main_x_x_x_11501
                 in
                 x_7069
                   (fun (x_11513:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_x_11468 (snd (#1 x_11513), snd (#2 x_11513)))
               in
               let
                 x_1829 (ii_2123:((bool * int) * (bool * int))) 
                       (k_main_x_11521:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_2123) = false then
                   if fst (snd ii_2123) = false then
                     k_main_x_11521 ((false, (true, 0)), (false, (true, 0)))
                   else
                     x_1826 (snd (snd ii_2123))
                       (fun (x_13225:(bool * int)) -> k_main_x_11521 ((false, (true, 0)), (true, x_13225)))
                 else
                   if fst (snd ii_2123) = false then
                     x_1825 (snd (fst ii_2123))
                       (fun (x_13222:(bool * int)) -> k_main_x_11521 ((true, x_13222), (false, (true, 0))))
                   else
                     let x_6850 (k_main_x_x_11631:(((bool * int) * (bool * int)) -> X)) =
                       x_x_4361 (snd (fst ii_2123)) (snd (snd ii_2123)) k_main_x_x_11631
                     in
                     x_6850
                       (fun (x_11655:((bool * int) * (bool * int))) ->
                          k_main_x_11521 ((true, fst x_11655), (true, snd x_11655)))
               in
               let
                 x_7042 (k_main_x_11781:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_11862 ((true, i_1016), (false, 0), (false, 0)) k_main_x_11781
               in
               x_7042
                 (fun (x_11829:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let x_7019 (k_main_x_11793:((bool * int) -> X)) = x_11882 i_1016 k_main_x_11793 in
                     x_7019
                       (fun (x_11828:(bool * int)) ->
                          (let n_1504 (k_main_n_11804:(int -> X)) =
                             if fst (snd (#0 x_11829)) <> false then
                               k_main_n_11804 (snd (snd (#0 x_11829)))
                             else
                               _|_
                           in
                           n_1504
                             (fun (n_11827:int) ->
                                (let n_1505 (k_main_n_11812:(int -> X)) =
                                   if fst x_11828 <> false then
                                     k_main_n_11812 (snd x_11828)
                                   else
                                     _|_
                                 in
                                 n_1505
                                   (fun (n_11826:int) ->
                                      (if n_11827 = n_11826 then
                                         k_main_11004 ()
                                       else
                                         {|fail|} () k_main_11004))))))))))))
 in
 let x_7037 (k_x_11893:(int -> X)) = rand_int_cps () k_x_11893 in
 x_7037
   (fun (x_11938:int) ->
      (let x_7039 (k_x_11905:(int -> X)) = rand_int_cps () k_x_11905 in
       x_7039
         (fun (x_11937:int) ->
            (let x_7041 (k_x_11926:(unit -> X)) = (main_1015 x_11938) x_11937 k_x_11926 in
             x_7041 (fun (x_11932:unit) -> {end})))))

remove_pair:
 let rec make_list_1008 (n_1009:int) (k_make_list_7396:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_7396 (fun (x_1123:int) -> fun (k_make_list_7398:(bool -> int -> X)) -> k_make_list_7398 false 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool -> int -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_7417:(bool -> int -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_7417 true x_1113
         else
           let x_4377 (k_make_list_cons_x_x_7430:(bool -> int -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_7430 in
           x_4377 (fun (x0_7455:bool) -> fun (x1_7455:int) -> k_make_list_cons_x_7417 x0_7455 x1_7455)
       in
       let
         x_1732 (ii00_3523:bool) (ii01_3523:int) (ii10_3523:bool) (ii11_3523:int) 
               (k_make_list_cons_x_7464:(bool ->
                                           bool ->
                                             r011_7461:int ->
                                               bool -> bool -> r111_7461:int[\r111_7461. r011_7461 = r111_7461] -> X)) =
         if ii00_3523 = false then
           if ii10_3523 = false then
             k_make_list_cons_x_7464 false true 0 false true 0
           else
             xs_1114 ii11_3523
               (fun (x0_11984:bool) ->
                  fun (x1_11984:int) -> k_make_list_cons_x_7464 false true 0 true x0_11984 x1_11984)
         else
           if ii10_3523 = false then
             x_1592 ii01_3523
               (fun (x0_11981:bool) ->
                  fun (x1_11981:int) -> k_make_list_cons_x_7464 true x0_11981 x1_11981 false true 0)
           else
             let x_4395 (k_make_list_cons_x_x_7573:(bool -> int -> X)) = x_1592 ii01_3523 k_make_list_cons_x_x_7573 in
             x_4395
               (fun (x0_7607:bool) ->
                  fun (x1_7607:int) ->
                    xs_1114 ii11_3523
                      (fun (x0_11963:bool) ->
                         fun (x1_11963:int) -> k_make_list_cons_x_7464 true x0_7607 x1_7607 true x0_11963 x1_11963))
       in
       x_1732
     in
     let x_4505 (k_make_list_x_7642:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_7642
     in
     x_4505
       (fun (x_7782:(int -> (bool -> int -> X) -> X)) ->
          (let x_4507 (k_make_list_x_7663:(int -> X)) = rand_int_cps () k_make_list_x_7663 in
           x_4507
             (fun (x_7778:int) ->
                k_make_list_7396
                  (let x_1739 (i_3447:int) (k_make_list_x_7701:(bool -> int -> X)) =
                     cons_1117 x_7778 x_7782 true i_3447 false 0
                       (fun (p00_12018:bool) ->
                          fun (p010_12018:bool) ->
                            fun (p011_12018:int) ->
                              fun (p10_12018:bool) ->
                                fun (p110_12018:bool) ->
                                  fun (p111_12018:int) -> k_make_list_x_7701 p010_12018 p011_12018)
                   in
                   x_1739))))
 in
 let rec
   append_1059
              (x_1023:(bool ->
                         int ->
                           bool ->
                             int ->
                               (bool ->
                                  bool ->
                                    r011_7808:int ->
                                      bool -> bool -> r111_7808:int[\r111_7808. r011_7808 = r111_7808] -> X) -> X))
              (k_append_7814:((bool ->
                                 int ->
                                   bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_7811:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_7811:int[\r111_7811. r011_7811 = r111_7811] ->
                                                        bool -> bool -> int -> X) -> X) -> X)) =
   let x_1746 (i_3310:int) (k_append_x_7821:(bool -> int -> X)) =
     x_1023 true i_3310 false 0
       (fun (p00_12038:bool) ->
          fun (p010_12038:bool) ->
            fun (p011_12038:int) ->
              fun (p10_12038:bool) ->
                fun (p110_12038:bool) -> fun (p111_12038:int) -> k_append_x_7821 p010_12038 p011_12038)
   in
   let x_1747 (i_3303:int) (k_append_x_7865:(bool -> int -> X)) =
     x_1023 false 0 true i_3303
       (fun (p00_12048:bool) ->
          fun (p010_12048:bool) ->
            fun (p011_12048:int) ->
              fun (p10_12048:bool) ->
                fun (p110_12048:bool) -> fun (p111_12048:int) -> k_append_x_7865 p110_12048 p111_12048)
   in
   let rec x_x_4257 (x_4231:int) (x_4232:int) (k_append_x_x_7909:(bool -> int -> bool -> int -> X)) =
     let
       x_4604
             (k_append_x_x_x_7934:(bool ->
                                     bool ->
                                       r011_7933:int ->
                                         bool -> bool -> r111_7933:int[\r111_7933. r011_7933 = r111_7933] -> X)) =
       x_1023 false 0 true x_4231 k_append_x_x_x_7934
     in
     x_4604
       (fun (x00_7980:bool) ->
          fun (x010_7980:bool) ->
            fun (x011_7980:int) ->
              fun (x10_7980:bool) ->
                fun (x110_7980:bool) ->
                  fun (x111_7980:int) ->
                    x_1023 false 0 true x_4232
                      (fun (p00_12066:bool) ->
                         fun (p010_12066:bool) ->
                           fun (p011_12066:int) ->
                             fun (p10_12066:bool) ->
                               fun (p110_12066:bool) ->
                                 fun (p111_12066:int) -> k_append_x_x_7909 x110_7980 x111_7980 p110_12066 p111_12066))
   in
   let rec x_x_3820 (x_3794:int) (x_3795:int) (k_append_x_x_7991:(bool -> int -> bool -> int -> X)) =
     let
       x_7334
             (k_append_x_x_x_8016:(bool ->
                                     bool ->
                                       r011_8015:int ->
                                         bool -> bool -> r111_8015:int[\r111_8015. r011_8015 = r111_8015] -> X)) =
       x_1023 true x_3794 true x_3795 k_append_x_x_x_8016
     in
     x_7334
       (fun (x00_8028:bool) ->
          fun (x010_8028:bool) ->
            fun (x011_8028:int) ->
              fun (x10_8028:bool) ->
                fun (x110_8028:bool) ->
                  fun (x111_8028:int) -> k_append_x_x_7991 x010_8028 x011_8028 x110_8028 x111_8028)
   in
   let rec x_x_4108 (x_4082:int) (x_4083:int) (k_append_x_x_8039:(bool -> int -> bool -> int -> X)) =
     let
       x_7325
             (k_append_x_x_x_8064:(bool ->
                                     bool ->
                                       r011_8063:int ->
                                         bool -> bool -> r111_8063:int[\r111_8063. r011_8063 = r111_8063] -> X)) =
       x_1023 true x_4082 true x_4083 k_append_x_x_x_8064
     in
     x_7325
       (fun (x00_8076:bool) ->
          fun (x010_8076:bool) ->
            fun (x011_8076:int) ->
              fun (x10_8076:bool) ->
                fun (x110_8076:bool) ->
                  fun (x111_8076:int) -> k_append_x_x_8039 x010_8076 x011_8076 x110_8076 x111_8076)
   in
   let rec
     x_x_x_4166 (x_4127:int) (x_4128:int) (x_4129:int) 
               (k_append_x_x_x_8087:(bool -> int -> bool -> int -> bool -> int -> X)) =
     let
       x_4697
             (k_append_x_x_x_x_8112:(bool ->
                                       bool ->
                                         r011_8111:int ->
                                           bool -> bool -> r111_8111:int[\r111_8111. r011_8111 = r111_8111] -> X)) =
       x_1023 false 0 true x_4127 k_append_x_x_x_x_8112
     in
     x_4697
       (fun (x00_8157:bool) ->
          fun (x010_8157:bool) ->
            fun (x011_8157:int) ->
              fun (x10_8157:bool) ->
                fun (x110_8157:bool) ->
                  fun (x111_8157:int) ->
                    (let
                       x_7316
                             (k_append_x_x_x_x_8142:(bool ->
                                                       bool ->
                                                         r011_8141:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_8141:int[\r111_8141. r011_8141 = r111_8141] -> X)) =
                       x_1023 true x_4128 true x_4129 k_append_x_x_x_x_8142
                     in
                     x_7316
                       (fun (x00_8156:bool) ->
                          fun (x010_8156:bool) ->
                            fun (x011_8156:int) ->
                              fun (x10_8156:bool) ->
                                fun (x110_8156:bool) ->
                                  fun (x111_8156:int) ->
                                    k_append_x_x_x_8087 x110_8157 x111_8157 x010_8156 x011_8156 x110_8156 x111_8156)))
   in
   let rec x_x_4212 (x_4186:int) (x_4187:int) (k_append_x_x_8169:(bool -> int -> bool -> int -> X)) =
     let
       x_7306
             (k_append_x_x_x_8194:(bool ->
                                     bool ->
                                       r011_8193:int ->
                                         bool -> bool -> r111_8193:int[\r111_8193. r011_8193 = r111_8193] -> X)) =
       x_1023 true x_4187 true x_4186 k_append_x_x_x_8194
     in
     x_7306
       (fun (x00_8206:bool) ->
          fun (x010_8206:bool) ->
            fun (x011_8206:int) ->
              fun (x10_8206:bool) ->
                fun (x110_8206:bool) ->
                  fun (x111_8206:int) -> k_append_x_x_8169 x110_8206 x111_8206 x010_8206 x011_8206)
   in
   let rec x_x_4302 (x_4276:int) (x_4277:int) (k_append_x_x_8217:(bool -> int -> bool -> int -> X)) =
     let
       x_7297
             (k_append_x_x_x_8242:(bool ->
                                     bool ->
                                       r011_8241:int ->
                                         bool -> bool -> r111_8241:int[\r111_8241. r011_8241 = r111_8241] -> X)) =
       x_1023 true x_4276 true x_4277 k_append_x_x_x_8242
     in
     x_7297
       (fun (x00_8254:bool) ->
          fun (x010_8254:bool) ->
            fun (x011_8254:int) ->
              fun (x10_8254:bool) ->
                fun (x110_8254:bool) ->
                  fun (x111_8254:int) -> k_append_x_x_8217 x010_8254 x011_8254 x110_8254 x111_8254)
   in
   let
     x_7289
           (k_append_x_8286:(bool ->
                               bool ->
                                 r011_8285:int -> bool -> bool -> r111_8285:int[\r111_8285. r011_8285 = r111_8285] -> X)) =
     x_1023 true 0 false 0 k_append_x_8286
   in
   x_7289
     (fun (x00_10952:bool) ->
        fun (x010_10952:bool) ->
          fun (x011_10952:int) ->
            fun (x10_10952:bool) ->
              fun (x110_10952:bool) ->
                fun (x111_10952:int) ->
                  (if x010_10952 = false then
                     k_append_7814
                       (let
                          x_1811 (iii00_3257:bool) (iii01_3257:int) (iii10_3257:bool) (iii11_3257:int) 
                                (iii20_3257:bool) (iii21_3257:int) 
                                (k_append_x_8296:(bool ->
                                                    bool ->
                                                      r011_8293:int ->
                                                        bool ->
                                                          bool ->
                                                            r111_8293:
                                                              int[\r111_8293. r011_8293 = r111_8293] ->
                                                              bool -> bool -> int -> X)) =
                          if iii00_3257 = false then
                            if iii10_3257 = false then
                              if iii20_3257 = false then
                                k_append_x_8296 false true 0 false true 0 false true 0
                              else
                                x_1747 iii21_3257
                                  (fun (x0_13005:bool) ->
                                     fun (x1_13005:int) ->
                                       k_append_x_8296 false true 0 false true 0 true x0_13005 x1_13005)
                            else
                              if iii20_3257 = false then
                                x_1746 iii11_3257
                                  (fun (x0_12992:bool) ->
                                     fun (x1_12992:int) ->
                                       k_append_x_8296 false true 0 true x0_12992 x1_12992 false true 0)
                              else
                                let x_6401 (k_append_x_x_8448:(bool -> int -> bool -> int -> X)) =
                                  x_x_4302 iii11_3257 iii21_3257 k_append_x_x_8448
                                in
                                x_6401
                                  (fun (x00_8486:bool) ->
                                     fun (x01_8486:int) ->
                                       fun (x10_8486:bool) ->
                                         fun (x11_8486:int) ->
                                           k_append_x_8296 false true 0 true x00_8486 x01_8486 true x10_8486 x11_8486)
                          else
                            if iii10_3257 = false then
                              if iii20_3257 = false then
                                x_1747 iii01_3257
                                  (fun (x0_12949:bool) ->
                                     fun (x1_12949:int) ->
                                       k_append_x_8296 true x0_12949 x1_12949 false true 0 false true 0)
                              else
                                let x_6317 (k_append_x_x_8550:(bool -> int -> bool -> int -> X)) =
                                  x_x_4257 iii01_3257 iii21_3257 k_append_x_x_8550
                                in
                                x_6317
                                  (fun (x00_8588:bool) ->
                                     fun (x01_8588:int) ->
                                       fun (x10_8588:bool) ->
                                         fun (x11_8588:int) ->
                                           k_append_x_8296 true x00_8588 x01_8588 false true 0 true x10_8588 x11_8588)
                            else
                              if iii20_3257 = false then
                                let x_6275 (k_append_x_x_8600:(bool -> int -> bool -> int -> X)) =
                                  x_x_4212 iii01_3257 iii11_3257 k_append_x_x_8600
                                in
                                x_6275
                                  (fun (x00_8638:bool) ->
                                     fun (x01_8638:int) ->
                                       fun (x10_8638:bool) ->
                                         fun (x11_8638:int) ->
                                           k_append_x_8296 true x00_8638 x01_8638 true x10_8638 x11_8638 false true 0)
                              else
                                let x_6243 (k_append_x_x_8647:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                  x_x_x_4166 iii01_3257 iii11_3257 iii21_3257 k_append_x_x_8647
                                in
                                x_6243
                                  (fun (x00_8679:bool) ->
                                     fun (x01_8679:int) ->
                                       fun (x10_8679:bool) ->
                                         fun (x11_8679:int) ->
                                           fun (x20_8679:bool) ->
                                             fun (x21_8679:int) ->
                                               k_append_x_8296 true x00_8679 x01_8679 true x10_8679 x11_8679 true
                                                 x20_8679 x21_8679)
                        in
                        x_1811)
                   else
                     if x010_10952 <> false then
                       let xs'_1014 (x_1150:int) (k_append_xs'_8701:(bool -> int -> X)) =
                         x_1023 true (x_1150 + 1) false 0
                           (fun (p00_12338:bool) ->
                              fun (p010_12338:bool) ->
                                fun (p011_12338:int) ->
                                  fun (p10_12338:bool) ->
                                    fun (p110_12338:bool) ->
                                      fun (p111_12338:int) -> k_append_xs'_8701 p010_12338 p011_12338)
                       in
                       let rec
                         xs'_x_3867 (x_3841:int) (x_3842:int) (k_append_xs'_x_8745:(bool -> int -> bool -> int -> X)) =
                         let
                           x_7272
                                 (k_append_xs'_x_x_8770:(bool ->
                                                           bool ->
                                                             r011_8769:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8769:
                                                                    int[\r111_8769. r011_8769 = r111_8769] -> X)) =
                           x_1023 true (x_3841 + 1) true x_3842 k_append_xs'_x_x_8770
                         in
                         x_7272
                           (fun (x00_8782:bool) ->
                              fun (x010_8782:bool) ->
                                fun (x011_8782:int) ->
                                  fun (x10_8782:bool) ->
                                    fun (x110_8782:bool) ->
                                      fun (x111_8782:int) ->
                                        k_append_xs'_x_8745 x010_8782 x011_8782 x110_8782 x111_8782)
                       in
                       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool -> int -> X) -> X)) =
                         let x_1625 (i_1220:int) (k_append_cons_x_8799:(bool -> int -> X)) =
                           if i_1220 = 0 then
                             k_append_cons_x_8799 true x_1221
                           else
                             let x_5201 (k_append_cons_x_x_8812:(bool -> int -> X)) =
                               xs_1222 (i_1220 - 1) k_append_cons_x_x_8812
                             in
                             x_5201 (fun (x0_8837:bool) -> fun (x1_8837:int) -> k_append_cons_x_8799 x0_8837 x1_8837)
                         in
                         let
                           x_1785 (ii00_3086:bool) (ii01_3086:int) (ii10_3086:bool) (ii11_3086:int) 
                                 (k_append_cons_x_8846:(bool ->
                                                          bool ->
                                                            r011_8843:int ->
                                                              bool ->
                                                                bool ->
                                                                  r111_8843:int[\r111_8843. r011_8843 = r111_8843] -> X)) =
                           if ii00_3086 = false then
                             if ii10_3086 = false then
                               k_append_cons_x_8846 false true 0 false true 0
                             else
                               xs_1222 ii11_3086
                                 (fun (x0_12369:bool) ->
                                    fun (x1_12369:int) -> k_append_cons_x_8846 false true 0 true x0_12369 x1_12369)
                           else
                             if ii10_3086 = false then
                               x_1625 ii01_3086
                                 (fun (x0_12366:bool) ->
                                    fun (x1_12366:int) -> k_append_cons_x_8846 true x0_12366 x1_12366 false true 0)
                             else
                               let x_5219 (k_append_cons_x_x_8955:(bool -> int -> X)) =
                                 x_1625 ii01_3086 k_append_cons_x_x_8955
                               in
                               x_5219
                                 (fun (x0_8989:bool) ->
                                    fun (x1_8989:int) ->
                                      xs_1222 ii11_3086
                                        (fun (x0_12348:bool) ->
                                           fun (x1_12348:int) ->
                                             k_append_cons_x_8846 true x0_8989 x1_8989 true x0_12348 x1_12348))
                         in
                         x_1785
                       in
                       let
                         x_1788 (ii00_3004:bool) (ii01_3004:int) (ii10_3004:bool) (ii11_3004:int) 
                               (k_append_x_9015:(bool ->
                                                   bool ->
                                                     r011_9014:int ->
                                                       bool ->
                                                         bool -> r111_9014:int[\r111_9014. r011_9014 = r111_9014] -> X)) =
                         if ii00_3004 = false then
                           if ii10_3004 = false then
                             k_append_x_9015 false true 0 false true 0
                           else
                             x_1747 ii11_3004
                               (fun (x0_12414:bool) ->
                                  fun (x1_12414:int) -> k_append_x_9015 false true 0 true x0_12414 x1_12414)
                         else
                           if ii10_3004 = false then
                             xs'_1014 ii01_3004
                               (fun (x0_12411:bool) ->
                                  fun (x1_12411:int) -> k_append_x_9015 true x0_12411 x1_12411 false true 0)
                           else
                             let x_5333 (k_append_x_x_9125:(bool -> int -> bool -> int -> X)) =
                               xs'_x_3867 ii01_3004 ii11_3004 k_append_x_x_9125
                             in
                             x_5333
                               (fun (x00_9149:bool) ->
                                  fun (x01_9149:int) ->
                                    fun (x10_9149:bool) ->
                                      fun (x11_9149:int) ->
                                        k_append_x_9015 true x00_9149 x01_9149 true x10_9149 x11_9149)
                       in
                       let
                         x_5480
                               (k_append_x_9270:((bool ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          bool ->
                                                            int ->
                                                              (bool ->
                                                                 bool ->
                                                                   r011_9267:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9267:
                                                                    int[\r111_9267. r011_9267 = r111_9267] ->
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                         append_1059 x_1788 k_append_x_9270
                       in
                       x_5480
                         (fun (x_10448:(bool ->
                                          int ->
                                            bool ->
                                              int ->
                                                bool ->
                                                  int ->
                                                    (bool ->
                                                       bool ->
                                                         r011_10446:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_10446:
                                                                 int[\r111_10446. r011_10446 = r111_10446] ->
                                                                 bool -> bool -> int -> X) -> X)) ->
                            k_append_7814
                              (let x_1792 (i_2966:int) (k_append_x_9292:(bool -> int -> X)) =
                                 x_10448 true i_2966 false 0 false 0
                                   (fun (p00_12477:bool) ->
                                      fun (p010_12477:bool) ->
                                        fun (p011_12477:int) ->
                                          fun (p10_12477:bool) ->
                                            fun (p110_12477:bool) ->
                                              fun (p111_12477:int) ->
                                                fun (p20_12477:bool) ->
                                                  fun (p210_12477:bool) ->
                                                    fun (p211_12477:int) -> k_append_x_9292 p010_12477 p011_12477)
                               in
                               let x_1793 (i_2956:int) (k_append_x_9339:(bool -> int -> X)) =
                                 x_10448 false 0 true i_2956 false 0
                                   (fun (p00_12496:bool) ->
                                      fun (p010_12496:bool) ->
                                        fun (p011_12496:int) ->
                                          fun (p10_12496:bool) ->
                                            fun (p110_12496:bool) ->
                                              fun (p111_12496:int) ->
                                                fun (p20_12496:bool) ->
                                                  fun (p210_12496:bool) ->
                                                    fun (p211_12496:int) -> k_append_x_9339 p110_12496 p111_12496)
                               in
                               let x_1794 (i_2946:int) (k_append_x_9386:(bool -> int -> X)) =
                                 x_10448 false 0 false 0 true i_2946
                                   (fun (p00_12515:bool) ->
                                      fun (p010_12515:bool) ->
                                        fun (p011_12515:int) ->
                                          fun (p10_12515:bool) ->
                                            fun (p110_12515:bool) ->
                                              fun (p111_12515:int) ->
                                                fun (p20_12515:bool) ->
                                                  fun (p210_12515:bool) ->
                                                    fun (p211_12515:int) -> k_append_x_9386 p210_12515 p211_12515)
                               in
                               let rec
                                 x_x_3919 (x_3881:int) (x_3882:int) 
                                         (k_append_x_x_9434:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_7211
                                         (k_append_x_x_x_9467:(bool ->
                                                                 bool ->
                                                                   r011_9466:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9466:
                                                                    int[\r111_9466. r011_9466 = r111_9466] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_10448 false 0 true x_3881 true x_3882 k_append_x_x_x_9467
                                 in
                                 x_7211
                                   (fun (x00_9479:bool) ->
                                      fun (x010_9479:bool) ->
                                        fun (x011_9479:int) ->
                                          fun (x10_9479:bool) ->
                                            fun (x110_9479:bool) ->
                                              fun (x111_9479:int) ->
                                                fun (x20_9479:bool) ->
                                                  fun (x210_9479:bool) ->
                                                    fun (x211_9479:int) ->
                                                      k_append_x_x_9434 x110_9479 x111_9479 x210_9479 x211_9479)
                               in
                               let
                                 x_1797 (ii00_2929:bool) (ii01_2929:int) (ii10_2929:bool) (ii11_2929:int) 
                                       (k_append_x_9484:(bool ->
                                                           bool ->
                                                             r011_9481:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9481:
                                                                    int[\r111_9481. r011_9481 = r111_9481] -> X)) =
                                 if ii00_2929 = false then
                                   if ii10_2929 = false then
                                     k_append_x_9484 false true 0 false true 0
                                   else
                                     x_1794 ii11_2929
                                       (fun (x0_12553:bool) ->
                                          fun (x1_12553:int) -> k_append_x_9484 false true 0 true x0_12553 x1_12553)
                                 else
                                   if ii10_2929 = false then
                                     x_1793 ii01_2929
                                       (fun (x0_12550:bool) ->
                                          fun (x1_12550:int) -> k_append_x_9484 true x0_12550 x1_12550 false true 0)
                                   else
                                     let x_5604 (k_append_x_x_9594:(bool -> int -> bool -> int -> X)) =
                                       x_x_3919 ii01_2929 ii11_2929 k_append_x_x_9594
                                     in
                                     x_5604
                                       (fun (x00_9618:bool) ->
                                          fun (x01_9618:int) ->
                                            fun (x10_9618:bool) ->
                                              fun (x11_9618:int) ->
                                                k_append_x_9484 true x00_9618 x01_9618 true x10_9618 x11_9618)
                               in
                               let x_1802 (i_2893:int) (k_append_x_9737:(bool -> int -> X)) =
                                 cons_1225 x011_10952 x_1792 true i_2893 false 0
                                   (fun (p00_12607:bool) ->
                                      fun (p010_12607:bool) ->
                                        fun (p011_12607:int) ->
                                          fun (p10_12607:bool) ->
                                            fun (p110_12607:bool) ->
                                              fun (p111_12607:int) -> k_append_x_9737 p010_12607 p011_12607)
                               in
                               let rec
                                 x_x_4018 (x_3992:int) (x_3993:int) 
                                         (k_append_x_x_9777:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_7179
                                         (k_append_x_x_x_9802:(bool ->
                                                                 bool ->
                                                                   r011_9801:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9801:
                                                                    int[\r111_9801. r011_9801 = r111_9801] -> X)) =
                                   cons_1225 x011_10952 x_1792 true x_3992 false 0 k_append_x_x_x_9802
                                 in
                                 x_7179
                                   (fun (x00_9848:bool) ->
                                      fun (x010_9848:bool) ->
                                        fun (x011_9848:int) ->
                                          fun (x10_9848:bool) ->
                                            fun (x110_9848:bool) ->
                                              fun (x111_9848:int) ->
                                                x_1023 true x_3993 false 0
                                                  (fun (p00_12625:bool) ->
                                                     fun (p010_12625:bool) ->
                                                       fun (p011_12625:int) ->
                                                         fun (p10_12625:bool) ->
                                                           fun (p110_12625:bool) ->
                                                             fun (p111_12625:int) ->
                                                               k_append_x_x_9777 x010_9848 x011_9848 p010_12625
                                                                 p011_12625))
                               in
                               let rec
                                 x_x_4063 (x_4037:int) (x_4038:int) 
                                         (k_append_x_x_9854:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_7163
                                         (k_append_x_x_x_9879:(bool ->
                                                                 bool ->
                                                                   r011_9878:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9878:
                                                                    int[\r111_9878. r011_9878 = r111_9878] -> X)) =
                                   cons_1225 x011_10952 x_1792 true x_4037 false 0 k_append_x_x_x_9879
                                 in
                                 x_7163
                                   (fun (x00_9925:bool) ->
                                      fun (x010_9925:bool) ->
                                        fun (x011_9925:int) ->
                                          fun (x10_9925:bool) ->
                                            fun (x110_9925:bool) ->
                                              fun (x111_9925:int) ->
                                                x_1023 false 0 true x_4038
                                                  (fun (p00_12645:bool) ->
                                                     fun (p010_12645:bool) ->
                                                       fun (p011_12645:int) ->
                                                         fun (p10_12645:bool) ->
                                                           fun (p110_12645:bool) ->
                                                             fun (p111_12645:int) ->
                                                               k_append_x_x_9854 x010_9925 x011_9925 p110_12645
                                                                 p111_12645))
                               in
                               let rec
                                 x_x_x_3972 (x_3933:int) (x_3934:int) (x_3935:int) 
                                           (k_append_x_x_x_9932:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                 let
                                   x_7147
                                         (k_append_x_x_x_x_9957:(bool ->
                                                                   bool ->
                                                                    r011_9956:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9956:
                                                                    int[\r111_9956. r011_9956 = r111_9956] -> X)) =
                                   cons_1225 x011_10952 x_1792 true x_3933 false 0 k_append_x_x_x_x_9957
                                 in
                                 x_7147
                                   (fun (x00_10002:bool) ->
                                      fun (x010_10002:bool) ->
                                        fun (x011_10002:int) ->
                                          fun (x10_10002:bool) ->
                                            fun (x110_10002:bool) ->
                                              fun (x111_10002:int) ->
                                                (let
                                                   x_7138
                                                         (k_append_x_x_x_x_9987:(
                                                         bool ->
                                                           bool ->
                                                             r011_9986:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9986:
                                                                    int[\r111_9986. r011_9986 = r111_9986] -> X)) =
                                                   x_1023 true x_3934 true x_3935 k_append_x_x_x_x_9987
                                                 in
                                                 x_7138
                                                   (fun (x00_10001:bool) ->
                                                      fun (x010_10001:bool) ->
                                                        fun (x011_10001:int) ->
                                                          fun (x10_10001:bool) ->
                                                            fun (x110_10001:bool) ->
                                                              fun (x111_10001:int) ->
                                                                k_append_x_x_x_9932 x010_10002 x011_10002 x010_10001
                                                                  x011_10001 x110_10001 x111_10001)))
                               in
                               let
                                 x_1807 (iii00_2861:bool) (iii01_2861:int) (iii10_2861:bool) (iii11_2861:int) 
                                       (iii20_2861:bool) (iii21_2861:int) 
                                       (k_append_x_10046:(bool ->
                                                            bool ->
                                                              r011_10043:int ->
                                                                bool ->
                                                                  bool ->
                                                                    r111_10043:
                                                                    int[\r111_10043. 
                                                                    r011_10043 = r111_10043] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2861 = false then
                                   if iii10_2861 = false then
                                     if iii20_2861 = false then
                                       k_append_x_10046 false true 0 false true 0 false true 0
                                     else
                                       x_1747 iii21_2861
                                         (fun (x0_12810:bool) ->
                                            fun (x1_12810:int) ->
                                              k_append_x_10046 false true 0 false true 0 true x0_12810 x1_12810)
                                   else
                                     if iii20_2861 = false then
                                       x_1746 iii11_2861
                                         (fun (x0_12797:bool) ->
                                            fun (x1_12797:int) ->
                                              k_append_x_10046 false true 0 true x0_12797 x1_12797 false true 0)
                                     else
                                       let x_6067 (k_append_x_x_10198:(bool -> int -> bool -> int -> X)) =
                                         x_x_4108 iii11_2861 iii21_2861 k_append_x_x_10198
                                       in
                                       x_6067
                                         (fun (x00_10236:bool) ->
                                            fun (x01_10236:int) ->
                                              fun (x10_10236:bool) ->
                                                fun (x11_10236:int) ->
                                                  k_append_x_10046 false true 0 true x00_10236 x01_10236 true x10_10236
                                                    x11_10236)
                                 else
                                   if iii10_2861 = false then
                                     if iii20_2861 = false then
                                       x_1802 iii01_2861
                                         (fun (x0_12754:bool) ->
                                            fun (x1_12754:int) ->
                                              k_append_x_10046 true x0_12754 x1_12754 false true 0 false true 0)
                                     else
                                       let x_5983 (k_append_x_x_10300:(bool -> int -> bool -> int -> X)) =
                                         x_x_4063 iii01_2861 iii21_2861 k_append_x_x_10300
                                       in
                                       x_5983
                                         (fun (x00_10338:bool) ->
                                            fun (x01_10338:int) ->
                                              fun (x10_10338:bool) ->
                                                fun (x11_10338:int) ->
                                                  k_append_x_10046 true x00_10338 x01_10338 false true 0 true x10_10338
                                                    x11_10338)
                                   else
                                     if iii20_2861 = false then
                                       let x_5941 (k_append_x_x_10350:(bool -> int -> bool -> int -> X)) =
                                         x_x_4018 iii01_2861 iii11_2861 k_append_x_x_10350
                                       in
                                       x_5941
                                         (fun (x00_10388:bool) ->
                                            fun (x01_10388:int) ->
                                              fun (x10_10388:bool) ->
                                                fun (x11_10388:int) ->
                                                  k_append_x_10046 true x00_10388 x01_10388 true x10_10388 x11_10388
                                                    false true 0)
                                     else
                                       let
                                         x_5909 (k_append_x_x_10397:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                         x_x_x_3972 iii01_2861 iii11_2861 iii21_2861 k_append_x_x_10397
                                       in
                                       x_5909
                                         (fun (x00_10429:bool) ->
                                            fun (x01_10429:int) ->
                                              fun (x10_10429:bool) ->
                                                fun (x11_10429:int) ->
                                                  fun (x20_10429:bool) ->
                                                    fun (x21_10429:int) ->
                                                      k_append_x_10046 true x00_10429 x01_10429 true x10_10429
                                                        x11_10429 true x20_10429 x21_10429)
                               in
                               x_1807))
                     else
                       let x_1610 (k_append_x_10488:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                       x_1610
                         (fun (x_10937:(int -> (bool -> int -> X) -> X)) ->
                            k_append_7814
                              (let
                                 x_1761 (iii00_2432:bool) (iii01_2432:int) (iii10_2432:bool) (iii11_2432:int) 
                                       (iii20_2432:bool) (iii21_2432:int) 
                                       (k_append_x_10496:(bool ->
                                                            bool ->
                                                              r011_10493:int ->
                                                                bool ->
                                                                  bool ->
                                                                    r111_10493:
                                                                    int[\r111_10493. 
                                                                    r011_10493 = r111_10493] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2432 = false then
                                   if iii10_2432 = false then
                                     if iii20_2432 = false then
                                       k_append_x_10496 false true 0 false true 0 false true 0
                                     else
                                       x_1747 iii21_2432
                                         (fun (x0_12270:bool) ->
                                            fun (x1_12270:int) ->
                                              k_append_x_10496 false true 0 false true 0 true x0_12270 x1_12270)
                                   else
                                     if iii20_2432 = false then
                                       x_1746 iii11_2432
                                         (fun (x0_12257:bool) ->
                                            fun (x1_12257:int) ->
                                              k_append_x_10496 false true 0 true x0_12257 x1_12257 false true 0)
                                     else
                                       let x_4980 (k_append_x_x_10648:(bool -> int -> bool -> int -> X)) =
                                         x_x_3820 iii11_2432 iii21_2432 k_append_x_x_10648
                                       in
                                       x_4980
                                         (fun (x00_10686:bool) ->
                                            fun (x01_10686:int) ->
                                              fun (x10_10686:bool) ->
                                                fun (x11_10686:int) ->
                                                  k_append_x_10496 false true 0 true x00_10686 x01_10686 true x10_10686
                                                    x11_10686)
                                 else
                                   if iii10_2432 = false then
                                     if iii20_2432 = false then
                                       x_10937 iii01_2432
                                         (fun (x0_12214:bool) ->
                                            fun (x1_12214:int) ->
                                              k_append_x_10496 true x0_12214 x1_12214 false true 0 false true 0)
                                     else
                                       let x_4897 (k_append_x_x_10749:(bool -> int -> X)) =
                                         x_10937 iii01_2432 k_append_x_x_10749
                                       in
                                       x_4897
                                         (fun (x0_10797:bool) ->
                                            fun (x1_10797:int) ->
                                              x_1747 iii21_2432
                                                (fun (x0_12165:bool) ->
                                                   fun (x1_12165:int) ->
                                                     k_append_x_10496 true x0_10797 x1_10797 false true 0 true x0_12165
                                                       x1_12165))
                                   else
                                     if iii20_2432 = false then
                                       let x_4856 (k_append_x_x_10808:(bool -> int -> X)) =
                                         x_10937 iii01_2432 k_append_x_x_10808
                                       in
                                       x_4856
                                         (fun (x0_10856:bool) ->
                                            fun (x1_10856:int) ->
                                              x_1746 iii11_2432
                                                (fun (x0_12157:bool) ->
                                                   fun (x1_12157:int) ->
                                                     k_append_x_10496 true x0_10856 x1_10856 true x0_12157 x1_12157
                                                       false true 0))
                                     else
                                       let x_4822 (k_append_x_x_10863:(bool -> int -> X)) =
                                         x_10937 iii01_2432 k_append_x_x_10863
                                       in
                                       x_4822
                                         (fun (x0_10918:bool) ->
                                            fun (x1_10918:int) ->
                                              (let x_4832 (k_append_x_x_10875:(bool -> int -> X)) =
                                                 x_1746 iii11_2432 k_append_x_x_10875
                                               in
                                               x_4832
                                                 (fun (x0_10917:bool) ->
                                                    fun (x1_10917:int) ->
                                                      x_1747 iii21_2432
                                                        (fun (x0_12126:bool) ->
                                                           fun (x1_12126:int) ->
                                                             k_append_x_10496 true x0_10918 x1_10918 true x0_10917
                                                               x1_10917 true x0_12126 x1_12126))))
                               in
                               x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_11004:(unit -> X)) =
   let x_6568 (k_main_x_11017:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_11017 in
   x_6568
     (fun (x_11882:(int -> (bool -> int -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_11032:(bool -> int -> X)) = k_main_f_11032 false 0 in
         let
           x_1820 (ix00_2198:bool) (ix01_2198:int) (ix10_2198:bool) (ix11_2198:int) 
                 (k_main_x_11045:(bool ->
                                    bool ->
                                      r011_11044:int ->
                                        bool -> bool -> r111_11044:int[\r111_11044. r011_11044 = r111_11044] -> X)) =
           if ix00_2198 = false then
             if ix10_2198 = false then
               k_main_x_11045 false true 0 false true 0
             else
               f_1479 ix11_2198
                 (fun (x0_13086:bool) -> fun (x1_13086:int) -> k_main_x_11045 false true 0 true x0_13086 x1_13086)
           else
             if ix10_2198 = false then
               x_11882 ix01_2198
                 (fun (x0_13083:bool) -> fun (x1_13083:int) -> k_main_x_11045 true x0_13083 x1_13083 false true 0)
             else
               let x_6580 (k_main_x_x_11154:(bool -> int -> X)) = x_11882 ix01_2198 k_main_x_x_11154 in
               x_6580
                 (fun (x0_11188:bool) ->
                    fun (x1_11188:int) ->
                      f_1479 ix11_2198
                        (fun (x0_13065:bool) ->
                           fun (x1_13065:int) -> k_main_x_11045 true x0_11188 x1_11188 true x0_13065 x1_13065))
         in
         let
           x_6726
                 (k_main_x_11300:((bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               (bool ->
                                                  bool ->
                                                    r011_11297:int ->
                                                      bool ->
                                                        bool ->
                                                          r111_11297:
                                                            int[\r111_11297. r011_11297 = r111_11297] ->
                                                            bool -> bool -> int -> X) -> X) -> X)) =
           append_1059 x_1820 k_main_x_11300
         in
         x_6726
           (fun (x_11862:(bool ->
                            int ->
                              bool ->
                                int ->
                                  bool ->
                                    int ->
                                      (bool ->
                                         bool ->
                                           r011_11860:int ->
                                             bool ->
                                               bool ->
                                                 r111_11860:int[\r111_11860. r011_11860 = r111_11860] ->
                                                   bool -> bool -> int -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_11370:(bool -> int -> X)) =
                 x_11862 false 0 true i_2150 false 0
                   (fun (p00_13168:bool) ->
                      fun (p010_13168:bool) ->
                        fun (p011_13168:int) ->
                          fun (p10_13168:bool) ->
                            fun (p110_13168:bool) ->
                              fun (p111_13168:int) ->
                                fun (p20_13168:bool) ->
                                  fun (p210_13168:bool) -> fun (p211_13168:int) -> k_main_x_11370 p110_13168 p111_13168)
               in
               let x_1826 (i_2140:int) (k_main_x_11419:(bool -> int -> X)) =
                 x_11862 false 0 false 0 true i_2140
                   (fun (p00_13187:bool) ->
                      fun (p010_13187:bool) ->
                        fun (p011_13187:int) ->
                          fun (p10_13187:bool) ->
                            fun (p110_13187:bool) ->
                              fun (p111_13187:int) ->
                                fun (p20_13187:bool) ->
                                  fun (p210_13187:bool) -> fun (p211_13187:int) -> k_main_x_11419 p210_13187 p211_13187)
               in
               let rec x_x_4361 (x_4323:int) (x_4324:int) (k_main_x_x_11468:(bool -> int -> bool -> int -> X)) =
                 let
                   x_7069
                         (k_main_x_x_x_11501:(bool ->
                                                bool ->
                                                  r011_11500:int ->
                                                    bool ->
                                                      bool ->
                                                        r111_11500:int[\r111_11500. r011_11500 = r111_11500] ->
                                                          bool -> bool -> int -> X)) =
                   x_11862 false 0 true x_4323 true x_4324 k_main_x_x_x_11501
                 in
                 x_7069
                   (fun (x00_11513:bool) ->
                      fun (x010_11513:bool) ->
                        fun (x011_11513:int) ->
                          fun (x10_11513:bool) ->
                            fun (x110_11513:bool) ->
                              fun (x111_11513:int) ->
                                fun (x20_11513:bool) ->
                                  fun (x210_11513:bool) ->
                                    fun (x211_11513:int) ->
                                      k_main_x_x_11468 x110_11513 x111_11513 x210_11513 x211_11513)
               in
               let
                 x_1829 (ii00_2123:bool) (ii01_2123:int) (ii10_2123:bool) (ii11_2123:int) 
                       (k_main_x_11521:(bool ->
                                          bool ->
                                            r011_11520:int ->
                                              bool -> bool -> r111_11520:int[\r111_11520. r011_11520 = r111_11520] -> X)) =
                 if ii00_2123 = false then
                   if ii10_2123 = false then
                     k_main_x_11521 false true 0 false true 0
                   else
                     x_1826 ii11_2123
                       (fun (x0_13225:bool) ->
                          fun (x1_13225:int) -> k_main_x_11521 false true 0 true x0_13225 x1_13225)
                 else
                   if ii10_2123 = false then
                     x_1825 ii01_2123
                       (fun (x0_13222:bool) ->
                          fun (x1_13222:int) -> k_main_x_11521 true x0_13222 x1_13222 false true 0)
                   else
                     let x_6850 (k_main_x_x_11631:(bool -> int -> bool -> int -> X)) =
                       x_x_4361 ii01_2123 ii11_2123 k_main_x_x_11631
                     in
                     x_6850
                       (fun (x00_11655:bool) ->
                          fun (x01_11655:int) ->
                            fun (x10_11655:bool) ->
                              fun (x11_11655:int) -> k_main_x_11521 true x00_11655 x01_11655 true x10_11655 x11_11655)
               in
               let
                 x_7042
                       (k_main_x_11781:(bool ->
                                          bool ->
                                            r011_11780:int ->
                                              bool ->
                                                bool ->
                                                  r111_11780:int[\r111_11780. r011_11780 = r111_11780] ->
                                                    bool -> bool -> int -> X)) =
                 x_11862 true i_1016 false 0 false 0 k_main_x_11781
               in
               x_7042
                 (fun (x00_11829:bool) ->
                    fun (x010_11829:bool) ->
                      fun (x011_11829:int) ->
                        fun (x10_11829:bool) ->
                          fun (x110_11829:bool) ->
                            fun (x111_11829:int) ->
                              fun (x20_11829:bool) ->
                                fun (x210_11829:bool) ->
                                  fun (x211_11829:int) ->
                                    (let x_7019 (k_main_x_11793:(bool -> int -> X)) = x_11882 i_1016 k_main_x_11793 in
                                     x_7019
                                       (fun (x0_11828:bool) ->
                                          fun (x1_11828:int) ->
                                            (let n_1504 (k_main_n_11804:(int -> X)) =
                                               if x010_11829 <> false then
                                                 k_main_n_11804 x011_11829
                                               else
                                                 _|_
                                             in
                                             n_1504
                                               (fun (n_11827:int) ->
                                                  (let n_1505 (k_main_n_11812:(int -> X)) =
                                                     if x0_11828 <> false then
                                                       k_main_n_11812 x1_11828
                                                     else
                                                       _|_
                                                   in
                                                   n_1505
                                                     (fun (n_11826:int) ->
                                                        (if n_11827 = n_11826 then
                                                           k_main_11004 ()
                                                         else
                                                           {|fail|} () k_main_11004))))))))))))
 in
 let x_7037 (k_x_11893:(int -> X)) = rand_int_cps () k_x_11893 in
 x_7037
   (fun (x_11938:int) ->
      (let x_7039 (k_x_11905:(int -> X)) = rand_int_cps () k_x_11905 in
       x_7039
         (fun (x_11937:int) ->
            (let x_7041 (k_x_11926:(unit -> X)) = main_1015 x_11938 x_11937 k_x_11926 in
             x_7041 (fun (x_11932:unit) -> {end})))))

BEGIN
