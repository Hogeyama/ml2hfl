MoCHi: Model Checker for Higher-Order Programs
  Build: _ad5b1df (after 2014-07-13 07:40:08 +0900)
  FPAT version: f51d099
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil2_nth.ml -bool-init-empty -debug-module Tupling -disable-rc -color 
           -tupling -gchi -list-option -abs-remove-false

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
      ((false, 0), (if fst (snd xi_3620) = false then
                      (false, (true, 0))
                    else
                      (true, x_1057 (snd (snd xi_3620)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
      ((false, 0), (if fst (snd xi_3620) = false then
                      (false, (true, 0))
                    else
                      (true, x_1057 (snd (snd xi_3620)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
      ((false, 0), (if fst (snd xi_3620) = false then
                      (false, (true, 0))
                    else
                      (true, x_1057 (snd (snd xi_3620)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
      ((false, 0), (if fst (snd xi_3620) = false then
                      (false, (true, 0))
                    else
                      (true, x_1057 (snd (snd xi_3620)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
      ((false, 0), (if fst (snd xi_3620) = false then
                      (false, (true, 0))
                    else
                      (true, x_1057 (snd (snd xi_3620)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
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
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
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
TUPLE: (true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086)))
x_1625
xs_1222
TUPLE: (true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004)))
xs'_1014
x_1747
compose: xs'_1014, let x_3159 =
                     x_1023
                       (let x1_3827 = let x1_3819 = true in
                                      let x2_3820 = x_3817 + 1 in
                                      (x1_3819, x2_3820) in
                        let x2_3828 = let x1_3823 = false in
                                      let x2_3824 = 0 in
                                      (x1_3823, x2_3824) in
                        (x1_3827, x2_3828))
                   in
                   let x_1764 = snd (fst x_3159) in
                   x_1764; x_1747, snd
                                   (snd
                                    (x_1023
                                      (let x1_3839 = let x1_3831 = false in
                                                     let x2_3832 = 0 in
                                                     (x1_3831, x2_3832) in
                                       let x2_3840 = let x1_3835 = true in
                                                     let x2_3836 = x_3818 in
                                                     (x1_3835, x2_3836) in
                                       (x1_3839, x2_3840)))); 
PB: x:xs'_1014
CHECK: x_1764
CHECK: snd (fst x_3159)
CHECK: x_1023
         (let x1_3827 = let x1_3819 = true in
                        let x2_3820 = x_3817 + 1 in
                        (x1_3819, x2_3820) in
          let x2_3828 = let x1_3823 = false in
                        let x2_3824 = 0 in
                        (x1_3823, x2_3824) in
          (x1_3827, x2_3828))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3839 = let x1_3831 = false in
                         let x2_3832 = 0 in
                         (x1_3831, x2_3832) in
           let x2_3840 = let x1_3835 = true in
                         let x2_3836 = x_3818 in
                         (x1_3835, x2_3836) in
           (x1_3839, x2_3840))))
compose_let
xs'_1014:let x_3159 =
           x_1023
             (let x1_3827 = let x1_3819 = true in
                            let x2_3820 = x_3817 + 1 in
                            (x1_3819, x2_3820) in
              let x2_3828 = let x1_3823 = false in
                            let x2_3824 = 0 in
                            (x1_3823, x2_3824) in
              (x1_3827, x2_3828))
         in
         let x_1764 = snd (fst x_3159) in
         x_1764

x_1747:snd
       (snd
        (x_1023
          (let x1_3839 = let x1_3831 = false in
                         let x2_3832 = 0 in
                         (x1_3831, x2_3832) in
           let x2_3840 = let x1_3835 = true in
                         let x2_3836 = x_3818 in
                         (x1_3835, x2_3836) in
           (x1_3839, x2_3840))))

ADD: xs'_x_3843
TUPLE: (true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929)))
x_1793
x_1794
compose: x_1793, snd
                 (#1
                  (x_1791
                    (let x1_3871 = let x1_3859 = false in
                                   let x2_3860 = 0 in
                                   (x1_3859, x2_3860) in
                     let x2_3872 = let x1_3863 = true in
                                   let x2_3864 = x_3857 in
                                   (x1_3863, x2_3864) in
                     let x3_3873 = let x1_3867 = false in
                                   let x2_3868 = 0 in
                                   (x1_3867, x2_3868) in
                     (x1_3871, x2_3872, x3_3873)))); x_1794, snd
                                                             (#2
                                                              (x_1791
                                                                (let x1_3889 =
                                                                   let x1_3877 = false in
                                                                   let x2_3878 = 0 in
                                                                   (x1_3877, x2_3878)
                                                                 in
                                                                 let x2_3890 =
                                                                   let x1_3881 = false in
                                                                   let x2_3882 = 0 in
                                                                   (x1_3881, x2_3882)
                                                                 in
                                                                 let x3_3891 =
                                                                   let x1_3885 = true in
                                                                   let x2_3886 = x_3858 in
                                                                   (x1_3885, x2_3886)
                                                                 in
                                                                 (x1_3889, x2_3890, x3_3891)))); 
PB: x:x_1793
CHECK: snd
       (#1
        (x_1791
          (let x1_3871 = let x1_3859 = false in
                         let x2_3860 = 0 in
                         (x1_3859, x2_3860) in
           let x2_3872 = let x1_3863 = true in
                         let x2_3864 = x_3857 in
                         (x1_3863, x2_3864) in
           let x3_3873 = let x1_3867 = false in
                         let x2_3868 = 0 in
                         (x1_3867, x2_3868) in
           (x1_3871, x2_3872, x3_3873))))
PB: x:x_1794
CHECK: snd
       (#2
        (x_1791
          (let x1_3889 = let x1_3877 = false in
                         let x2_3878 = 0 in
                         (x1_3877, x2_3878) in
           let x2_3890 = let x1_3881 = false in
                         let x2_3882 = 0 in
                         (x1_3881, x2_3882) in
           let x3_3891 = let x1_3885 = true in
                         let x2_3886 = x_3858 in
                         (x1_3885, x2_3886) in
           (x1_3889, x2_3890, x3_3891))))
compose_let
x_1793:snd
       (#1
        (x_1791
          (let x1_3871 = let x1_3859 = false in
                         let x2_3860 = 0 in
                         (x1_3859, x2_3860) in
           let x2_3872 = let x1_3863 = true in
                         let x2_3864 = x_3857 in
                         (x1_3863, x2_3864) in
           let x3_3873 = let x1_3867 = false in
                         let x2_3868 = 0 in
                         (x1_3867, x2_3868) in
           (x1_3871, x2_3872, x3_3873))))

x_1794:snd
       (#2
        (x_1791
          (let x1_3889 = let x1_3877 = false in
                         let x2_3878 = 0 in
                         (x1_3877, x2_3878) in
           let x2_3890 = let x1_3881 = false in
                         let x2_3882 = 0 in
                         (x1_3881, x2_3882) in
           let x3_3891 = let x1_3885 = true in
                         let x2_3886 = x_3858 in
                         (x1_3885, x2_3886) in
           (x1_3889, x2_3890, x3_3891))))

ADD: x_x_3895
TUPLE: (true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861)))
x_1802
x_1746
x_1747
compose: x_1802, snd
                 (fst
                  (x_1801
                    (let x1_3920 = let x1_3912 = true in
                                   let x2_3913 = x_3909 in
                                   (x1_3912, x2_3913) in
                     let x2_3921 = let x1_3916 = false in
                                   let x2_3917 = 0 in
                                   (x1_3916, x2_3917) in
                     (x1_3920, x2_3921)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_3932 =
                                                          let x1_3924 = true in
                                                          let x2_3925 = x_3910 in
                                                          (x1_3924, x2_3925)
                                                        in
                                                        let x2_3933 =
                                                          let x1_3928 = false in
                                                          let x2_3929 = 0 in
                                                          (x1_3928, x2_3929)
                                                        in
                                                        (x1_3932, x2_3933)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_3944 = let x1_3936 = false in
                  let x2_3937 = 0 in
                  (x1_3936, x2_3937) in
    let x2_3945 = let x1_3940 = true in
                  let x2_3941 = x_3911 in
                  (x1_3940, x2_3941) in
    (x1_3944, x2_3945)))); 
PB: x:x_1802
CHECK: snd
       (fst
        (x_1801
          (let x1_3920 = let x1_3912 = true in
                         let x2_3913 = x_3909 in
                         (x1_3912, x2_3913) in
           let x2_3921 = let x1_3916 = false in
                         let x2_3917 = 0 in
                         (x1_3916, x2_3917) in
           (x1_3920, x2_3921))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3932 = let x1_3924 = true in
                         let x2_3925 = x_3910 in
                         (x1_3924, x2_3925) in
           let x2_3933 = let x1_3928 = false in
                         let x2_3929 = 0 in
                         (x1_3928, x2_3929) in
           (x1_3932, x2_3933))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3944 = let x1_3936 = false in
                         let x2_3937 = 0 in
                         (x1_3936, x2_3937) in
           let x2_3945 = let x1_3940 = true in
                         let x2_3941 = x_3911 in
                         (x1_3940, x2_3941) in
           (x1_3944, x2_3945))))
compose_let
x_1802:snd
       (fst
        (x_1801
          (let x1_3920 = let x1_3912 = true in
                         let x2_3913 = x_3909 in
                         (x1_3912, x2_3913) in
           let x2_3921 = let x1_3916 = false in
                         let x2_3917 = 0 in
                         (x1_3916, x2_3917) in
           (x1_3920, x2_3921))))

x_1746:snd
       (fst
        (x_1023
          (let x1_3932 = let x1_3924 = true in
                         let x2_3925 = x_3910 in
                         (x1_3924, x2_3925) in
           let x2_3933 = let x1_3928 = false in
                         let x2_3929 = 0 in
                         (x1_3928, x2_3929) in
           (x1_3932, x2_3933))))

x_1747:snd
       (snd
        (x_1023
          (let x1_3944 = let x1_3936 = false in
                         let x2_3937 = 0 in
                         (x1_3936, x2_3937) in
           let x2_3945 = let x1_3940 = true in
                         let x2_3941 = x_3911 in
                         (x1_3940, x2_3941) in
           (x1_3944, x2_3945))))

ADD: x_x_x_3948
TUPLE: (true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257)))
x_1747
x_1746
x_1747
compose: x_1747, snd
                 (snd
                  (x_1023
                    (let x1_3979 = let x1_3971 = false in
                                   let x2_3972 = 0 in
                                   (x1_3971, x2_3972) in
                     let x2_3980 = let x1_3975 = true in
                                   let x2_3976 = x_3968 in
                                   (x1_3975, x2_3976) in
                     (x1_3979, x2_3980)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_3991 =
                                                          let x1_3983 = true in
                                                          let x2_3984 = x_3969 in
                                                          (x1_3983, x2_3984)
                                                        in
                                                        let x2_3992 =
                                                          let x1_3987 = false in
                                                          let x2_3988 = 0 in
                                                          (x1_3987, x2_3988)
                                                        in
                                                        (x1_3991, x2_3992)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_4003 = let x1_3995 = false in
                  let x2_3996 = 0 in
                  (x1_3995, x2_3996) in
    let x2_4004 = let x1_3999 = true in
                  let x2_4000 = x_3970 in
                  (x1_3999, x2_4000) in
    (x1_4003, x2_4004)))); 
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3979 = let x1_3971 = false in
                         let x2_3972 = 0 in
                         (x1_3971, x2_3972) in
           let x2_3980 = let x1_3975 = true in
                         let x2_3976 = x_3968 in
                         (x1_3975, x2_3976) in
           (x1_3979, x2_3980))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3991 = let x1_3983 = true in
                         let x2_3984 = x_3969 in
                         (x1_3983, x2_3984) in
           let x2_3992 = let x1_3987 = false in
                         let x2_3988 = 0 in
                         (x1_3987, x2_3988) in
           (x1_3991, x2_3992))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_4003 = let x1_3995 = false in
                         let x2_3996 = 0 in
                         (x1_3995, x2_3996) in
           let x2_4004 = let x1_3999 = true in
                         let x2_4000 = x_3970 in
                         (x1_3999, x2_4000) in
           (x1_4003, x2_4004))))
compose_let
x_1747:snd
       (snd
        (x_1023
          (let x1_3979 = let x1_3971 = false in
                         let x2_3972 = 0 in
                         (x1_3971, x2_3972) in
           let x2_3980 = let x1_3975 = true in
                         let x2_3976 = x_3968 in
                         (x1_3975, x2_3976) in
           (x1_3979, x2_3980))))

x_1746:snd
       (fst
        (x_1023
          (let x1_3991 = let x1_3983 = true in
                         let x2_3984 = x_3969 in
                         (x1_3983, x2_3984) in
           let x2_3992 = let x1_3987 = false in
                         let x2_3988 = 0 in
                         (x1_3987, x2_3988) in
           (x1_3991, x2_3992))))

x_1747:snd
       (snd
        (x_1023
          (let x1_4003 = let x1_3995 = false in
                         let x2_3996 = 0 in
                         (x1_3995, x2_3996) in
           let x2_4004 = let x1_3999 = true in
                         let x2_4000 = x_3970 in
                         (x1_3999, x2_4000) in
           (x1_4003, x2_4004))))

ADD: x_x_x_4007
TUPLE: (true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198)))
x_1812
TUPLE: (true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123)))
x_1825
x_1826
compose: x_1825, snd
                 (#1
                  (x_1823
                    (let x1_4041 = let x1_4029 = false in
                                   let x2_4030 = 0 in
                                   (x1_4029, x2_4030) in
                     let x2_4042 = let x1_4033 = true in
                                   let x2_4034 = x_4027 in
                                   (x1_4033, x2_4034) in
                     let x3_4043 = let x1_4037 = false in
                                   let x2_4038 = 0 in
                                   (x1_4037, x2_4038) in
                     (x1_4041, x2_4042, x3_4043)))); x_1826, snd
                                                             (#2
                                                              (x_1823
                                                                (let x1_4059 =
                                                                   let x1_4047 = false in
                                                                   let x2_4048 = 0 in
                                                                   (x1_4047, x2_4048)
                                                                 in
                                                                 let x2_4060 =
                                                                   let x1_4051 = false in
                                                                   let x2_4052 = 0 in
                                                                   (x1_4051, x2_4052)
                                                                 in
                                                                 let x3_4061 =
                                                                   let x1_4055 = true in
                                                                   let x2_4056 = x_4028 in
                                                                   (x1_4055, x2_4056)
                                                                 in
                                                                 (x1_4059, x2_4060, x3_4061)))); 
PB: x:x_1825
CHECK: snd
       (#1
        (x_1823
          (let x1_4041 = let x1_4029 = false in
                         let x2_4030 = 0 in
                         (x1_4029, x2_4030) in
           let x2_4042 = let x1_4033 = true in
                         let x2_4034 = x_4027 in
                         (x1_4033, x2_4034) in
           let x3_4043 = let x1_4037 = false in
                         let x2_4038 = 0 in
                         (x1_4037, x2_4038) in
           (x1_4041, x2_4042, x3_4043))))
PB: x:x_1826
CHECK: snd
       (#2
        (x_1823
          (let x1_4059 = let x1_4047 = false in
                         let x2_4048 = 0 in
                         (x1_4047, x2_4048) in
           let x2_4060 = let x1_4051 = false in
                         let x2_4052 = 0 in
                         (x1_4051, x2_4052) in
           let x3_4061 = let x1_4055 = true in
                         let x2_4056 = x_4028 in
                         (x1_4055, x2_4056) in
           (x1_4059, x2_4060, x3_4061))))
compose_let
x_1825:snd
       (#1
        (x_1823
          (let x1_4041 = let x1_4029 = false in
                         let x2_4030 = 0 in
                         (x1_4029, x2_4030) in
           let x2_4042 = let x1_4033 = true in
                         let x2_4034 = x_4027 in
                         (x1_4033, x2_4034) in
           let x3_4043 = let x1_4037 = false in
                         let x2_4038 = 0 in
                         (x1_4037, x2_4038) in
           (x1_4041, x2_4042, x3_4043))))

x_1826:snd
       (#2
        (x_1823
          (let x1_4059 = let x1_4047 = false in
                         let x2_4048 = 0 in
                         (x1_4047, x2_4048) in
           let x2_4060 = let x1_4051 = false in
                         let x2_4052 = 0 in
                         (x1_4051, x2_4052) in
           let x3_4061 = let x1_4055 = true in
                         let x2_4056 = x_4028 in
                         (x1_4055, x2_4056) in
           (x1_4059, x2_4060, x3_4061))))

ADD: x_x_4065
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
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
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
  let rec x_x_x_4007 x_3968 x_3969 x_3970 =
    let r_4011 =
      snd
      (snd
       (x_1023
         (let x1_3979 = let x1_3971 = false in
                        let x2_3972 = 0 in
                        (x1_3971, x2_3972) in
          let x2_3980 = let x1_3975 = true in
                        let x2_3976 = x_3968 in
                        (x1_3975, x2_3976) in
          (x1_3979, x2_3980))))
    in
    let r_4012 =
      snd
      (fst
       (x_1023
         (let x1_3991 = let x1_3983 = true in
                        let x2_3984 = x_3969 in
                        (x1_3983, x2_3984) in
          let x2_3992 = let x1_3987 = false in
                        let x2_3988 = 0 in
                        (x1_3987, x2_3988) in
          (x1_3991, x2_3992))))
    in
    let r_4013 =
      snd
      (snd
       (x_1023
         (let x1_4003 = let x1_3995 = false in
                        let x2_3996 = 0 in
                        (x1_3995, x2_3996) in
          let x2_4004 = let x1_3999 = true in
                        let x2_4000 = x_3970 in
                        (x1_3999, x2_4000) in
          (x1_4003, x2_4004))))
    in
    (r_4011, r_4012, r_4013)
  in
  let x_3302 = x_1023 ((true, 0), (false, 0)) in
  let x_1748 = snd (fst x_3302) in
  if fst x_1748 = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        ((false, (true, 0)), 
         (if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1746 (snd (#1 iii_3257)))),
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
        else
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            let r_4017 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 r_4017), (true, #1 r_4017), (true, #2 r_4017))
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
      let rec xs'_x_3843 x_3817 x_3818 =
        let x_3159 =
          x_1023
            (let x1_3827 = let x1_3819 = true in
                           let x2_3820 = x_3817 + 1 in
                           (x1_3819, x2_3820) in
             let x2_3828 = let x1_3823 = false in
                           let x2_3824 = 0 in
                           (x1_3823, x2_3824) in
             (x1_3827, x2_3828))
        in
        let x_1764 = snd (fst x_3159) in
        let r_3846 = x_1764 in
        let r_3847 =
          snd
          (snd
           (x_1023
             (let x1_3839 = let x1_3831 = false in
                            let x2_3832 = 0 in
                            (x1_3831, x2_3832) in
              let x2_3840 = let x1_3835 = true in
                            let x2_3836 = x_3818 in
                            (x1_3835, x2_3836) in
              (x1_3839, x2_3840))))
        in
        (r_3846, r_3847)
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
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
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
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            let r_3850 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst r_3850), (true, snd r_3850))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3895 x_3857 x_3858 =
        let r_3898 =
          snd
          (#1
           (x_1791
             (let x1_3871 = let x1_3859 = false in
                            let x2_3860 = 0 in
                            (x1_3859, x2_3860) in
              let x2_3872 = let x1_3863 = true in
                            let x2_3864 = x_3857 in
                            (x1_3863, x2_3864) in
              let x3_3873 = let x1_3867 = false in
                            let x2_3868 = 0 in
                            (x1_3867, x2_3868) in
              (x1_3871, x2_3872, x3_3873))))
        in
        let r_3899 =
          snd
          (#2
           (x_1791
             (let x1_3889 = let x1_3877 = false in
                            let x2_3878 = 0 in
                            (x1_3877, x2_3878) in
              let x2_3890 = let x1_3881 = false in
                            let x2_3882 = 0 in
                            (x1_3881, x2_3882) in
              let x3_3891 = let x1_3885 = true in
                            let x2_3886 = x_3858 in
                            (x1_3885, x2_3886) in
              (x1_3889, x2_3890, x3_3891))))
        in
        (r_3898, r_3899)
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            let r_3902 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst r_3902), (true, snd r_3902))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let rec x_x_x_3948 x_3909 x_3910 x_3911 =
        let r_3952 =
          snd
          (fst
           (x_1801
             (let x1_3920 = let x1_3912 = true in
                            let x2_3913 = x_3909 in
                            (x1_3912, x2_3913) in
              let x2_3921 = let x1_3916 = false in
                            let x2_3917 = 0 in
                            (x1_3916, x2_3917) in
              (x1_3920, x2_3921))))
        in
        let r_3953 =
          snd
          (fst
           (x_1023
             (let x1_3932 = let x1_3924 = true in
                            let x2_3925 = x_3910 in
                            (x1_3924, x2_3925) in
              let x2_3933 = let x1_3928 = false in
                            let x2_3929 = 0 in
                            (x1_3928, x2_3929) in
              (x1_3932, x2_3933))))
        in
        let r_3954 =
          snd
          (snd
           (x_1023
             (let x1_3944 = let x1_3936 = false in
                            let x2_3937 = 0 in
                            (x1_3936, x2_3937) in
              let x2_3945 = let x1_3940 = true in
                            let x2_3941 = x_3911 in
                            (x1_3940, x2_3941) in
              (x1_3944, x2_3945))))
        in
        (r_3952, r_3953, r_3954)
      in
      let x_1803 i_2886 = snd (snd (x_1801 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          ((false, (true, 0)), 
           (if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2861)))),
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
          else
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              let r_3958 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 r_3958), (true, #1 r_3958), (true, #2 r_3958))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          ((false, (true, 0)), 
           (if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1746 (snd (#1 iii_2432)))),
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
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
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
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
  let rec x_x_4065 x_4027 x_4028 =
    let r_4068 =
      snd
      (#1
       (x_1823
         (let x1_4041 = let x1_4029 = false in
                        let x2_4030 = 0 in
                        (x1_4029, x2_4030) in
          let x2_4042 = let x1_4033 = true in
                        let x2_4034 = x_4027 in
                        (x1_4033, x2_4034) in
          let x3_4043 = let x1_4037 = false in
                        let x2_4038 = 0 in
                        (x1_4037, x2_4038) in
          (x1_4041, x2_4042, x3_4043))))
    in
    let r_4069 =
      snd
      (#2
       (x_1823
         (let x1_4059 = let x1_4047 = false in
                        let x2_4048 = 0 in
                        (x1_4047, x2_4048) in
          let x2_4060 = let x1_4051 = false in
                        let x2_4052 = 0 in
                        (x1_4051, x2_4052) in
          let x3_4061 = let x1_4055 = true in
                        let x2_4056 = x_4028 in
                        (x1_4055, x2_4056) in
          (x1_4059, x2_4060, x3_4061))))
    in
    (r_4068, r_4069)
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        let r_4072 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst r_4072), (true, snd r_4072))
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
          let x_4081 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4081
                               else
                                 xs_1114 n_1526 in
          x_4081
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          let x_4157 =
            if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              let x_4163 = xs_1114 (snd (snd ii_3523)) in
              (true, x_4163)
          in
          ((false, (true, 0)), x_4157)
        else
          if fst (snd ii_3523) = false then
            let x_4122 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4122), (false, (true, 0)))
          else
            let x_4099 = x_1592 (snd (fst ii_3523)) in
            let x_4109 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4099), (true, x_4109))
      in
      x_1732
    in
    let x_4194 = make_list_1008 (n_1009 - 1) in
    let x_4196 = rand_int () in
    let x_4197 = cons_1117 x_4196 in
    let x_4198 = x_4197 x_4194 in
    let x_1739 i_3447 = let x_4215 = x_4198 ((true, i_3447), (false, 0)) in
                        snd (fst x_4215) in
    let x_1740 i_3440 = let x_4234 = x_4198 ((false, 0), (true, i_3440)) in
                        snd (snd x_4234) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_4260 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_4260) in
  let x_1747 i_3303 = let x_4279 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_4279) in
  let rec x_x_x_4007 x_3968 x_3969 x_3970 =
    let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
    let x_4307 = x_1023 ((true, x_3969), (false, 0)) in
    let x_4321 = x_1023 ((false, 0), (true, x_3970)) in
    (snd (snd x_4293), snd (fst x_4307), snd (snd x_4321))
  in
  let x_4343 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_4343)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        let x_5559 =
          if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5565 = x_1746 (snd (#1 iii_3257)) in
            (true, x_5565)
        in
        let x_5585 =
          if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5591 = x_1747 (snd (#2 iii_3257)) in
            (true, x_5591)
        in
        ((false, (true, 0)), x_5559, x_5585)
      else
        if fst (#1 iii_3257) = false then
          let x_5497 = x_1747 (snd (#0 iii_3257)) in
          let x_5512 =
            if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              let x_5518 = x_1747 (snd (#2 iii_3257)) in
              (true, x_5518)
          in
          ((true, x_5497), (false, (true, 0)), x_5512)
        else
          if fst (#2 iii_3257) = false then
            let x_5456 = x_1747 (snd (#0 iii_3257)) in
            let x_5466 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5456), (true, x_5466), (false, (true, 0)))
          else
            let x_5424 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5424), (true, #1 x_5424), (true, #2 x_5424))
    in
    x_1811
  else
    if fst (snd (fst x_4343)) <> false then
      let xs'_1014 x_1150 = let x_4572 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_4572) in
      let rec xs'_x_3843 x_3817 x_3818 =
        let x_4587 = x_1023 ((true, x_3817 + 1), (false, 0)) in
        let x_4602 = x_1023 ((false, 0), (true, x_3818)) in
        (snd (fst x_4587), snd (snd x_4602))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4613 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4613
                                 else
                                   xs_1222 n_1544 in
            x_4613
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            let x_4689 =
              if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                let x_4695 = xs_1222 (snd (snd ii_3086)) in
                (true, x_4695)
            in
            ((false, (true, 0)), x_4689)
          else
            if fst (snd ii_3086) = false then
              let x_4654 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4654), (false, (true, 0)))
            else
              let x_4631 = x_1625 (snd (fst ii_3086)) in
              let x_4641 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4631), (true, x_4641))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          let x_4789 =
            if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              let x_4795 = x_1747 (snd (snd ii_3004)) in
              (true, x_4795)
          in
          ((false, (true, 0)), x_4789)
        else
          if fst (snd ii_3004) = false then
            let x_4754 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4754), (false, (true, 0)))
          else
            let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4730), (true, snd x_4730))
      in
      let x_1789 i_2984 = let x_4840 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_4840) in
      let x_1790 i_2977 = let x_4859 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_4859) in
      let x_4862 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_4886 = x_4862 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_4886) in
      let x_1793 i_2956 = let x_4912 = x_4862 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_4912) in
      let x_1794 i_2946 = let x_4938 = x_4862 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_4938) in
      let rec x_x_3895 x_3857 x_3858 =
        let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
        let x_4974 = x_4862 ((false, 0), (false, 0), (true, x_3858)) in
        (snd (#1 x_4956), snd (#2 x_4974))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          let x_5045 =
            if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              let x_5051 = x_1794 (snd (snd ii_2929)) in
              (true, x_5051)
          in
          ((false, (true, 0)), x_5045)
        else
          if fst (snd ii_2929) = false then
            let x_5010 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5010), (false, (true, 0)))
          else
            let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_4986), (true, snd x_4986))
      in
      let x_1798 i_2909 = let x_5096 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_5096) in
      let x_1799 i_2902 = let x_5115 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_5115) in
      let x_5119 = cons_1225 (snd (snd (fst x_4343))) in
      let x_5120 = x_5119 x_1792 in
      let x_1802 i_2893 = let x_5137 = x_5120 ((true, i_2893), (false, 0)) in
                          snd (fst x_5137) in
      let rec x_x_x_3948 x_3909 x_3910 x_3911 =
        let x_5151 = x_5120 ((true, x_3909), (false, 0)) in
        let x_5165 = x_1023 ((true, x_3910), (false, 0)) in
        let x_5179 = x_1023 ((false, 0), (true, x_3911)) in
        (snd (fst x_5151), snd (fst x_5165), snd (snd x_5179))
      in
      let x_1803 i_2886 = let x_5202 = x_5120 ((false, 0), (true, i_2886)) in
                          snd (snd x_5202) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          let x_5349 =
            if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5355 = x_1746 (snd (#1 iii_2861)) in
              (true, x_5355)
          in
          let x_5375 =
            if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5381 = x_1747 (snd (#2 iii_2861)) in
              (true, x_5381)
          in
          ((false, (true, 0)), x_5349, x_5375)
        else
          if fst (#1 iii_2861) = false then
            let x_5287 = x_1802 (snd (#0 iii_2861)) in
            let x_5302 =
              if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                let x_5308 = x_1747 (snd (#2 iii_2861)) in
                (true, x_5308)
            in
            ((true, x_5287), (false, (true, 0)), x_5302)
          else
            if fst (#2 iii_2861) = false then
              let x_5246 = x_1802 (snd (#0 iii_2861)) in
              let x_5256 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5246), (true, x_5256), (false, (true, 0)))
            else
              let x_5214 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5214), (true, #1 x_5214), (true, #2 x_5214))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          let x_4493 =
            if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4499 = x_1746 (snd (#1 iii_2432)) in
              (true, x_4499)
          in
          let x_4519 =
            if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4525 = x_1747 (snd (#2 iii_2432)) in
              (true, x_4525)
          in
          ((false, (true, 0)), x_4493, x_4519)
        else
          if fst (#1 iii_2432) = false then
            let x_4431 = x_1610 (snd (#0 iii_2432)) in
            let x_4446 =
              if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                let x_4452 = x_1747 (snd (#2 iii_2432)) in
                (true, x_4452)
            in
            ((true, x_4431), (false, (true, 0)), x_4446)
          else
            if fst (#2 iii_2432) = false then
              let x_4390 = x_1610 (snd (#0 iii_2432)) in
              let x_4400 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4390), (true, x_4400), (false, (true, 0)))
            else
              let x_4356 = x_1610 (snd (#0 iii_2432)) in
              let x_4366 = x_1746 (snd (#1 iii_2432)) in
              let x_4376 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4356), (true, x_4366), (true, x_4376))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_5625 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      let x_5695 =
        if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          let x_5701 = f_1479 (snd (snd ix_2198)) in
          (true, x_5701)
      in
      ((false, (true, 0)), x_5695)
    else
      if fst (snd ix_2198) = false then
        let x_5660 = x_5625 (snd (fst ix_2198)) in
        ((true, x_5660), (false, (true, 0)))
      else
        let x_5637 = x_5625 (snd (fst ix_2198)) in
        let x_5647 = f_1479 (snd (snd ix_2198)) in
        ((true, x_5637), (true, x_5647))
  in
  let x_1821 i_2178 = let x_5746 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_5746) in
  let x_1822 x_2171 = let x_5765 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_5765) in
  let x_5768 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_5792 = x_5768 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_5792) in
  let x_1825 i_2150 = let x_5818 = x_5768 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_5818) in
  let x_1826 i_2140 = let x_5844 = x_5768 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_5844) in
  let rec x_x_4065 x_4027 x_4028 =
    let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
    let x_5880 = x_5768 ((false, 0), (false, 0), (true, x_4028)) in
    (snd (#1 x_5862), snd (#2 x_5880))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      let x_5951 =
        if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          let x_5957 = x_1826 (snd (snd ii_2123)) in
          (true, x_5957)
      in
      ((false, (true, 0)), x_5951)
    else
      if fst (snd ii_2123) = false then
        let x_5916 = x_1825 (snd (fst ii_2123)) in
        ((true, x_5916), (false, (true, 0)))
      else
        let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_5892), (true, snd x_5892))
  in
  let x_1830 i_2103 = let x_6002 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_6002) in
  let x_1831 i_2096 = let x_6021 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6021) in
  let x_6045 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6046 = x_5625 i_1016 in
  let n_1504 = if fst (snd (#0 x_6045)) <> false then
                 snd (snd (#0 x_6045))
               else
                 _|_ in
  let n_1505 = if fst x_6046 <> false then
                 snd x_6046
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6064 = rand_int () in
let x_6066 = rand_int () in
let x_6067 = main_1015 x_6064 in
let x_6068 = x_6067 x_6066 in
let x_1847 = x_6068 in
()

replace[1]: x_6045
APPS: x_6045 = x_5768 ...0... i_1016 ...
USED: x_6045 = x_5768 ...0... i_1016 ...
MUST: x_6045 = x_5768 ...0... i_1016 ...
USED': x_6045 = x_5768 ...0... i_1016 ...
NEW: x_6069 = x_5768 ((true, i_1016), (false, 0), (false, 0))
replace[1]: x_6021
APPS: x_6021 = x_1829 ...1... i_2096 ...
USED: x_6021 = x_1829 ...1... i_2096 ...
MUST: x_6021 = x_1829 ...1... i_2096 ...
USED': x_6021 = x_1829 ...1... i_2096 ...
replace[1]: x_6002
APPS: x_6002 = x_1829 ...0... i_2103 ...
USED: x_6002 = x_1829 ...0... i_2103 ...
MUST: x_6002 = x_1829 ...0... i_2103 ...
USED': x_6002 = x_1829 ...0... i_2103 ...
NEW: x_6081 = x_1829 ((true, i_2103), (false, 0))
replace[2]: x_5862
APPS: x_5880 = x_5768 ...2... x_4028 ...
APPS: x_5862 = x_5768 ...1... x_4027 ...
USED: x_5880 = x_5768 ...2... x_4028 ...
USED: x_5862 = x_5768 ...1... x_4027 ...
MUST: x_5862 = x_5768 ...1... x_4027 ...
MUST: x_5880 = x_5768 ...2... x_4028 ...
USED': x_5862 = x_5768 ...1... x_4027 ...
USED': x_5880 = x_5768 ...2... x_4028 ...
replace[1]: x_5880
APPS: x_5880 = x_5768 ...2... x_4028 ...
USED: x_5880 = x_5768 ...2... x_4028 ...
MUST: x_5880 = x_5768 ...2... x_4028 ...
USED': x_5880 = x_5768 ...2... x_4028 ...
replace[1]: x_5844
APPS: x_5844 = x_5768 ...2... i_2140 ...
USED: x_5844 = x_5768 ...2... i_2140 ...
MUST: x_5844 = x_5768 ...2... i_2140 ...
USED': x_5844 = x_5768 ...2... i_2140 ...
replace[1]: x_5818
APPS: x_5818 = x_5768 ...1... i_2150 ...
USED: x_5818 = x_5768 ...1... i_2150 ...
MUST: x_5818 = x_5768 ...1... i_2150 ...
USED': x_5818 = x_5768 ...1... i_2150 ...
replace[1]: x_5792
APPS: x_5792 = x_5768 ...0... i_2160 ...
USED: x_5792 = x_5768 ...0... i_2160 ...
MUST: x_5792 = x_5768 ...0... i_2160 ...
USED': x_5792 = x_5768 ...0... i_2160 ...
NEW: x_6093 = x_5768 ((true, i_2160), (false, 0), (false, 0))
replace[1]: x_5765
APPS: x_5765 = x_1820 ...1... x_2171 ...
USED: x_5765 = x_1820 ...1... x_2171 ...
MUST: x_5765 = x_1820 ...1... x_2171 ...
USED': x_5765 = x_1820 ...1... x_2171 ...
replace[1]: x_5746
APPS: x_5746 = x_1820 ...0... i_2178 ...
USED: x_5746 = x_1820 ...0... i_2178 ...
MUST: x_5746 = x_1820 ...0... i_2178 ...
USED': x_5746 = x_1820 ...0... i_2178 ...
NEW: x_6105 = x_1820 ((true, i_2178), (false, 0))
replace[1]: x_5202
APPS: x_5202 = x_5120 ...1... i_2886 ...
USED: x_5202 = x_5120 ...1... i_2886 ...
MUST: x_5202 = x_5120 ...1... i_2886 ...
USED': x_5202 = x_5120 ...1... i_2886 ...
replace[2]: x_5165
APPS: x_5179 = x_1023 ...1... x_3911 ...
APPS: x_5165 = x_1023 ...0... x_3910 ...
APPS: x_4343 = x_1023 ...0... 0 ...
USED: x_5179 = x_1023 ...1... x_3911 ...
USED: x_5165 = x_1023 ...0... x_3910 ...
MUST: x_5165 = x_1023 ...0... x_3910 ...
MUST: x_5179 = x_1023 ...1... x_3911 ...
USED': x_5165 = x_1023 ...0... x_3910 ...
USED': x_5179 = x_1023 ...1... x_3911 ...
NEW: x_6114 = x_1023 ((true, x_3910), (true, x_3911))
replace[1]: x_5151
APPS: x_5151 = x_5120 ...0... x_3909 ...
USED: x_5151 = x_5120 ...0... x_3909 ...
MUST: x_5151 = x_5120 ...0... x_3909 ...
USED': x_5151 = x_5120 ...0... x_3909 ...
NEW: x_6123 = x_5120 ((true, x_3909), (false, 0))
replace[1]: x_5137
APPS: x_5137 = x_5120 ...0... i_2893 ...
USED: x_5137 = x_5120 ...0... i_2893 ...
MUST: x_5137 = x_5120 ...0... i_2893 ...
USED': x_5137 = x_5120 ...0... i_2893 ...
NEW: x_6131 = x_5120 ((true, i_2893), (false, 0))
replace[1]: x_5115
APPS: x_5115 = x_1797 ...1... i_2902 ...
USED: x_5115 = x_1797 ...1... i_2902 ...
MUST: x_5115 = x_1797 ...1... i_2902 ...
USED': x_5115 = x_1797 ...1... i_2902 ...
replace[1]: x_5096
APPS: x_5096 = x_1797 ...0... i_2909 ...
USED: x_5096 = x_1797 ...0... i_2909 ...
MUST: x_5096 = x_1797 ...0... i_2909 ...
USED': x_5096 = x_1797 ...0... i_2909 ...
NEW: x_6140 = x_1797 ((true, i_2909), (false, 0))
replace[2]: x_4956
APPS: x_4974 = x_4862 ...2... x_3858 ...
APPS: x_4956 = x_4862 ...1... x_3857 ...
USED: x_4974 = x_4862 ...2... x_3858 ...
USED: x_4956 = x_4862 ...1... x_3857 ...
MUST: x_4956 = x_4862 ...1... x_3857 ...
MUST: x_4974 = x_4862 ...2... x_3858 ...
USED': x_4956 = x_4862 ...1... x_3857 ...
USED': x_4974 = x_4862 ...2... x_3858 ...
replace[1]: x_4974
APPS: x_4974 = x_4862 ...2... x_3858 ...
USED: x_4974 = x_4862 ...2... x_3858 ...
MUST: x_4974 = x_4862 ...2... x_3858 ...
USED': x_4974 = x_4862 ...2... x_3858 ...
replace[1]: x_4938
APPS: x_4938 = x_4862 ...2... i_2946 ...
USED: x_4938 = x_4862 ...2... i_2946 ...
MUST: x_4938 = x_4862 ...2... i_2946 ...
USED': x_4938 = x_4862 ...2... i_2946 ...
replace[1]: x_4912
APPS: x_4912 = x_4862 ...1... i_2956 ...
USED: x_4912 = x_4862 ...1... i_2956 ...
MUST: x_4912 = x_4862 ...1... i_2956 ...
USED': x_4912 = x_4862 ...1... i_2956 ...
replace[1]: x_4886
APPS: x_4886 = x_4862 ...0... i_2966 ...
USED: x_4886 = x_4862 ...0... i_2966 ...
MUST: x_4886 = x_4862 ...0... i_2966 ...
USED': x_4886 = x_4862 ...0... i_2966 ...
NEW: x_6152 = x_4862 ((true, i_2966), (false, 0), (false, 0))
replace[1]: x_4859
APPS: x_4859 = x_1788 ...1... i_2977 ...
USED: x_4859 = x_1788 ...1... i_2977 ...
MUST: x_4859 = x_1788 ...1... i_2977 ...
USED': x_4859 = x_1788 ...1... i_2977 ...
replace[1]: x_4840
APPS: x_4840 = x_1788 ...0... i_2984 ...
USED: x_4840 = x_1788 ...0... i_2984 ...
MUST: x_4840 = x_1788 ...0... i_2984 ...
USED': x_4840 = x_1788 ...0... i_2984 ...
NEW: x_6164 = x_1788 ((true, i_2984), (false, 0))
replace[2]: x_4587
APPS: x_4602 = x_1023 ...1... x_3818 ...
APPS: x_4587 = x_1023 ...0... x_3817 + 1 ...
APPS: x_4343 = x_1023 ...0... 0 ...
USED: x_4602 = x_1023 ...1... x_3818 ...
USED: x_4587 = x_1023 ...0... x_3817 + 1 ...
MUST: x_4587 = x_1023 ...0... x_3817 + 1 ...
MUST: x_4602 = x_1023 ...1... x_3818 ...
USED': x_4587 = x_1023 ...0... x_3817 + 1 ...
USED': x_4602 = x_1023 ...1... x_3818 ...
NEW: x_6172 = x_1023 ((true, x_3817 + 1), (true, x_3818))
replace[1]: x_4572
APPS: x_4572 = x_1023 ...0... x_1150 + 1 ...
APPS: x_4343 = x_1023 ...0... 0 ...
USED: x_4572 = x_1023 ...0... x_1150 + 1 ...
MUST: x_4572 = x_1023 ...0... x_1150 + 1 ...
USED': x_4572 = x_1023 ...0... x_1150 + 1 ...
NEW: x_6181 = x_1023 ((true, x_1150 + 1), (false, 0))
replace[1]: x_4343
APPS: x_4343 = x_1023 ...0... 0 ...
USED: x_4343 = x_1023 ...0... 0 ...
MUST: x_4343 = x_1023 ...0... 0 ...
USED': x_4343 = x_1023 ...0... 0 ...
NEW: x_6189 = x_1023 ((true, 0), (false, 0))
replace[3]: x_4293
APPS: x_4321 = x_1023 ...1... x_3970 ...
APPS: x_4307 = x_1023 ...0... x_3969 ...
APPS: x_4293 = x_1023 ...1... x_3968 ...
USED: x_4321 = x_1023 ...1... x_3970 ...
USED: x_4307 = x_1023 ...0... x_3969 ...
USED: x_4293 = x_1023 ...1... x_3968 ...
MUST: x_4293 = x_1023 ...1... x_3968 ...
MUST: x_4307 = x_1023 ...0... x_3969 ...
MUST: x_4321 = x_1023 ...1... x_3970 ...
USED': x_4307 = x_1023 ...0... x_3969 ...
USED': x_4293 = x_1023 ...1... x_3968 ...
USED': x_4321 = x_1023 ...1... x_3970 ...
replace[2]: x_4307
APPS: x_4321 = x_1023 ...1... x_3970 ...
APPS: x_4307 = x_1023 ...0... x_3969 ...
USED: x_4321 = x_1023 ...1... x_3970 ...
USED: x_4307 = x_1023 ...0... x_3969 ...
MUST: x_4307 = x_1023 ...0... x_3969 ...
MUST: x_4321 = x_1023 ...1... x_3970 ...
USED': x_4307 = x_1023 ...0... x_3969 ...
USED': x_4321 = x_1023 ...1... x_3970 ...
NEW: x_6198 = x_1023 ((true, x_3969), (true, x_3970))
replace[1]: x_4279
APPS: x_4279 = x_1023 ...1... i_3303 ...
USED: x_4279 = x_1023 ...1... i_3303 ...
MUST: x_4279 = x_1023 ...1... i_3303 ...
USED': x_4279 = x_1023 ...1... i_3303 ...
replace[1]: x_4260
APPS: x_4260 = x_1023 ...0... i_3310 ...
USED: x_4260 = x_1023 ...0... i_3310 ...
MUST: x_4260 = x_1023 ...0... i_3310 ...
USED': x_4260 = x_1023 ...0... i_3310 ...
NEW: x_6208 = x_1023 ((true, i_3310), (false, 0))
replace[1]: x_4234
APPS: x_4234 = x_4198 ...1... i_3440 ...
USED: x_4234 = x_4198 ...1... i_3440 ...
MUST: x_4234 = x_4198 ...1... i_3440 ...
USED': x_4234 = x_4198 ...1... i_3440 ...
replace[1]: x_4215
APPS: x_4215 = x_4198 ...0... i_3447 ...
USED: x_4215 = x_4198 ...0... i_3447 ...
MUST: x_4215 = x_4198 ...0... i_3447 ...
USED': x_4215 = x_4198 ...0... i_3447 ...
NEW: x_6217 = x_4198 ((true, i_3447), (false, 0))
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
          let x_4081 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4081
                               else
                                 xs_1114 n_1526 in
          x_4081
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          let x_4157 =
            if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              let x_4163 = xs_1114 (snd (snd ii_3523)) in
              (true, x_4163)
          in
          ((false, (true, 0)), x_4157)
        else
          if fst (snd ii_3523) = false then
            let x_4122 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4122), (false, (true, 0)))
          else
            let x_4099 = x_1592 (snd (fst ii_3523)) in
            let x_4109 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4099), (true, x_4109))
      in
      x_1732
    in
    let x_4194 = make_list_1008 (n_1009 - 1) in
    let x_4196 = rand_int () in
    let x_4197 = cons_1117 x_4196 in
    let x_4198 = x_4197 x_4194 in
    let x_1739 i_3447 =
      let x_4215 = x_4198 ((true, i_3447), (false, 0)) in
      let x_6217 = x_4198 ((true, i_3447), (false, 0)) in
      snd (fst x_6217)
    in
    let x_1740 i_3440 = let x_4234 = x_4198 ((false, 0), (true, i_3440)) in
                        snd (snd x_4234) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 =
    let x_4260 = x_1023 ((true, i_3310), (false, 0)) in
    let x_6208 = x_1023 ((true, i_3310), (false, 0)) in
    snd (fst x_6208)
  in
  let x_1747 i_3303 = let x_4279 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_4279) in
  let rec x_x_x_4007 x_3968 x_3969 x_3970 =
    let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
    let x_4307 = x_1023 ((true, x_3969), (false, 0)) in
    let x_4321 = x_1023 ((false, 0), (true, x_3970)) in
    let x_6198 = x_1023 ((true, x_3969), (true, x_3970)) in
    (snd (snd x_4293), snd (fst x_6198), snd (snd x_6198))
  in
  let x_4343 = x_1023 ((true, 0), (false, 0)) in
  let x_6189 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6189)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        let x_5559 =
          if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5565 = x_1746 (snd (#1 iii_3257)) in
            (true, x_5565)
        in
        let x_5585 =
          if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5591 = x_1747 (snd (#2 iii_3257)) in
            (true, x_5591)
        in
        ((false, (true, 0)), x_5559, x_5585)
      else
        if fst (#1 iii_3257) = false then
          let x_5497 = x_1747 (snd (#0 iii_3257)) in
          let x_5512 =
            if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              let x_5518 = x_1747 (snd (#2 iii_3257)) in
              (true, x_5518)
          in
          ((true, x_5497), (false, (true, 0)), x_5512)
        else
          if fst (#2 iii_3257) = false then
            let x_5456 = x_1747 (snd (#0 iii_3257)) in
            let x_5466 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5456), (true, x_5466), (false, (true, 0)))
          else
            let x_5424 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5424), (true, #1 x_5424), (true, #2 x_5424))
    in
    x_1811
  else
    if fst (snd (fst x_6189)) <> false then
      let xs'_1014 x_1150 =
        let x_4572 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_6181 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        snd (fst x_6181)
      in
      let rec xs'_x_3843 x_3817 x_3818 =
        let x_4587 = x_1023 ((true, x_3817 + 1), (false, 0)) in
        let x_4602 = x_1023 ((false, 0), (true, x_3818)) in
        let x_6172 = x_1023 ((true, x_3817 + 1), (true, x_3818)) in
        (snd (fst x_6172), snd (snd x_6172))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4613 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4613
                                 else
                                   xs_1222 n_1544 in
            x_4613
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            let x_4689 =
              if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                let x_4695 = xs_1222 (snd (snd ii_3086)) in
                (true, x_4695)
            in
            ((false, (true, 0)), x_4689)
          else
            if fst (snd ii_3086) = false then
              let x_4654 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4654), (false, (true, 0)))
            else
              let x_4631 = x_1625 (snd (fst ii_3086)) in
              let x_4641 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4631), (true, x_4641))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          let x_4789 =
            if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              let x_4795 = x_1747 (snd (snd ii_3004)) in
              (true, x_4795)
          in
          ((false, (true, 0)), x_4789)
        else
          if fst (snd ii_3004) = false then
            let x_4754 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4754), (false, (true, 0)))
          else
            let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4730), (true, snd x_4730))
      in
      let x_1789 i_2984 =
        let x_4840 = x_1788 ((true, i_2984), (false, 0)) in
        let x_6164 = x_1788 ((true, i_2984), (false, 0)) in
        snd (fst x_6164)
      in
      let x_1790 i_2977 = let x_4859 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_4859) in
      let x_4862 = append_1059 x_1788 in
      let x_1792 i_2966 =
        let x_4886 = x_4862 ((true, i_2966), (false, 0), (false, 0)) in
        let x_6152 = x_4862 ((true, i_2966), (false, 0), (false, 0)) in
        snd (#0 x_6152)
      in
      let x_1793 i_2956 = let x_4912 = x_4862 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_4912) in
      let x_1794 i_2946 = let x_4938 = x_4862 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_4938) in
      let rec x_x_3895 x_3857 x_3858 =
        let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
        let x_4974 = x_4862 ((false, 0), (false, 0), (true, x_3858)) in
        (snd (#1 x_4956), snd (#2 x_4974))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          let x_5045 =
            if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              let x_5051 = x_1794 (snd (snd ii_2929)) in
              (true, x_5051)
          in
          ((false, (true, 0)), x_5045)
        else
          if fst (snd ii_2929) = false then
            let x_5010 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5010), (false, (true, 0)))
          else
            let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_4986), (true, snd x_4986))
      in
      let x_1798 i_2909 =
        let x_5096 = x_1797 ((true, i_2909), (false, 0)) in
        let x_6140 = x_1797 ((true, i_2909), (false, 0)) in
        snd (fst x_6140)
      in
      let x_1799 i_2902 = let x_5115 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_5115) in
      let x_5119 = cons_1225 (snd (snd (fst x_6189))) in
      let x_5120 = x_5119 x_1792 in
      let x_1802 i_2893 =
        let x_5137 = x_5120 ((true, i_2893), (false, 0)) in
        let x_6131 = x_5120 ((true, i_2893), (false, 0)) in
        snd (fst x_6131)
      in
      let rec x_x_x_3948 x_3909 x_3910 x_3911 =
        let x_5151 = x_5120 ((true, x_3909), (false, 0)) in
        let x_6123 = x_5120 ((true, x_3909), (false, 0)) in
        let x_5165 = x_1023 ((true, x_3910), (false, 0)) in
        let x_5179 = x_1023 ((false, 0), (true, x_3911)) in
        let x_6114 = x_1023 ((true, x_3910), (true, x_3911)) in
        (snd (fst x_6123), snd (fst x_6114), snd (snd x_6114))
      in
      let x_1803 i_2886 = let x_5202 = x_5120 ((false, 0), (true, i_2886)) in
                          snd (snd x_5202) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          let x_5349 =
            if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5355 = x_1746 (snd (#1 iii_2861)) in
              (true, x_5355)
          in
          let x_5375 =
            if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5381 = x_1747 (snd (#2 iii_2861)) in
              (true, x_5381)
          in
          ((false, (true, 0)), x_5349, x_5375)
        else
          if fst (#1 iii_2861) = false then
            let x_5287 = x_1802 (snd (#0 iii_2861)) in
            let x_5302 =
              if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                let x_5308 = x_1747 (snd (#2 iii_2861)) in
                (true, x_5308)
            in
            ((true, x_5287), (false, (true, 0)), x_5302)
          else
            if fst (#2 iii_2861) = false then
              let x_5246 = x_1802 (snd (#0 iii_2861)) in
              let x_5256 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5246), (true, x_5256), (false, (true, 0)))
            else
              let x_5214 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5214), (true, #1 x_5214), (true, #2 x_5214))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          let x_4493 =
            if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4499 = x_1746 (snd (#1 iii_2432)) in
              (true, x_4499)
          in
          let x_4519 =
            if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4525 = x_1747 (snd (#2 iii_2432)) in
              (true, x_4525)
          in
          ((false, (true, 0)), x_4493, x_4519)
        else
          if fst (#1 iii_2432) = false then
            let x_4431 = x_1610 (snd (#0 iii_2432)) in
            let x_4446 =
              if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                let x_4452 = x_1747 (snd (#2 iii_2432)) in
                (true, x_4452)
            in
            ((true, x_4431), (false, (true, 0)), x_4446)
          else
            if fst (#2 iii_2432) = false then
              let x_4390 = x_1610 (snd (#0 iii_2432)) in
              let x_4400 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4390), (true, x_4400), (false, (true, 0)))
            else
              let x_4356 = x_1610 (snd (#0 iii_2432)) in
              let x_4366 = x_1746 (snd (#1 iii_2432)) in
              let x_4376 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4356), (true, x_4366), (true, x_4376))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_5625 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      let x_5695 =
        if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          let x_5701 = f_1479 (snd (snd ix_2198)) in
          (true, x_5701)
      in
      ((false, (true, 0)), x_5695)
    else
      if fst (snd ix_2198) = false then
        let x_5660 = x_5625 (snd (fst ix_2198)) in
        ((true, x_5660), (false, (true, 0)))
      else
        let x_5637 = x_5625 (snd (fst ix_2198)) in
        let x_5647 = f_1479 (snd (snd ix_2198)) in
        ((true, x_5637), (true, x_5647))
  in
  let x_1821 i_2178 =
    let x_5746 = x_1820 ((true, i_2178), (false, 0)) in
    let x_6105 = x_1820 ((true, i_2178), (false, 0)) in
    snd (fst x_6105)
  in
  let x_1822 x_2171 = let x_5765 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_5765) in
  let x_5768 = append_1059 x_1820 in
  let x_1824 i_2160 =
    let x_5792 = x_5768 ((true, i_2160), (false, 0), (false, 0)) in
    let x_6093 = x_5768 ((true, i_2160), (false, 0), (false, 0)) in
    snd (#0 x_6093)
  in
  let x_1825 i_2150 = let x_5818 = x_5768 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_5818) in
  let x_1826 i_2140 = let x_5844 = x_5768 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_5844) in
  let rec x_x_4065 x_4027 x_4028 =
    let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
    let x_5880 = x_5768 ((false, 0), (false, 0), (true, x_4028)) in
    (snd (#1 x_5862), snd (#2 x_5880))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      let x_5951 =
        if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          let x_5957 = x_1826 (snd (snd ii_2123)) in
          (true, x_5957)
      in
      ((false, (true, 0)), x_5951)
    else
      if fst (snd ii_2123) = false then
        let x_5916 = x_1825 (snd (fst ii_2123)) in
        ((true, x_5916), (false, (true, 0)))
      else
        let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_5892), (true, snd x_5892))
  in
  let x_1830 i_2103 =
    let x_6002 = x_1829 ((true, i_2103), (false, 0)) in
    let x_6081 = x_1829 ((true, i_2103), (false, 0)) in
    snd (fst x_6081)
  in
  let x_1831 i_2096 = let x_6021 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6021) in
  let x_6045 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6069 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6046 = x_5625 i_1016 in
  let n_1504 = if fst (snd (#0 x_6069)) <> false then
                 snd (snd (#0 x_6069))
               else
                 _|_ in
  let n_1505 = if fst x_6046 <> false then
                 snd x_6046
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6064 = rand_int () in
let x_6066 = rand_int () in
let x_6067 = main_1015 x_6064 in
let x_6068 = x_6067 x_6066 in
let x_1847 = x_6068 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_6064; is_subsumed: 
rand_int (), x_6067 x_6066; is_subsumed: main_1015 x_6064, x_6068; is_subsumed: 
rand_int (), x_6068; is_subsumed: rand_int (), x_6068; is_subsumed: make_list_1008 n_1017, 
append_1059 x_1820; is_subsumed: make_list_1008 n_1017, x_5768 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_5768 ((true, i_1016), (false, 0), (false, 0)), x_5768 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_5768 ((true, i_1016), (false, 0), (false, 0)); x_6045 |-> x_6069
is_subsumed: x_5768 ((true, i_1016), (false, 0), (false, 0)), x_5625 i_1016; is_subsumed: 
x_5768 ((true, i_1016), (false, 0), (false, 0)), x_5625 i_1016; is_subsumed: 
append_1059 x_1820, x_5625 i_1016; is_subsumed: x_5625 i_1016, if fst (snd (#0 x_6069)) <> false then
                                                                 snd (snd (#0 x_6069))
                                                               else
                                                                 _|_; is_subsumed: 
x_5768 ((true, i_1016), (false, 0), (false, 0)), if fst (snd (#0 x_6069)) <> false then
                                                   snd (snd (#0 x_6069))
                                                 else
                                                   _|_; is_subsumed: 
append_1059 x_1820, if fst (snd (#0 x_6069)) <> false then
                      snd (snd (#0 x_6069))
                    else
                      _|_; is_subsumed: make_list_1008 n_1017, if fst (snd (#0 x_6069)) <> false then
                                                                 snd (snd (#0 x_6069))
                                                               else
                                                                 _|_; is_subsumed: 
if fst (snd (#0 x_6069)) <> false then
  snd (snd (#0 x_6069))
else
  _|_, if fst x_6046 <> false then
         snd x_6046
       else
         _|_; is_subsumed: x_5768 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_6046 <> false then
  snd x_6046
else
  _|_; is_subsumed: x_5768 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_6046 <> false then
  snd x_6046
else
  _|_; is_subsumed: append_1059 x_1820, if fst x_6046 <> false then
                                          snd x_6046
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_6046 <> false then
  snd x_6046
else
  _|_; is_subsumed: append_1059 x_1820, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
x_1829 ((true, i_2103), (false, 0)), x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); x_6002 |-> x_6081
is_subsumed: append_1059 x_1820, x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
make_list_1008 n_1017, x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
append_1059 x_1820, x_1825 (snd (fst ii_2123)); is_subsumed: make_list_1008 n_1017, 
x_1825 (snd (fst ii_2123)); is_subsumed: append_1059 x_1820, if fst (snd ii_2123) = false then
                                                               (false, (true, 0))
                                                             else
                                                               let x_5957 = x_1826 (snd (snd ii_2123)) in
                                                               (true, x_5957); is_subsumed: 
make_list_1008 n_1017, if fst (snd ii_2123) = false then
                         (false, (true, 0))
                       else
                         let x_5957 = x_1826 (snd (snd ii_2123)) in
                         (true, x_5957); is_subsumed: make_list_1008 n_1017, 
x_5768 ((false, 0), (true, x_4027), (false, 0)); is_subsumed: x_5768 ((false, 0), (true, x_4027), (false, 0)), 
x_5768 ((false, 0), (false, 0), (true, x_4028)); is_subsumed: make_list_1008 n_1017, 
x_5768 ((false, 0), (false, 0), (true, x_4028)); is_subsumed: make_list_1008 n_1017, 
x_5768 ((false, 0), (false, 0), (true, i_2140)); is_subsumed: make_list_1008 n_1017, 
x_5768 ((false, 0), (true, i_2150), (false, 0)); is_subsumed: make_list_1008 n_1017, 
x_5768 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: x_5768 ((true, i_2160), (false, 0), (false, 0)), 
x_5768 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: make_list_1008 n_1017, 
x_5768 ((true, i_2160), (false, 0), (false, 0)); x_5792 |-> x_6093
is_subsumed: make_list_1008 n_1017, x_1820 ((false, 0), (true, x_2171)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
x_1820 ((true, i_2178), (false, 0)), x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); x_5746 |-> x_6105
is_subsumed: x_5625 (snd (fst ix_2198)), f_1479 (snd (snd ix_2198)); is_subsumed: 
make_list_1008 n_1017, f_1479 (snd (snd ix_2198)); is_subsumed: make_list_1008 n_1017, 
if fst (snd ix_2198) = false then
  (false, (true, 0))
else
  let x_5701 = f_1479 (snd (snd ix_2198)) in
  (true, x_5701); is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, 0), (false, 0)); x_4343 |-> x_6189
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
if fst (#2 iii_2432) = false then
  (false, (true, 0))
else
  let x_4452 = x_1747 (snd (#2 iii_2432)) in
  (true, x_4452); is_subsumed: _|_, if fst (#2 iii_2432) = false then
                                      (false, (true, 0))
                                    else
                                      let x_4452 = x_1747 (snd (#2 iii_2432)) in
                                      (true, x_4452); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_2432) = false then
  (false, (true, 0))
else
  let x_4452 = x_1747 (snd (#2 iii_2432)) in
  (true, x_4452); is_subsumed: x_1023 ((true, 0), (false, 0)), if fst (#2 iii_2432) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_4452 = x_1747 (snd (#2 iii_2432)) in
                                                                 (true, x_4452); is_subsumed: _|_, 
if fst (#1 iii_2432) = false then
  (false, (true, 0))
else
  let x_4499 = x_1746 (snd (#1 iii_2432)) in
  (true, x_4499); is_subsumed: x_1023 ((true, 0), (false, 0)), if fst (#1 iii_2432) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_4499 = x_1746 (snd (#1 iii_2432)) in
                                                                 (true, x_4499); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (#1 iii_2432) = false then
                                  (false, (true, 0))
                                else
                                  let x_4499 = x_1746 (snd (#1 iii_2432)) in
                                  (true, x_4499); is_subsumed: if fst (#1 iii_2432) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_4499 = x_1746 (snd (#1 iii_2432)) in
                                                                 (true, x_4499), 
if fst (#2 iii_2432) = false then
  (false, (true, 0))
else
  let x_4525 = x_1747 (snd (#2 iii_2432)) in
  (true, x_4525); is_subsumed: _|_, if fst (#2 iii_2432) = false then
                                      (false, (true, 0))
                                    else
                                      let x_4525 = x_1747 (snd (#2 iii_2432)) in
                                      (true, x_4525); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_2432) = false then
  (false, (true, 0))
else
  let x_4525 = x_1747 (snd (#2 iii_2432)) in
  (true, x_4525); is_subsumed: x_1023 ((true, 0), (false, 0)), if fst (#2 iii_2432) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_4525 = x_1747 (snd (#2 iii_2432)) in
                                                                 (true, x_4525); is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: x_1023 ((true, 0), (false, 0)), 
append_1059 x_1788; is_subsumed: append_1059 x_1788, cons_1225 (snd (snd (fst x_6189))); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd (snd (fst x_6189))); is_subsumed: 
append_1059 x_1788, x_5119 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_5119 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), x_5119 x_1792; is_subsumed: 
x_5119 x_1792, x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_5119 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1802 (snd (#0 iii_2861)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5119 x_1792, x_1746 (snd (#1 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
x_1746 (snd (#1 iii_2861)); is_subsumed: append_1059 x_1788, x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5119 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1802 (snd (#0 iii_2861)), if fst (#2 iii_2861) = false then
                              (false, (true, 0))
                            else
                              let x_5308 = x_1747 (snd (#2 iii_2861)) in
                              (true, x_5308); is_subsumed: x_5119 x_1792, 
if fst (#2 iii_2861) = false then
  (false, (true, 0))
else
  let x_5308 = x_1747 (snd (#2 iii_2861)) in
  (true, x_5308); is_subsumed: cons_1225 (snd (snd (fst x_6189))), if 
                                                                   fst (#2 iii_2861) = false then
                                                                     
                                                                   (false, (true, 0))
                                                                   else
                                                                     
                                                                   let x_5308 = x_1747 (snd (#2 iii_2861)) in
                                                                   (true, x_5308); is_subsumed: 
append_1059 x_1788, if fst (#2 iii_2861) = false then
                      (false, (true, 0))
                    else
                      let x_5308 = x_1747 (snd (#2 iii_2861)) in
                      (true, x_5308); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_2861) = false then
  (false, (true, 0))
else
  let x_5308 = x_1747 (snd (#2 iii_2861)) in
  (true, x_5308); is_subsumed: x_1023 ((true, 0), (false, 0)), if fst (#2 iii_2861) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_5308 = x_1747 (snd (#2 iii_2861)) in
                                                                 (true, x_5308); is_subsumed: 
x_5119 x_1792, if fst (#1 iii_2861) = false then
                 (false, (true, 0))
               else
                 let x_5355 = x_1746 (snd (#1 iii_2861)) in
                 (true, x_5355); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
if fst (#1 iii_2861) = false then
  (false, (true, 0))
else
  let x_5355 = x_1746 (snd (#1 iii_2861)) in
  (true, x_5355); is_subsumed: append_1059 x_1788, if fst (#1 iii_2861) = false then
                                                     (false, (true, 0))
                                                   else
                                                     let x_5355 = x_1746 (snd (#1 iii_2861)) in
                                                     (true, x_5355); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (#1 iii_2861) = false then
                                  (false, (true, 0))
                                else
                                  let x_5355 = x_1746 (snd (#1 iii_2861)) in
                                  (true, x_5355); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#1 iii_2861) = false then
  (false, (true, 0))
else
  let x_5355 = x_1746 (snd (#1 iii_2861)) in
  (true, x_5355); is_subsumed: if fst (#1 iii_2861) = false then
                                 (false, (true, 0))
                               else
                                 let x_5355 = x_1746 (snd (#1 iii_2861)) in
                                 (true, x_5355), if fst (#2 iii_2861) = false then
                                                   (false, (true, 0))
                                                 else
                                                   let x_5381 = x_1747 (snd (#2 iii_2861)) in
                                                   (true, x_5381); is_subsumed: 
x_5119 x_1792, if fst (#2 iii_2861) = false then
                 (false, (true, 0))
               else
                 let x_5381 = x_1747 (snd (#2 iii_2861)) in
                 (true, x_5381); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
if fst (#2 iii_2861) = false then
  (false, (true, 0))
else
  let x_5381 = x_1747 (snd (#2 iii_2861)) in
  (true, x_5381); is_subsumed: append_1059 x_1788, if fst (#2 iii_2861) = false then
                                                     (false, (true, 0))
                                                   else
                                                     let x_5381 = x_1747 (snd (#2 iii_2861)) in
                                                     (true, x_5381); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (#2 iii_2861) = false then
                                  (false, (true, 0))
                                else
                                  let x_5381 = x_1747 (snd (#2 iii_2861)) in
                                  (true, x_5381); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_2861) = false then
  (false, (true, 0))
else
  let x_5381 = x_1747 (snd (#2 iii_2861)) in
  (true, x_5381); is_subsumed: cons_1225 (snd (snd (fst x_6189))), x_5120 ((false, 0), (true, i_2886)); is_subsumed: 
append_1059 x_1788, x_5120 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((false, 0), (true, i_2886)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
x_5120 ((true, x_3909), (false, 0)), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, x_3909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, x_3909), (false, 0)); x_5151 |-> x_6123
is_subsumed: x_5120 ((true, x_3909), (false, 0)), x_1023 ((true, x_3910), (false, 0)); is_subsumed: 
x_5120 ((true, x_3909), (false, 0)), x_1023 ((true, x_3910), (false, 0)); is_subsumed: 
x_5119 x_1792, x_1023 ((true, x_3910), (false, 0)); is_subsumed: cons_1225 (snd (snd (fst x_6189))), 
x_1023 ((true, x_3910), (false, 0)); is_subsumed: append_1059 x_1788, 
x_1023 ((true, x_3910), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3910), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3910), (false, 0)); is_subsumed: x_1023 ((true, x_3910), (false, 0)), 
x_1023 ((false, 0), (true, x_3911)); is_subsumed: x_5120 ((true, x_3909), (false, 0)), 
x_1023 ((false, 0), (true, x_3911)); is_subsumed: x_5120 ((true, x_3909), (false, 0)), 
x_1023 ((false, 0), (true, x_3911)); is_subsumed: x_5119 x_1792, x_1023 ((false, 0), (true, x_3911)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_1023 ((false, 0), (true, x_3911)); is_subsumed: 
append_1059 x_1788, x_1023 ((false, 0), (true, x_3911)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3911)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3911)); is_subsumed: 
x_1023 ((false, 0), (true, x_3911)), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_1023 ((true, x_3910), (false, 0)), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_5120 ((true, x_3909), (false, 0)), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_5120 ((true, x_3909), (false, 0)), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_5119 x_1792, x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
append_1059 x_1788, x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3910), (true, x_3911)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3910), (true, x_3911)); x_5179 |-> x_6114
x_5165 |-> x_6114
is_subsumed: cons_1225 (snd (snd (fst x_6189))), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
x_5120 ((true, i_2893), (false, 0)), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_6189))), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5120 ((true, i_2893), (false, 0)); x_5137 |-> x_6131
is_subsumed: append_1059 x_1788, x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1797 ((true, i_2909), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); x_5096 |-> x_6140
is_subsumed: append_1059 x_1788, x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
append_1059 x_1788, x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: append_1059 x_1788, if fst (snd ii_2929) = false then
                                                               (false, (true, 0))
                                                             else
                                                               let x_5051 = x_1794 (snd (snd ii_2929)) in
                                                               (true, x_5051); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (snd ii_2929) = false then
                                  (false, (true, 0))
                                else
                                  let x_5051 = x_1794 (snd (snd ii_2929)) in
                                  (true, x_5051); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (snd ii_2929) = false then
  (false, (true, 0))
else
  let x_5051 = x_1794 (snd (snd ii_2929)) in
  (true, x_5051); is_subsumed: x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (true, x_3857), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (true, x_3857), (false, 0)); is_subsumed: 
x_4862 ((false, 0), (true, x_3857), (false, 0)), x_4862 ((false, 0), (false, 0), (true, x_3858)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (false, 0), (true, x_3858)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (false, 0), (true, x_3858)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_4862 ((true, i_2966), (false, 0), (false, 0)), x_4862 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4862 ((true, i_2966), (false, 0), (false, 0)); x_4886 |-> x_6152
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1788 ((true, i_2984), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); x_4840 |-> x_6164
is_subsumed: x_1023 ((true, 0), (false, 0)), xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (snd ii_3004) = false then
                                  (false, (true, 0))
                                else
                                  let x_4795 = x_1747 (snd (snd ii_3004)) in
                                  (true, x_4795); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (snd ii_3004) = false then
  (false, (true, 0))
else
  let x_4795 = x_1747 (snd (snd ii_3004)) in
  (true, x_4795); is_subsumed: x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1625 (snd (fst ii_3086)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (snd (snd ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1625 (snd (fst ii_3086)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (snd ii_3086) = false then
                                  (false, (true, 0))
                                else
                                  let x_4695 = xs_1222 (snd (snd ii_3086)) in
                                  (true, x_4695); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (snd ii_3086) = false then
  (false, (true, 0))
else
  let x_4695 = xs_1222 (snd (snd ii_3086)) in
  (true, x_4695); is_subsumed: x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs_1222 (i_1220 - 1); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3817 + 1), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3817 + 1), (false, 0)); is_subsumed: x_1023 ((true, x_3817 + 1), (false, 0)), 
x_1023 ((false, 0), (true, x_3818)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((false, 0), (true, x_3818)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((false, 0), (true, x_3818)); is_subsumed: x_1023 ((false, 0), (true, x_3818)), 
x_1023 ((true, x_3817 + 1), (true, x_3818)); is_subsumed: x_1023 ((true, x_3817 + 1), (false, 0)), 
x_1023 ((true, x_3817 + 1), (true, x_3818)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3817 + 1), (true, x_3818)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3817 + 1), (true, x_3818)); x_4602 |-> x_6172
x_4587 |-> x_6172
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_1150 + 1), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); x_4572 |-> x_6181
is_subsumed: x_1023 ((true, 0), (false, 0)), x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1747 (snd (#0 iii_3257)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1747 (snd (#0 iii_3257)), if fst (#2 iii_3257) = false then
                              (false, (true, 0))
                            else
                              let x_5518 = x_1747 (snd (#2 iii_3257)) in
                              (true, x_5518); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_3257) = false then
  (false, (true, 0))
else
  let x_5518 = x_1747 (snd (#2 iii_3257)) in
  (true, x_5518); is_subsumed: x_1023 ((true, 0), (false, 0)), if fst (#2 iii_3257) = false then
                                                                 (false, (true, 0))
                                                               else
                                                                 let x_5518 = x_1747 (snd (#2 iii_3257)) in
                                                                 (true, x_5518); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (#1 iii_3257) = false then
                                  (false, (true, 0))
                                else
                                  let x_5565 = x_1746 (snd (#1 iii_3257)) in
                                  (true, x_5565); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#1 iii_3257) = false then
  (false, (true, 0))
else
  let x_5565 = x_1746 (snd (#1 iii_3257)) in
  (true, x_5565); is_subsumed: if fst (#1 iii_3257) = false then
                                 (false, (true, 0))
                               else
                                 let x_5565 = x_1746 (snd (#1 iii_3257)) in
                                 (true, x_5565), if fst (#2 iii_3257) = false then
                                                   (false, (true, 0))
                                                 else
                                                   let x_5591 = x_1747 (snd (#2 iii_3257)) in
                                                   (true, x_5591); is_subsumed: 
x_1023 ((true, 0), (false, 0)), if fst (#2 iii_3257) = false then
                                  (false, (true, 0))
                                else
                                  let x_5591 = x_1747 (snd (#2 iii_3257)) in
                                  (true, x_5591); is_subsumed: x_1023 ((true, 0), (false, 0)), 
if fst (#2 iii_3257) = false then
  (false, (true, 0))
else
  let x_5591 = x_1747 (snd (#2 iii_3257)) in
  (true, x_5591); is_subsumed: x_1023 ((false, 0), (true, x_3968)), x_1023 ((true, x_3969), (false, 0)); is_subsumed: 
x_1023 ((true, x_3969), (false, 0)), x_1023 ((false, 0), (true, x_3970)); is_subsumed: 
x_1023 ((false, 0), (true, x_3968)), x_1023 ((false, 0), (true, x_3970)); is_subsumed: 
x_1023 ((false, 0), (true, x_3970)), x_1023 ((true, x_3969), (true, x_3970)); is_subsumed: 
x_1023 ((true, x_3969), (false, 0)), x_1023 ((true, x_3969), (true, x_3970)); is_subsumed: 
x_1023 ((false, 0), (true, x_3968)), x_1023 ((true, x_3969), (true, x_3970)); x_4321 |-> x_6198
x_4307 |-> x_6198
is_subsumed: x_1023 ((true, i_3310), (false, 0)), x_1023 ((true, i_3310), (false, 0)); x_4260 |-> x_6208
is_subsumed: make_list_1008 (n_1009 - 1), rand_int (); is_subsumed: make_list_1008 (n_1009 - 1), 
cons_1117 x_4196; is_subsumed: rand_int (), x_4197 x_4194; is_subsumed: 
cons_1117 x_4196, x_4198 ((false, 0), (true, i_3440)); is_subsumed: rand_int (), 
x_4198 ((false, 0), (true, i_3440)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4198 ((false, 0), (true, i_3440)); is_subsumed: cons_1117 x_4196, x_4198 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4198 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4198 ((true, i_3447), (false, 0)); is_subsumed: x_4198 ((true, i_3447), (false, 0)), 
x_4198 ((true, i_3447), (false, 0)); is_subsumed: cons_1117 x_4196, x_4198 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4198 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4198 ((true, i_3447), (false, 0)); x_4215 |-> x_6217
is_subsumed: x_1592 (snd (fst ii_3523)), xs_1114 (snd (snd ii_3523)); 
x_4215; x_4260; x_4321; x_4307; x_4572; x_4587; x_4602; x_4840; x_4886; x_5096; x_5137; x_5165; x_5179; x_5151; 
x_4343; x_6045; x_6002; x_5792; x_5746
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
          let x_4081 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4081
                               else
                                 xs_1114 n_1526 in
          x_4081
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          let x_4157 =
            if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              let x_4163 = xs_1114 (snd (snd ii_3523)) in
              (true, x_4163)
          in
          ((false, (true, 0)), x_4157)
        else
          if fst (snd ii_3523) = false then
            let x_4122 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4122), (false, (true, 0)))
          else
            let x_4099 = x_1592 (snd (fst ii_3523)) in
            let x_4109 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4099), (true, x_4109))
      in
      x_1732
    in
    let x_4194 = make_list_1008 (n_1009 - 1) in
    let x_4196 = rand_int () in
    let x_4197 = cons_1117 x_4196 in
    let x_4198 = x_4197 x_4194 in
    let x_1739 i_3447 = let x_6217 = x_4198 ((true, i_3447), (false, 0)) in
                        snd (fst x_6217) in
    let x_1740 i_3440 = let x_4234 = x_4198 ((false, 0), (true, i_3440)) in
                        snd (snd x_4234) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_6208 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_6208) in
  let x_1747 i_3303 = let x_4279 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_4279) in
  let rec x_x_x_4007 x_3968 x_3969 x_3970 =
    let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
    let x_6198 = x_1023 ((true, x_3969), (true, x_3970)) in
    (snd (snd x_4293), snd (fst x_6198), snd (snd x_6198))
  in
  let x_6189 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6189)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        let x_5559 =
          if fst (#1 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5565 = x_1746 (snd (#1 iii_3257)) in
            (true, x_5565)
        in
        let x_5585 =
          if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            let x_5591 = x_1747 (snd (#2 iii_3257)) in
            (true, x_5591)
        in
        ((false, (true, 0)), x_5559, x_5585)
      else
        if fst (#1 iii_3257) = false then
          let x_5497 = x_1747 (snd (#0 iii_3257)) in
          let x_5512 =
            if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              let x_5518 = x_1747 (snd (#2 iii_3257)) in
              (true, x_5518)
          in
          ((true, x_5497), (false, (true, 0)), x_5512)
        else
          if fst (#2 iii_3257) = false then
            let x_5456 = x_1747 (snd (#0 iii_3257)) in
            let x_5466 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5456), (true, x_5466), (false, (true, 0)))
          else
            let x_5424 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5424), (true, #1 x_5424), (true, #2 x_5424))
    in
    x_1811
  else
    if fst (snd (fst x_6189)) <> false then
      let xs'_1014 x_1150 = let x_6181 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_6181) in
      let rec xs'_x_3843 x_3817 x_3818 =
        let x_6172 = x_1023 ((true, x_3817 + 1), (true, x_3818)) in
        (snd (fst x_6172), snd (snd x_6172))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4613 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4613
                                 else
                                   xs_1222 n_1544 in
            x_4613
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            let x_4689 =
              if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                let x_4695 = xs_1222 (snd (snd ii_3086)) in
                (true, x_4695)
            in
            ((false, (true, 0)), x_4689)
          else
            if fst (snd ii_3086) = false then
              let x_4654 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4654), (false, (true, 0)))
            else
              let x_4631 = x_1625 (snd (fst ii_3086)) in
              let x_4641 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4631), (true, x_4641))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          let x_4789 =
            if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              let x_4795 = x_1747 (snd (snd ii_3004)) in
              (true, x_4795)
          in
          ((false, (true, 0)), x_4789)
        else
          if fst (snd ii_3004) = false then
            let x_4754 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4754), (false, (true, 0)))
          else
            let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4730), (true, snd x_4730))
      in
      let x_1789 i_2984 = let x_6164 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_6164) in
      let x_1790 i_2977 = let x_4859 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_4859) in
      let x_4862 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_6152 = x_4862 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_6152) in
      let x_1793 i_2956 = let x_4912 = x_4862 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_4912) in
      let x_1794 i_2946 = let x_4938 = x_4862 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_4938) in
      let rec x_x_3895 x_3857 x_3858 =
        let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
        let x_4974 = x_4862 ((false, 0), (false, 0), (true, x_3858)) in
        (snd (#1 x_4956), snd (#2 x_4974))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          let x_5045 =
            if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              let x_5051 = x_1794 (snd (snd ii_2929)) in
              (true, x_5051)
          in
          ((false, (true, 0)), x_5045)
        else
          if fst (snd ii_2929) = false then
            let x_5010 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5010), (false, (true, 0)))
          else
            let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_4986), (true, snd x_4986))
      in
      let x_1798 i_2909 = let x_6140 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_6140) in
      let x_1799 i_2902 = let x_5115 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_5115) in
      let x_5119 = cons_1225 (snd (snd (fst x_6189))) in
      let x_5120 = x_5119 x_1792 in
      let x_1802 i_2893 = let x_6131 = x_5120 ((true, i_2893), (false, 0)) in
                          snd (fst x_6131) in
      let rec x_x_x_3948 x_3909 x_3910 x_3911 =
        let x_6123 = x_5120 ((true, x_3909), (false, 0)) in
        let x_6114 = x_1023 ((true, x_3910), (true, x_3911)) in
        (snd (fst x_6123), snd (fst x_6114), snd (snd x_6114))
      in
      let x_1803 i_2886 = let x_5202 = x_5120 ((false, 0), (true, i_2886)) in
                          snd (snd x_5202) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          let x_5349 =
            if fst (#1 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5355 = x_1746 (snd (#1 iii_2861)) in
              (true, x_5355)
          in
          let x_5375 =
            if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              let x_5381 = x_1747 (snd (#2 iii_2861)) in
              (true, x_5381)
          in
          ((false, (true, 0)), x_5349, x_5375)
        else
          if fst (#1 iii_2861) = false then
            let x_5287 = x_1802 (snd (#0 iii_2861)) in
            let x_5302 =
              if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                let x_5308 = x_1747 (snd (#2 iii_2861)) in
                (true, x_5308)
            in
            ((true, x_5287), (false, (true, 0)), x_5302)
          else
            if fst (#2 iii_2861) = false then
              let x_5246 = x_1802 (snd (#0 iii_2861)) in
              let x_5256 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5246), (true, x_5256), (false, (true, 0)))
            else
              let x_5214 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5214), (true, #1 x_5214), (true, #2 x_5214))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          let x_4493 =
            if fst (#1 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4499 = x_1746 (snd (#1 iii_2432)) in
              (true, x_4499)
          in
          let x_4519 =
            if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              let x_4525 = x_1747 (snd (#2 iii_2432)) in
              (true, x_4525)
          in
          ((false, (true, 0)), x_4493, x_4519)
        else
          if fst (#1 iii_2432) = false then
            let x_4431 = x_1610 (snd (#0 iii_2432)) in
            let x_4446 =
              if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                let x_4452 = x_1747 (snd (#2 iii_2432)) in
                (true, x_4452)
            in
            ((true, x_4431), (false, (true, 0)), x_4446)
          else
            if fst (#2 iii_2432) = false then
              let x_4390 = x_1610 (snd (#0 iii_2432)) in
              let x_4400 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4390), (true, x_4400), (false, (true, 0)))
            else
              let x_4356 = x_1610 (snd (#0 iii_2432)) in
              let x_4366 = x_1746 (snd (#1 iii_2432)) in
              let x_4376 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4356), (true, x_4366), (true, x_4376))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_5625 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      let x_5695 =
        if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          let x_5701 = f_1479 (snd (snd ix_2198)) in
          (true, x_5701)
      in
      ((false, (true, 0)), x_5695)
    else
      if fst (snd ix_2198) = false then
        let x_5660 = x_5625 (snd (fst ix_2198)) in
        ((true, x_5660), (false, (true, 0)))
      else
        let x_5637 = x_5625 (snd (fst ix_2198)) in
        let x_5647 = f_1479 (snd (snd ix_2198)) in
        ((true, x_5637), (true, x_5647))
  in
  let x_1821 i_2178 = let x_6105 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_6105) in
  let x_1822 x_2171 = let x_5765 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_5765) in
  let x_5768 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_6093 = x_5768 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_6093) in
  let x_1825 i_2150 = let x_5818 = x_5768 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_5818) in
  let x_1826 i_2140 = let x_5844 = x_5768 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_5844) in
  let rec x_x_4065 x_4027 x_4028 =
    let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
    let x_5880 = x_5768 ((false, 0), (false, 0), (true, x_4028)) in
    (snd (#1 x_5862), snd (#2 x_5880))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      let x_5951 =
        if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          let x_5957 = x_1826 (snd (snd ii_2123)) in
          (true, x_5957)
      in
      ((false, (true, 0)), x_5951)
    else
      if fst (snd ii_2123) = false then
        let x_5916 = x_1825 (snd (fst ii_2123)) in
        ((true, x_5916), (false, (true, 0)))
      else
        let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_5892), (true, snd x_5892))
  in
  let x_1830 i_2103 = let x_6081 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_6081) in
  let x_1831 i_2096 = let x_6021 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6021) in
  let x_6069 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6046 = x_5625 i_1016 in
  let n_1504 = if fst (snd (#0 x_6069)) <> false then
                 snd (snd (#0 x_6069))
               else
                 _|_ in
  let n_1505 = if fst x_6046 <> false then
                 snd x_6046
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6064 = rand_int () in
let x_6066 = rand_int () in
let x_6067 = main_1015 x_6064 in
let x_6068 = x_6067 x_6066 in
let x_1847 = x_6068 in
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
          let x_4081 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4081
                               else
                                 xs_1114 n_1526 in
          x_4081
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_3523) = false then
              (false, (true, 0))
            else
              (true, xs_1114 (snd (snd ii_3523)))))
        else
          if fst (snd ii_3523) = false then
            ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
          else
            let x_4099 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4099), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_4194 = make_list_1008 (n_1009 - 1) in
    let x_4196 = rand_int () in
    let x_4197 = cons_1117 x_4196 in
    let x_4198 = x_4197 x_4194 in
    let x_1739 i_3447 = snd (fst (x_4198 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_4198 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let rec x_x_x_4007 x_3968 x_3969 x_3970 =
    let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
    let x_6198 = x_1023 ((true, x_3969), (true, x_3970)) in
    (snd (snd x_4293), snd (fst x_6198), snd (snd x_6198))
  in
  let x_6189 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6189)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        let x_5559 = if fst (#1 iii_3257) = false then
                       (false, (true, 0))
                     else
                       (true, x_1746 (snd (#1 iii_3257))) in
        ((false, (true, 0)), x_5559, 
         (if fst (#2 iii_3257) = false then
            (false, (true, 0))
          else
            (true, x_1747 (snd (#2 iii_3257)))))
      else
        if fst (#1 iii_3257) = false then
          let x_5497 = x_1747 (snd (#0 iii_3257)) in
          ((true, x_5497), (false, (true, 0)), 
           (if fst (#2 iii_3257) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_3257)))))
        else
          if fst (#2 iii_3257) = false then
            let x_5456 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5456), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            let x_5424 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5424), (true, #1 x_5424), (true, #2 x_5424))
    in
    x_1811
  else
    if fst (snd (fst x_6189)) <> false then
      let xs'_1014 x_1150 = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
      let rec xs'_x_3843 x_3817 x_3818 =
        let x_6172 = x_1023 ((true, x_3817 + 1), (true, x_3818)) in
        (snd (fst x_6172), snd (snd x_6172))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4613 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4613
                                 else
                                   xs_1222 n_1544 in
            x_4613
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            ((false, (true, 0)), 
             (if fst (snd ii_3086) = false then
                (false, (true, 0))
              else
                (true, xs_1222 (snd (snd ii_3086)))))
          else
            if fst (snd ii_3086) = false then
              ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
            else
              let x_4631 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4631), (true, xs_1222 (snd (snd ii_3086))))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_3004) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (snd ii_3004)))))
        else
          if fst (snd ii_3004) = false then
            ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
          else
            let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4730), (true, snd x_4730))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_4862 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_4862 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_4862 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_4862 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3895 x_3857 x_3858 =
        let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
        (snd (#1 x_4956), snd (#2 (x_4862 ((false, 0), (false, 0), (true, x_3858)))))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          ((false, (true, 0)), 
           (if fst (snd ii_2929) = false then
              (false, (true, 0))
            else
              (true, x_1794 (snd (snd ii_2929)))))
        else
          if fst (snd ii_2929) = false then
            ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
          else
            let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_4986), (true, snd x_4986))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_5119 = cons_1225 (snd (snd (fst x_6189))) in
      let x_5120 = x_5119 x_1792 in
      let x_1802 i_2893 = snd (fst (x_5120 ((true, i_2893), (false, 0)))) in
      let rec x_x_x_3948 x_3909 x_3910 x_3911 =
        let x_6123 = x_5120 ((true, x_3909), (false, 0)) in
        let x_6114 = x_1023 ((true, x_3910), (true, x_3911)) in
        (snd (fst x_6123), snd (fst x_6114), snd (snd x_6114))
      in
      let x_1803 i_2886 = snd (snd (x_5120 ((false, 0), (true, i_2886)))) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          let x_5349 = if fst (#1 iii_2861) = false then
                         (false, (true, 0))
                       else
                         (true, x_1746 (snd (#1 iii_2861))) in
          ((false, (true, 0)), x_5349, 
           (if fst (#2 iii_2861) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2861)))))
        else
          if fst (#1 iii_2861) = false then
            let x_5287 = x_1802 (snd (#0 iii_2861)) in
            ((true, x_5287), (false, (true, 0)), 
             (if fst (#2 iii_2861) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2861)))))
          else
            if fst (#2 iii_2861) = false then
              let x_5246 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5246), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              let x_5214 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5214), (true, #1 x_5214), (true, #2 x_5214))
      in
      x_1807
    else
      let x_1610 = _|_ in
      let x_1761 iii_2432 =
        if fst (#0 iii_2432) = false then
          let x_4493 = if fst (#1 iii_2432) = false then
                         (false, (true, 0))
                       else
                         (true, x_1746 (snd (#1 iii_2432))) in
          ((false, (true, 0)), x_4493, 
           (if fst (#2 iii_2432) = false then
              (false, (true, 0))
            else
              (true, x_1747 (snd (#2 iii_2432)))))
        else
          if fst (#1 iii_2432) = false then
            let x_4431 = x_1610 (snd (#0 iii_2432)) in
            ((true, x_4431), (false, (true, 0)), 
             (if fst (#2 iii_2432) = false then
                (false, (true, 0))
              else
                (true, x_1747 (snd (#2 iii_2432)))))
          else
            if fst (#2 iii_2432) = false then
              let x_4390 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4390), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              let x_4356 = x_1610 (snd (#0 iii_2432)) in
              let x_4366 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4356), (true, x_4366), (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_5625 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      ((false, (true, 0)), 
       (if fst (snd ix_2198) = false then
          (false, (true, 0))
        else
          (true, f_1479 (snd (snd ix_2198)))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_5625 (snd (fst ix_2198))), (false, (true, 0)))
      else
        let x_5637 = x_5625 (snd (fst ix_2198)) in
        ((true, x_5637), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_5768 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_5768 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_5768 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_5768 ((false, 0), (false, 0), (true, i_2140)))) in
  let rec x_x_4065 x_4027 x_4028 =
    let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
    (snd (#1 x_5862), snd (#2 (x_5768 ((false, 0), (false, 0), (true, x_4028)))))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      ((false, (true, 0)), 
       (if fst (snd ii_2123) = false then
          (false, (true, 0))
        else
          (true, x_1826 (snd (snd ii_2123)))))
    else
      if fst (snd ii_2123) = false then
        ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
      else
        let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_5892), (true, snd x_5892))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_6069 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6046 = x_5625 i_1016 in
  let n_1504 = if fst (snd (#0 x_6069)) <> false then
                 snd (snd (#0 x_6069))
               else
                 _|_ in
  let n_1505 = if fst x_6046 <> false then
                 snd x_6046
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6064 = rand_int () in
let x_6066 = rand_int () in
let x_6067 = main_1015 x_6064 in
let x_6068 = x_6067 x_6066 in
let x_1847 = x_6068 in
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
           let x_4081 = xs_1114 (i_1112 - 1) in
           let xs_1525 (n_1526:int) = if n_1526 = i_1112 - 1 then
                                        x_4081
                                      else
                                        xs_1114 n_1526 in
           x_4081
       in
       let x_1732 (ii_3523:((bool * int) * (bool * int))) =
         if fst (fst ii_3523) = false then
           ((false, (true, 0)), 
            (if fst (snd ii_3523) = false then
               (false, (true, 0))
             else
               (true, xs_1114 (snd (snd ii_3523)))))
         else
           if fst (snd ii_3523) = false then
             ((true, x_1592 (snd (fst ii_3523))), (false, (true, 0)))
           else
             let x_4099 = x_1592 (snd (fst ii_3523)) in
             ((true, x_4099), (true, xs_1114 (snd (snd ii_3523))))
       in
       x_1732
     in
     let x_4194 = make_list_1008 (n_1009 - 1) in
     let x_4196 = rand_int () in
     let x_4197 = cons_1117 x_4196 in
     let x_4198 = x_4197 x_4194 in
     let x_1739 (i_3447:int) = snd (fst (x_4198 ((true, i_3447), (false, 0)))) in
     let x_1740 (i_3440:int) = snd (snd (x_4198 ((false, 0), (true, i_3440)))) in
     x_1739
 in
 let rec append_1059 (x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1746 (i_3310:int) = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
   let x_1747 (i_3303:int) = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
   let rec x_x_x_4007 (x_3968:int) (x_3969:int) (x_3970:int) =
     let x_4293 = x_1023 ((false, 0), (true, x_3968)) in
     let x_6198 = x_1023 ((true, x_3969), (true, x_3970)) in
     (snd (snd x_4293), snd (fst x_6198), snd (snd x_6198))
   in
   let x_6189 = x_1023 ((true, 0), (false, 0)) in
   if fst (snd (fst x_6189)) = false then
     let x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) =
       if fst (#0 iii_3257) = false then
         let x_5559 = if fst (#1 iii_3257) = false then
                        (false, (true, 0))
                      else
                        (true, x_1746 (snd (#1 iii_3257))) in
         ((false, (true, 0)), x_5559, 
          (if fst (#2 iii_3257) = false then
             (false, (true, 0))
           else
             (true, x_1747 (snd (#2 iii_3257)))))
       else
         if fst (#1 iii_3257) = false then
           let x_5497 = x_1747 (snd (#0 iii_3257)) in
           ((true, x_5497), (false, (true, 0)), 
            (if fst (#2 iii_3257) = false then
               (false, (true, 0))
             else
               (true, x_1747 (snd (#2 iii_3257)))))
         else
           if fst (#2 iii_3257) = false then
             let x_5456 = x_1747 (snd (#0 iii_3257)) in
             ((true, x_5456), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
           else
             let x_5424 = x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
             ((true, #0 x_5424), (true, #1 x_5424), (true, #2 x_5424))
     in
     x_1811
   else
     if fst (snd (fst x_6189)) <> false then
       let xs'_1014 (x_1150:int) = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
       let rec xs'_x_3843 (x_3817:int) (x_3818:int) =
         let x_6172 = x_1023 ((true, x_3817 + 1), (true, x_3818)) in
         (snd (fst x_6172), snd (snd x_6172))
       in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
         let x_1625 (i_1220:int) =
           if i_1220 = 0 then
             (true, x_1221)
           else
             let x_4613 = xs_1222 (i_1220 - 1) in
             let xs_1543 (n_1544:int) = if n_1544 = i_1220 - 1 then
                                          x_4613
                                        else
                                          xs_1222 n_1544 in
             x_4613
         in
         let x_1785 (ii_3086:((bool * int) * (bool * int))) =
           if fst (fst ii_3086) = false then
             ((false, (true, 0)), 
              (if fst (snd ii_3086) = false then
                 (false, (true, 0))
               else
                 (true, xs_1222 (snd (snd ii_3086)))))
           else
             if fst (snd ii_3086) = false then
               ((true, x_1625 (snd (fst ii_3086))), (false, (true, 0)))
             else
               let x_4631 = x_1625 (snd (fst ii_3086)) in
               ((true, x_4631), (true, xs_1222 (snd (snd ii_3086))))
         in
         x_1785
       in
       let x_1788 (ii_3004:((bool * int) * (bool * int))) =
         if fst (fst ii_3004) = false then
           ((false, (true, 0)), 
            (if fst (snd ii_3004) = false then
               (false, (true, 0))
             else
               (true, x_1747 (snd (snd ii_3004)))))
         else
           if fst (snd ii_3004) = false then
             ((true, xs'_1014 (snd (fst ii_3004))), (false, (true, 0)))
           else
             let x_4730 = xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) in
             ((true, fst x_4730), (true, snd x_4730))
       in
       let x_1789 (i_2984:int) = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
       let x_1790 (i_2977:int) = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
       let x_4862 = append_1059 x_1788 in
       let x_1792 (i_2966:int) = snd (#0 (x_4862 ((true, i_2966), (false, 0), (false, 0)))) in
       let x_1793 (i_2956:int) = snd (#1 (x_4862 ((false, 0), (true, i_2956), (false, 0)))) in
       let x_1794 (i_2946:int) = snd (#2 (x_4862 ((false, 0), (false, 0), (true, i_2946)))) in
       let rec x_x_3895 (x_3857:int) (x_3858:int) =
         let x_4956 = x_4862 ((false, 0), (true, x_3857), (false, 0)) in
         (snd (#1 x_4956), snd (#2 (x_4862 ((false, 0), (false, 0), (true, x_3858)))))
       in
       let x_1797 (ii_2929:((bool * int) * (bool * int))) =
         if fst (fst ii_2929) = false then
           ((false, (true, 0)), 
            (if fst (snd ii_2929) = false then
               (false, (true, 0))
             else
               (true, x_1794 (snd (snd ii_2929)))))
         else
           if fst (snd ii_2929) = false then
             ((true, x_1793 (snd (fst ii_2929))), (false, (true, 0)))
           else
             let x_4986 = x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) in
             ((true, fst x_4986), (true, snd x_4986))
       in
       let x_1798 (i_2909:int) = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
       let x_1799 (i_2902:int) = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
       let x_5119 = cons_1225 (snd (snd (fst x_6189))) in
       let x_5120 = x_5119 x_1792 in
       let x_1802 (i_2893:int) = snd (fst (x_5120 ((true, i_2893), (false, 0)))) in
       let rec x_x_x_3948 (x_3909:int) (x_3910:int) (x_3911:int) =
         let x_6123 = x_5120 ((true, x_3909), (false, 0)) in
         let x_6114 = x_1023 ((true, x_3910), (true, x_3911)) in
         (snd (fst x_6123), snd (fst x_6114), snd (snd x_6114))
       in
       let x_1803 (i_2886:int) = snd (snd (x_5120 ((false, 0), (true, i_2886)))) in
       let x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_2861) = false then
           let x_5349 =
             if fst (#1 iii_2861) = false then
               (false, (true, 0))
             else
               (true, x_1746 (snd (#1 iii_2861)))
           in
           ((false, (true, 0)), x_5349, 
            (if fst (#2 iii_2861) = false then
               (false, (true, 0))
             else
               (true, x_1747 (snd (#2 iii_2861)))))
         else
           if fst (#1 iii_2861) = false then
             let x_5287 = x_1802 (snd (#0 iii_2861)) in
             ((true, x_5287), (false, (true, 0)), 
              (if fst (#2 iii_2861) = false then
                 (false, (true, 0))
               else
                 (true, x_1747 (snd (#2 iii_2861)))))
           else
             if fst (#2 iii_2861) = false then
               let x_5246 = x_1802 (snd (#0 iii_2861)) in
               ((true, x_5246), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
             else
               let x_5214 = x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
               ((true, #0 x_5214), (true, #1 x_5214), (true, #2 x_5214))
       in
       x_1807
     else
       let x_1610 = _|_ in
       let x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_2432) = false then
           let x_4493 =
             if fst (#1 iii_2432) = false then
               (false, (true, 0))
             else
               (true, x_1746 (snd (#1 iii_2432)))
           in
           ((false, (true, 0)), x_4493, 
            (if fst (#2 iii_2432) = false then
               (false, (true, 0))
             else
               (true, x_1747 (snd (#2 iii_2432)))))
         else
           if fst (#1 iii_2432) = false then
             let x_4431 = x_1610 (snd (#0 iii_2432)) in
             ((true, x_4431), (false, (true, 0)), 
              (if fst (#2 iii_2432) = false then
                 (false, (true, 0))
               else
                 (true, x_1747 (snd (#2 iii_2432)))))
           else
             if fst (#2 iii_2432) = false then
               let x_4390 = x_1610 (snd (#0 iii_2432)) in
               ((true, x_4390), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
             else
               let x_4356 = x_1610 (snd (#0 iii_2432)) in
               let x_4366 = x_1746 (snd (#1 iii_2432)) in
               ((true, x_4356), (true, x_4366), (true, x_1747 (snd (#2 iii_2432))))
       in
       x_1761
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_5625 = make_list_1008 n_1017 in
   let f_1479 (x_1329:int) = (false, 0) in
   let x_1820 (ix_2198:((bool * int) * (bool * int))) =
     if fst (fst ix_2198) = false then
       ((false, (true, 0)), 
        (if fst (snd ix_2198) = false then
           (false, (true, 0))
         else
           (true, f_1479 (snd (snd ix_2198)))))
     else
       if fst (snd ix_2198) = false then
         ((true, x_5625 (snd (fst ix_2198))), (false, (true, 0)))
       else
         let x_5637 = x_5625 (snd (fst ix_2198)) in
         ((true, x_5637), (true, f_1479 (snd (snd ix_2198))))
   in
   let x_1821 (i_2178:int) = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
   let x_1822 (x_2171:int) = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
   let x_5768 = append_1059 x_1820 in
   let x_1824 (i_2160:int) = snd (#0 (x_5768 ((true, i_2160), (false, 0), (false, 0)))) in
   let x_1825 (i_2150:int) = snd (#1 (x_5768 ((false, 0), (true, i_2150), (false, 0)))) in
   let x_1826 (i_2140:int) = snd (#2 (x_5768 ((false, 0), (false, 0), (true, i_2140)))) in
   let rec x_x_4065 (x_4027:int) (x_4028:int) =
     let x_5862 = x_5768 ((false, 0), (true, x_4027), (false, 0)) in
     (snd (#1 x_5862), snd (#2 (x_5768 ((false, 0), (false, 0), (true, x_4028)))))
   in
   let x_1829 (ii_2123:((bool * int) * (bool * int))) =
     if fst (fst ii_2123) = false then
       ((false, (true, 0)), 
        (if fst (snd ii_2123) = false then
           (false, (true, 0))
         else
           (true, x_1826 (snd (snd ii_2123)))))
     else
       if fst (snd ii_2123) = false then
         ((true, x_1825 (snd (fst ii_2123))), (false, (true, 0)))
       else
         let x_5892 = x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) in
         ((true, fst x_5892), (true, snd x_5892))
   in
   let x_1830 (i_2103:int) = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
   let x_1831 (i_2096:int) = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
   let x_6069 = x_5768 ((true, i_1016), (false, 0), (false, 0)) in
   let x_6046 = x_5625 i_1016 in
   let n_1504 = if fst (snd (#0 x_6069)) <> false then
                  snd (snd (#0 x_6069))
                else
                  _|_ in
   let n_1505 = if fst x_6046 <> false then
                  snd x_6046
                else
                  _|_ in
   if n_1504 = n_1505 then
     ()
   else
     {fail} ()
 in
 let x_6064 = rand_int () in
 let x_6066 = rand_int () in
 let x_6067 = main_1015 x_6064 in
 let x_6068 = x_6067 x_6066 in
 let x_1847 = x_6068 in
 ()

CPS:
 let rec make_list_1008 (n_1009:int) (k_make_list_6237:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6237 (fun (x_1123:int) -> fun (k_make_list_6239:((bool * int) -> X)) -> k_make_list_6239 (false, 0))
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> ((bool * int) -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_6258:((bool * int) -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_6258 (true, x_1113)
         else
           let x_4081 (k_make_list_cons_x_x_6271:((bool * int) -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_6271 in
           x_4081 (fun (x_6296:(bool * int)) -> k_make_list_cons_x_6258 x_6296)
       in
       let
         x_1732 (ii_3523:((bool * int) * (bool * int))) 
               (k_make_list_cons_x_6305:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
         if fst (fst ii_3523) = false then
           let k_make_list_cons_x_6354 (x_9947:(bool * (bool * int))) =
             k_make_list_cons_x_6305 ((false, (true, 0)), x_9947)
           in
           if fst (snd ii_3523) = false then
             k_make_list_cons_x_6354 (false, (true, 0))
           else
             xs_1114 (snd (snd ii_3523)) (fun (x_9943:(bool * int)) -> k_make_list_cons_x_6354 (true, x_9943))
         else
           if fst (snd ii_3523) = false then
             x_1592 (snd (fst ii_3523))
               (fun (x_9940:(bool * int)) -> k_make_list_cons_x_6305 ((true, x_9940), (false, (true, 0))))
           else
             let x_4099 (k_make_list_cons_x_x_6396:((bool * int) -> X)) =
               x_1592 (snd (fst ii_3523)) k_make_list_cons_x_x_6396
             in
             x_4099
               (fun (x_6430:(bool * int)) ->
                  xs_1114 (snd (snd ii_3523))
                    (fun (x_9922:(bool * int)) -> k_make_list_cons_x_6305 ((true, x_6430), (true, x_9922))))
       in
       x_1732
     in
     let x_4194 (k_make_list_x_6465:((int -> ((bool * int) -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_6465
     in
     x_4194
       (fun (x_6605:(int -> ((bool * int) -> X) -> X)) ->
          (let x_4196 (k_make_list_x_6486:(int -> X)) = rand_int_cps () k_make_list_x_6486 in
           x_4196
             (fun (x_6601:int) ->
                k_make_list_6237
                  (let x_1739 (i_3447:int) (k_make_list_x_6524:((bool * int) -> X)) =
                     ((cons_1117 x_6601) x_6605) ((true, i_3447), (false, 0))
                       (fun (p_9966:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_make_list_x_6524 (snd (fst p_9966)))
                   in
                   x_1739))))
 in
 let rec
   append_1059 (x_1023:(((bool * int) * (bool * int)) -> (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) 
              (k_append_6637:((((bool * int) * (bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                -> X)) =
   let x_1746 (i_3310:int) (k_append_x_6644:((bool * int) -> X)) =
     x_1023 ((true, i_3310), (false, 0))
       (fun (p_9986:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_6644 (snd (fst p_9986)))
   in
   let x_1747 (i_3303:int) (k_append_x_6688:((bool * int) -> X)) =
     x_1023 ((false, 0), (true, i_3303))
       (fun (p_9996:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_6688 (snd (snd p_9996)))
   in
   let rec
     x_x_x_4007 (x_3968:int) (x_3969:int) (x_3970:int) 
               (k_append_x_x_x_6732:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     let x_4293 (k_append_x_x_x_x_6757:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((false, 0), (true, x_3968)) k_append_x_x_x_x_6757
     in
     x_4293
       (fun (x_6802:((bool * (bool * int)) * (bool * (bool * int)))) ->
          (let x_6198 (k_append_x_x_x_x_6787:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
             x_1023 ((true, x_3969), (true, x_3970)) k_append_x_x_x_x_6787
           in
           x_6198
             (fun (x_6801:((bool * (bool * int)) * (bool * (bool * int)))) ->
                k_append_x_x_x_6732 (snd (snd x_6802), snd (fst x_6801), snd (snd x_6801)))))
   in
   let x_6189 (k_append_x_6835:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
     x_1023 ((true, 0), (false, 0)) k_append_x_6835
   in
   x_6189
     (fun (x_8930:((bool * (bool * int)) * (bool * (bool * int)))) ->
        (if fst (snd (fst x_8930)) = false then
           k_append_6637
             (let
                x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) 
                      (k_append_x_6845:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                if fst (#0 iii_3257) = false then
                  let x_5559 (k_append_x_x_6877:((bool * (bool * int)) -> X)) =
                    if fst (#1 iii_3257) = false then
                      k_append_x_x_6877 (false, (true, 0))
                    else
                      x_1746 (snd (#1 iii_3257)) (fun (x_10540:(bool * int)) -> k_append_x_x_6877 (true, x_10540))
                  in
                  x_5559
                    (fun (x_6932:(bool * (bool * int))) ->
                       (let k_append_x_6930 (x_10548:(bool * (bool * int))) =
                          k_append_x_6845 ((false, (true, 0)), x_6932, x_10548)
                        in
                        if fst (#2 iii_3257) = false then
                          k_append_x_6930 (false, (true, 0))
                        else
                          x_1747 (snd (#2 iii_3257)) (fun (x_10544:(bool * int)) -> k_append_x_6930 (true, x_10544))))
                else
                  if fst (#1 iii_3257) = false then
                    let x_5497 (k_append_x_x_6939:((bool * int) -> X)) = x_1747 (snd (#0 iii_3257)) k_append_x_x_6939 in
                    x_5497
                      (fun (x_7003:(bool * int)) ->
                         (let k_append_x_7001 (x_10520:(bool * (bool * int))) =
                            k_append_x_6845 ((true, x_7003), (false, (true, 0)), x_10520)
                          in
                          if fst (#2 iii_3257) = false then
                            k_append_x_7001 (false, (true, 0))
                          else
                            x_1747 (snd (#2 iii_3257)) (fun (x_10516:(bool * int)) -> k_append_x_7001 (true, x_10516))))
                  else
                    if fst (#2 iii_3257) = false then
                      let x_5456 (k_append_x_x_7010:((bool * int) -> X)) = x_1747 (snd (#0 iii_3257)) k_append_x_x_7010 in
                      x_5456
                        (fun (x_7058:(bool * int)) ->
                           x_1746 (snd (#1 iii_3257))
                             (fun (x_10508:(bool * int)) ->
                                k_append_x_6845 ((true, x_7058), (true, x_10508), (false, (true, 0)))))
                    else
                      let x_5424 (k_append_x_x_7067:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                        x_x_x_4007 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) k_append_x_x_7067
                      in
                      x_5424
                        (fun (x_7099:((bool * int) * (bool * int) * (bool * int))) ->
                           k_append_x_6845 ((true, #0 x_7099), (true, #1 x_7099), (true, #2 x_7099)))
              in
              x_1811)
         else
           if fst (snd (fst x_8930)) <> false then
             let xs'_1014 (x_1150:int) (k_append_xs'_7121:((bool * int) -> X)) =
               x_1023 ((true, x_1150 + 1), (false, 0))
                 (fun (p_10113:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_7121 (snd (fst p_10113)))
             in
             let rec xs'_x_3843 (x_3817:int) (x_3818:int) (k_append_xs'_x_7165:(((bool * int) * (bool * int)) -> X)) =
               let x_6172 (k_append_xs'_x_x_7190:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_1023 ((true, x_3817 + 1), (true, x_3818)) k_append_xs'_x_x_7190
               in
               x_6172
                 (fun (x_7202:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_x_7165 (snd (fst x_7202), snd (snd x_7202)))
             in
             let cons_1225 (x_1221:int) (xs_1222:(int -> ((bool * int) -> X) -> X)) =
               let x_1625 (i_1220:int) (k_append_cons_x_7219:((bool * int) -> X)) =
                 if i_1220 = 0 then
                   k_append_cons_x_7219 (true, x_1221)
                 else
                   let x_4613 (k_append_cons_x_x_7232:((bool * int) -> X)) =
                     xs_1222 (i_1220 - 1) k_append_cons_x_x_7232
                   in
                   x_4613 (fun (x_7257:(bool * int)) -> k_append_cons_x_7219 x_7257)
               in
               let
                 x_1785 (ii_3086:((bool * int) * (bool * int))) 
                       (k_append_cons_x_7266:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_3086) = false then
                   let k_append_cons_x_7315 (x_10148:(bool * (bool * int))) =
                     k_append_cons_x_7266 ((false, (true, 0)), x_10148)
                   in
                   if fst (snd ii_3086) = false then
                     k_append_cons_x_7315 (false, (true, 0))
                   else
                     xs_1222 (snd (snd ii_3086)) (fun (x_10144:(bool * int)) -> k_append_cons_x_7315 (true, x_10144))
                 else
                   if fst (snd ii_3086) = false then
                     x_1625 (snd (fst ii_3086))
                       (fun (x_10141:(bool * int)) -> k_append_cons_x_7266 ((true, x_10141), (false, (true, 0))))
                   else
                     let x_4631 (k_append_cons_x_x_7357:((bool * int) -> X)) =
                       x_1625 (snd (fst ii_3086)) k_append_cons_x_x_7357
                     in
                     x_4631
                       (fun (x_7391:(bool * int)) ->
                          xs_1222 (snd (snd ii_3086))
                            (fun (x_10123:(bool * int)) -> k_append_cons_x_7266 ((true, x_7391), (true, x_10123))))
               in
               x_1785
             in
             let
               x_1788 (ii_3004:((bool * int) * (bool * int))) 
                     (k_append_x_7417:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
               if fst (fst ii_3004) = false then
                 let k_append_x_7466 (x_10182:(bool * (bool * int))) = k_append_x_7417 ((false, (true, 0)), x_10182) in
                 if fst (snd ii_3004) = false then
                   k_append_x_7466 (false, (true, 0))
                 else
                   x_1747 (snd (snd ii_3004)) (fun (x_10178:(bool * int)) -> k_append_x_7466 (true, x_10178))
               else
                 if fst (snd ii_3004) = false then
                   xs'_1014 (snd (fst ii_3004))
                     (fun (x_10175:(bool * int)) -> k_append_x_7417 ((true, x_10175), (false, (true, 0))))
                 else
                   let x_4730 (k_append_x_x_7509:(((bool * int) * (bool * int)) -> X)) =
                     xs'_x_3843 (snd (fst ii_3004)) (snd (snd ii_3004)) k_append_x_x_7509
                   in
                   x_4730
                     (fun (x_7533:((bool * int) * (bool * int))) ->
                        k_append_x_7417 ((true, fst x_7533), (true, snd x_7533)))
             in
             let
               x_4862
                     (k_append_x_7654:((((bool * int) * (bool * int) * (bool * int)) ->
                                          (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)
                                            -> X) -> X)) = append_1059 x_1788 k_append_x_7654
             in
             x_4862
               (fun (x_8573:(((bool * int) * (bool * int) * (bool * int)) ->
                               (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                  k_append_6637
                    (let x_1792 (i_2966:int) (k_append_x_7676:((bool * int) -> X)) =
                       x_8573 ((true, i_2966), (false, 0), (false, 0))
                         (fun (p_10230:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_7676 (snd (#0 p_10230)))
                     in
                     let x_1793 (i_2956:int) (k_append_x_7723:((bool * int) -> X)) =
                       x_8573 ((false, 0), (true, i_2956), (false, 0))
                         (fun (p_10249:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_7723 (snd (#1 p_10249)))
                     in
                     let x_1794 (i_2946:int) (k_append_x_7770:((bool * int) -> X)) =
                       x_8573 ((false, 0), (false, 0), (true, i_2946))
                         (fun (p_10268:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_7770 (snd (#2 p_10268)))
                     in
                     let rec
                       x_x_3895 (x_3857:int) (x_3858:int) (k_append_x_x_7818:(((bool * int) * (bool * int)) -> X)) =
                       let
                         x_4956
                               (k_append_x_x_x_7851:(((bool * (bool * int)) * (
                                                      bool * (bool * int)) * (
                                                      bool * (bool * int))) -> X)) =
                         x_8573 ((false, 0), (true, x_3857), (false, 0)) k_append_x_x_x_7851
                       in
                       x_4956
                         (fun (x_7905:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            x_8573 ((false, 0), (false, 0), (true, x_3858))
                              (fun (p_10304:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                                 k_append_x_x_7818 (snd (#1 x_7905), snd (#2 p_10304))))
                     in
                     let
                       x_1797 (ii_2929:((bool * int) * (bool * int))) 
                             (k_append_x_7910:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       if fst (fst ii_2929) = false then
                         let k_append_x_7959 (x_10331:(bool * (bool * int))) =
                           k_append_x_7910 ((false, (true, 0)), x_10331)
                         in
                         if fst (snd ii_2929) = false then
                           k_append_x_7959 (false, (true, 0))
                         else
                           x_1794 (snd (snd ii_2929)) (fun (x_10327:(bool * int)) -> k_append_x_7959 (true, x_10327))
                       else
                         if fst (snd ii_2929) = false then
                           x_1793 (snd (fst ii_2929))
                             (fun (x_10324:(bool * int)) -> k_append_x_7910 ((true, x_10324), (false, (true, 0))))
                         else
                           let x_4986 (k_append_x_x_8002:(((bool * int) * (bool * int)) -> X)) =
                             x_x_3895 (snd (fst ii_2929)) (snd (snd ii_2929)) k_append_x_x_8002
                           in
                           x_4986
                             (fun (x_8026:((bool * int) * (bool * int))) ->
                                k_append_x_7910 ((true, fst x_8026), (true, snd x_8026)))
                     in
                     let x_1802 (i_2893:int) (k_append_x_8145:((bool * int) -> X)) =
                       ((cons_1225 (snd (snd (fst x_8930)))) x_1792) 
                         ((true, i_2893), (false, 0))
                         (fun (p_10370:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8145 (snd (fst p_10370)))
                     in
                     let rec
                       x_x_x_3948 (x_3909:int) (x_3910:int) (x_3911:int) 
                                 (k_append_x_x_x_8186:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                       let x_6123 (k_append_x_x_x_x_8211:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1225 (snd (snd (fst x_8930)))) x_1792) 
                           ((true, x_3909), (false, 0)) k_append_x_x_x_x_8211
                       in
                       x_6123
                         (fun (x_8256:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_6114 (k_append_x_x_x_x_8241:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                               x_1023 ((true, x_3910), (true, x_3911)) k_append_x_x_x_x_8241
                             in
                             x_6114
                               (fun (x_8255:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                  k_append_x_x_x_8186 (snd (fst x_8256), snd (fst x_8255), snd (snd x_8255)))))
                     in
                     let
                       x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_8300:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_2861) = false then
                         let x_5349 (k_append_x_x_8332:((bool * (bool * int)) -> X)) =
                           if fst (#1 iii_2861) = false then
                             k_append_x_x_8332 (false, (true, 0))
                           else
                             x_1746 (snd (#1 iii_2861))
                               (fun (x_10459:(bool * int)) -> k_append_x_x_8332 (true, x_10459))
                         in
                         x_5349
                           (fun (x_8387:(bool * (bool * int))) ->
                              (let k_append_x_8385 (x_10467:(bool * (bool * int))) =
                                 k_append_x_8300 ((false, (true, 0)), x_8387, x_10467)
                               in
                               if fst (#2 iii_2861) = false then
                                 k_append_x_8385 (false, (true, 0))
                               else
                                 x_1747 (snd (#2 iii_2861))
                                   (fun (x_10463:(bool * int)) -> k_append_x_8385 (true, x_10463))))
                       else
                         if fst (#1 iii_2861) = false then
                           let x_5287 (k_append_x_x_8394:((bool * int) -> X)) =
                             x_1802 (snd (#0 iii_2861)) k_append_x_x_8394
                           in
                           x_5287
                             (fun (x_8458:(bool * int)) ->
                                (let k_append_x_8456 (x_10439:(bool * (bool * int))) =
                                   k_append_x_8300 ((true, x_8458), (false, (true, 0)), x_10439)
                                 in
                                 if fst (#2 iii_2861) = false then
                                   k_append_x_8456 (false, (true, 0))
                                 else
                                   x_1747 (snd (#2 iii_2861))
                                     (fun (x_10435:(bool * int)) -> k_append_x_8456 (true, x_10435))))
                         else
                           if fst (#2 iii_2861) = false then
                             let x_5246 (k_append_x_x_8465:((bool * int) -> X)) =
                               x_1802 (snd (#0 iii_2861)) k_append_x_x_8465
                             in
                             x_5246
                               (fun (x_8513:(bool * int)) ->
                                  x_1746 (snd (#1 iii_2861))
                                    (fun (x_10427:(bool * int)) ->
                                       k_append_x_8300 ((true, x_8513), (true, x_10427), (false, (true, 0)))))
                           else
                             let x_5214 (k_append_x_x_8522:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                               x_x_x_3948 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) k_append_x_x_8522
                             in
                             x_5214
                               (fun (x_8554:((bool * int) * (bool * int) * (bool * int))) ->
                                  k_append_x_8300 ((true, #0 x_8554), (true, #1 x_8554), (true, #2 x_8554)))
                     in
                     x_1807))
           else
             let x_1610 (k_append_x_8613:((int -> ((bool * int) -> X) -> X) -> X)) = _|_ in
             x_1610
               (fun (x_8915:(int -> ((bool * int) -> X) -> X)) ->
                  k_append_6637
                    (let
                       x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_8621:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_2432) = false then
                         let x_4493 (k_append_x_x_8653:((bool * (bool * int)) -> X)) =
                           if fst (#1 iii_2432) = false then
                             k_append_x_x_8653 (false, (true, 0))
                           else
                             x_1746 (snd (#1 iii_2432))
                               (fun (x_10085:(bool * int)) -> k_append_x_x_8653 (true, x_10085))
                         in
                         x_4493
                           (fun (x_8708:(bool * (bool * int))) ->
                              (let k_append_x_8706 (x_10093:(bool * (bool * int))) =
                                 k_append_x_8621 ((false, (true, 0)), x_8708, x_10093)
                               in
                               if fst (#2 iii_2432) = false then
                                 k_append_x_8706 (false, (true, 0))
                               else
                                 x_1747 (snd (#2 iii_2432))
                                   (fun (x_10089:(bool * int)) -> k_append_x_8706 (true, x_10089))))
                       else
                         if fst (#1 iii_2432) = false then
                           let x_4431 (k_append_x_x_8715:((bool * int) -> X)) =
                             x_8915 (snd (#0 iii_2432)) k_append_x_x_8715
                           in
                           x_4431
                             (fun (x_8779:(bool * int)) ->
                                (let k_append_x_8777 (x_10065:(bool * (bool * int))) =
                                   k_append_x_8621 ((true, x_8779), (false, (true, 0)), x_10065)
                                 in
                                 if fst (#2 iii_2432) = false then
                                   k_append_x_8777 (false, (true, 0))
                                 else
                                   x_1747 (snd (#2 iii_2432))
                                     (fun (x_10061:(bool * int)) -> k_append_x_8777 (true, x_10061))))
                         else
                           if fst (#2 iii_2432) = false then
                             let x_4390 (k_append_x_x_8786:((bool * int) -> X)) =
                               x_8915 (snd (#0 iii_2432)) k_append_x_x_8786
                             in
                             x_4390
                               (fun (x_8834:(bool * int)) ->
                                  x_1746 (snd (#1 iii_2432))
                                    (fun (x_10053:(bool * int)) ->
                                       k_append_x_8621 ((true, x_8834), (true, x_10053), (false, (true, 0)))))
                           else
                             let x_4356 (k_append_x_x_8841:((bool * int) -> X)) =
                               x_8915 (snd (#0 iii_2432)) k_append_x_x_8841
                             in
                             x_4356
                               (fun (x_8896:(bool * int)) ->
                                  (let x_4366 (k_append_x_x_8853:((bool * int) -> X)) =
                                     x_1746 (snd (#1 iii_2432)) k_append_x_x_8853
                                   in
                                   x_4366
                                     (fun (x_8895:(bool * int)) ->
                                        x_1747 (snd (#2 iii_2432))
                                          (fun (x_10022:(bool * int)) ->
                                             k_append_x_8621 ((true, x_8896), (true, x_8895), (true, x_10022))))))
                     in
                     x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_8957:(unit -> X)) =
   let x_5625 (k_main_x_8970:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_8970 in
   x_5625
     (fun (x_9841:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_8985:((bool * int) -> X)) = k_main_f_8985 (false, 0) in
         let
           x_1820 (ix_2198:((bool * int) * (bool * int))) 
                 (k_main_x_8998:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
           if fst (fst ix_2198) = false then
             let k_main_x_9047 (x_10585:(bool * (bool * int))) = k_main_x_8998 ((false, (true, 0)), x_10585) in
             if fst (snd ix_2198) = false then
               k_main_x_9047 (false, (true, 0))
             else
               f_1479 (snd (snd ix_2198)) (fun (x_10581:(bool * int)) -> k_main_x_9047 (true, x_10581))
           else
             if fst (snd ix_2198) = false then
               x_9841 (snd (fst ix_2198))
                 (fun (x_10578:(bool * int)) -> k_main_x_8998 ((true, x_10578), (false, (true, 0))))
             else
               let x_5637 (k_main_x_x_9089:((bool * int) -> X)) = x_9841 (snd (fst ix_2198)) k_main_x_x_9089 in
               x_5637
                 (fun (x_9123:(bool * int)) ->
                    f_1479 (snd (snd ix_2198))
                      (fun (x_10560:(bool * int)) -> k_main_x_8998 ((true, x_9123), (true, x_10560))))
         in
         let
           x_5768
                 (k_main_x_9235:((((bool * int) * (bool * int) * (bool * int)) ->
                                    (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                   -> X)) = append_1059 x_1820 k_main_x_9235
         in
         x_5768
           (fun (x_9821:(((bool * int) * (bool * int) * (bool * int)) ->
                           (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_9305:((bool * int) -> X)) =
                 x_9821 ((false, 0), (true, i_2150), (false, 0))
                   (fun (p_10652:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_9305 (snd (#1 p_10652)))
               in
               let x_1826 (i_2140:int) (k_main_x_9354:((bool * int) -> X)) =
                 x_9821 ((false, 0), (false, 0), (true, i_2140))
                   (fun (p_10671:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_9354 (snd (#2 p_10671)))
               in
               let rec x_x_4065 (x_4027:int) (x_4028:int) (k_main_x_x_9403:(((bool * int) * (bool * int)) -> X)) =
                 let
                   x_5862
                         (k_main_x_x_x_9436:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) ->
                                               X)) = x_9821 ((false, 0), (true, x_4027), (false, 0)) k_main_x_x_x_9436
                 in
                 x_5862
                   (fun (x_9490:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      x_9821 ((false, 0), (false, 0), (true, x_4028))
                        (fun (p_10707:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                           k_main_x_x_9403 (snd (#1 x_9490), snd (#2 p_10707))))
               in
               let
                 x_1829 (ii_2123:((bool * int) * (bool * int))) 
                       (k_main_x_9498:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_2123) = false then
                   let k_main_x_9547 (x_10734:(bool * (bool * int))) = k_main_x_9498 ((false, (true, 0)), x_10734) in
                   if fst (snd ii_2123) = false then
                     k_main_x_9547 (false, (true, 0))
                   else
                     x_1826 (snd (snd ii_2123)) (fun (x_10730:(bool * int)) -> k_main_x_9547 (true, x_10730))
                 else
                   if fst (snd ii_2123) = false then
                     x_1825 (snd (fst ii_2123))
                       (fun (x_10727:(bool * int)) -> k_main_x_9498 ((true, x_10727), (false, (true, 0))))
                   else
                     let x_5892 (k_main_x_x_9590:(((bool * int) * (bool * int)) -> X)) =
                       x_x_4065 (snd (fst ii_2123)) (snd (snd ii_2123)) k_main_x_x_9590
                     in
                     x_5892
                       (fun (x_9614:((bool * int) * (bool * int))) ->
                          k_main_x_9498 ((true, fst x_9614), (true, snd x_9614)))
               in
               let
                 x_6069 (k_main_x_9740:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_9821 ((true, i_1016), (false, 0), (false, 0)) k_main_x_9740
               in
               x_6069
                 (fun (x_9788:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let x_6046 (k_main_x_9752:((bool * int) -> X)) = x_9841 i_1016 k_main_x_9752 in
                     x_6046
                       (fun (x_9787:(bool * int)) ->
                          (let n_1504 (k_main_n_9763:(int -> X)) =
                             if fst (snd (#0 x_9788)) <> false then
                               k_main_n_9763 (snd (snd (#0 x_9788)))
                             else
                               _|_
                           in
                           n_1504
                             (fun (n_9786:int) ->
                                (let n_1505 (k_main_n_9771:(int -> X)) =
                                   if fst x_9787 <> false then
                                     k_main_n_9771 (snd x_9787)
                                   else
                                     _|_
                                 in
                                 n_1505
                                   (fun (n_9785:int) ->
                                      (if n_9786 = n_9785 then
                                         k_main_8957 ()
                                       else
                                         {|fail|} () k_main_8957))))))))))))
 in
 let x_6064 (k_x_9852:(int -> X)) = rand_int_cps () k_x_9852 in
 x_6064
   (fun (x_9897:int) ->
      (let x_6066 (k_x_9864:(int -> X)) = rand_int_cps () k_x_9864 in
       x_6066
         (fun (x_9896:int) ->
            (let x_6068 (k_x_9885:(unit -> X)) = (main_1015 x_9897) x_9896 k_x_9885 in
             x_6068 (fun (x_9891:unit) -> {end})))))

remove_pair:
 let rec make_list_1008 (n_1009:int) (k_make_list_6237:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6237 (fun (x_1123:int) -> fun (k_make_list_6239:(bool -> int -> X)) -> k_make_list_6239 false 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool -> int -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_6258:(bool -> int -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_6258 true x_1113
         else
           let x_4081 (k_make_list_cons_x_x_6271:(bool -> int -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_6271 in
           x_4081 (fun (x0_6296:bool) -> fun (x1_6296:int) -> k_make_list_cons_x_6258 x0_6296 x1_6296)
       in
       let
         x_1732 (ii00_3523:bool) (ii01_3523:int) (ii10_3523:bool) (ii11_3523:int) 
               (k_make_list_cons_x_6305:(bool ->
                                           bool ->
                                             r011_6302:int ->
                                               bool -> bool -> r111_6302:int[\r111_6302. r011_6302 = r111_6302] -> X)) =
         if ii00_3523 = false then
           let k_make_list_cons_x_6354 (x0_9947:bool) (x10_9947:bool) (x11_9947:int) =
             k_make_list_cons_x_6305 false true 0 x0_9947 x10_9947 x11_9947
           in
           if ii10_3523 = false then
             k_make_list_cons_x_6354 false true 0
           else
             xs_1114 ii11_3523
               (fun (x0_9943:bool) -> fun (x1_9943:int) -> k_make_list_cons_x_6354 true x0_9943 x1_9943)
         else
           if ii10_3523 = false then
             x_1592 ii01_3523
               (fun (x0_9940:bool) -> fun (x1_9940:int) -> k_make_list_cons_x_6305 true x0_9940 x1_9940 false true 0)
           else
             let x_4099 (k_make_list_cons_x_x_6396:(bool -> int -> X)) = x_1592 ii01_3523 k_make_list_cons_x_x_6396 in
             x_4099
               (fun (x0_6430:bool) ->
                  fun (x1_6430:int) ->
                    xs_1114 ii11_3523
                      (fun (x0_9922:bool) ->
                         fun (x1_9922:int) -> k_make_list_cons_x_6305 true x0_6430 x1_6430 true x0_9922 x1_9922))
       in
       x_1732
     in
     let x_4194 (k_make_list_x_6465:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_6465
     in
     x_4194
       (fun (x_6605:(int -> (bool -> int -> X) -> X)) ->
          (let x_4196 (k_make_list_x_6486:(int -> X)) = rand_int_cps () k_make_list_x_6486 in
           x_4196
             (fun (x_6601:int) ->
                k_make_list_6237
                  (let x_1739 (i_3447:int) (k_make_list_x_6524:(bool -> int -> X)) =
                     cons_1117 x_6601 x_6605 true i_3447 false 0
                       (fun (p00_9966:bool) ->
                          fun (p010_9966:bool) ->
                            fun (p011_9966:int) ->
                              fun (p10_9966:bool) ->
                                fun (p110_9966:bool) -> fun (p111_9966:int) -> k_make_list_x_6524 p010_9966 p011_9966)
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
                                    r011_6631:int ->
                                      bool -> bool -> r111_6631:int[\r111_6631. r011_6631 = r111_6631] -> X) -> X))
              (k_append_6637:((bool ->
                                 int ->
                                   bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_6634:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_6634:int[\r111_6634. r011_6634 = r111_6634] ->
                                                        bool -> bool -> int -> X) -> X) -> X)) =
   let x_1746 (i_3310:int) (k_append_x_6644:(bool -> int -> X)) =
     x_1023 true i_3310 false 0
       (fun (p00_9986:bool) ->
          fun (p010_9986:bool) ->
            fun (p011_9986:int) ->
              fun (p10_9986:bool) -> fun (p110_9986:bool) -> fun (p111_9986:int) -> k_append_x_6644 p010_9986 p011_9986)
   in
   let x_1747 (i_3303:int) (k_append_x_6688:(bool -> int -> X)) =
     x_1023 false 0 true i_3303
       (fun (p00_9996:bool) ->
          fun (p010_9996:bool) ->
            fun (p011_9996:int) ->
              fun (p10_9996:bool) -> fun (p110_9996:bool) -> fun (p111_9996:int) -> k_append_x_6688 p110_9996 p111_9996)
   in
   let rec
     x_x_x_4007 (x_3968:int) (x_3969:int) (x_3970:int) 
               (k_append_x_x_x_6732:(bool -> int -> bool -> int -> bool -> int -> X)) =
     let
       x_4293
             (k_append_x_x_x_x_6757:(bool ->
                                       bool ->
                                         r011_6756:int ->
                                           bool -> bool -> r111_6756:int[\r111_6756. r011_6756 = r111_6756] -> X)) =
       x_1023 false 0 true x_3968 k_append_x_x_x_x_6757
     in
     x_4293
       (fun (x00_6802:bool) ->
          fun (x010_6802:bool) ->
            fun (x011_6802:int) ->
              fun (x10_6802:bool) ->
                fun (x110_6802:bool) ->
                  fun (x111_6802:int) ->
                    (let
                       x_6198
                             (k_append_x_x_x_x_6787:(bool ->
                                                       bool ->
                                                         r011_6786:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_6786:int[\r111_6786. r011_6786 = r111_6786] -> X)) =
                       x_1023 true x_3969 true x_3970 k_append_x_x_x_x_6787
                     in
                     x_6198
                       (fun (x00_6801:bool) ->
                          fun (x010_6801:bool) ->
                            fun (x011_6801:int) ->
                              fun (x10_6801:bool) ->
                                fun (x110_6801:bool) ->
                                  fun (x111_6801:int) ->
                                    k_append_x_x_x_6732 x110_6802 x111_6802 x010_6801 x011_6801 x110_6801 x111_6801)))
   in
   let
     x_6189
           (k_append_x_6835:(bool ->
                               bool ->
                                 r011_6834:int -> bool -> bool -> r111_6834:int[\r111_6834. r011_6834 = r111_6834] -> X)) =
     x_1023 true 0 false 0 k_append_x_6835
   in
   x_6189
     (fun (x00_8930:bool) ->
        fun (x010_8930:bool) ->
          fun (x011_8930:int) ->
            fun (x10_8930:bool) ->
              fun (x110_8930:bool) ->
                fun (x111_8930:int) ->
                  (if x010_8930 = false then
                     k_append_6637
                       (let
                          x_1811 (iii00_3257:bool) (iii01_3257:int) (iii10_3257:bool) (iii11_3257:int) 
                                (iii20_3257:bool) (iii21_3257:int) 
                                (k_append_x_6845:(bool ->
                                                    bool ->
                                                      r011_6842:int ->
                                                        bool ->
                                                          bool ->
                                                            r111_6842:
                                                              int[\r111_6842. r011_6842 = r111_6842] ->
                                                              bool -> bool -> int -> X)) =
                          if iii00_3257 = false then
                            let x_5559 (k_append_x_x_6877:(bool -> bool -> int -> X)) =
                              if iii10_3257 = false then
                                k_append_x_x_6877 false true 0
                              else
                                x_1746 iii11_3257
                                  (fun (x0_10540:bool) ->
                                     fun (x1_10540:int) -> k_append_x_x_6877 true x0_10540 x1_10540)
                            in
                            x_5559
                              (fun (x0_6932:bool) ->
                                 fun (x10_6932:bool) ->
                                   fun (x11_6932:int) ->
                                     (let k_append_x_6930 (x0_10548:bool) (x10_10548:bool) (x11_10548:int) =
                                        k_append_x_6845 false true 0 x0_6932 x10_6932 x11_6932 x0_10548 x10_10548
                                          x11_10548
                                      in
                                      if iii20_3257 = false then
                                        k_append_x_6930 false true 0
                                      else
                                        x_1747 iii21_3257
                                          (fun (x0_10544:bool) ->
                                             fun (x1_10544:int) -> k_append_x_6930 true x0_10544 x1_10544)))
                          else
                            if iii10_3257 = false then
                              let x_5497 (k_append_x_x_6939:(bool -> int -> X)) = x_1747 iii01_3257 k_append_x_x_6939 in
                              x_5497
                                (fun (x0_7003:bool) ->
                                   fun (x1_7003:int) ->
                                     (let k_append_x_7001 (x0_10520:bool) (x10_10520:bool) (x11_10520:int) =
                                        k_append_x_6845 true x0_7003 x1_7003 false true 0 x0_10520 x10_10520 x11_10520
                                      in
                                      if iii20_3257 = false then
                                        k_append_x_7001 false true 0
                                      else
                                        x_1747 iii21_3257
                                          (fun (x0_10516:bool) ->
                                             fun (x1_10516:int) -> k_append_x_7001 true x0_10516 x1_10516)))
                            else
                              if iii20_3257 = false then
                                let x_5456 (k_append_x_x_7010:(bool -> int -> X)) = x_1747 iii01_3257 k_append_x_x_7010 in
                                x_5456
                                  (fun (x0_7058:bool) ->
                                     fun (x1_7058:int) ->
                                       x_1746 iii11_3257
                                         (fun (x0_10508:bool) ->
                                            fun (x1_10508:int) ->
                                              k_append_x_6845 true x0_7058 x1_7058 true x0_10508 x1_10508 false true 0))
                              else
                                let x_5424 (k_append_x_x_7067:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                  x_x_x_4007 iii01_3257 iii11_3257 iii21_3257 k_append_x_x_7067
                                in
                                x_5424
                                  (fun (x00_7099:bool) ->
                                     fun (x01_7099:int) ->
                                       fun (x10_7099:bool) ->
                                         fun (x11_7099:int) ->
                                           fun (x20_7099:bool) ->
                                             fun (x21_7099:int) ->
                                               k_append_x_6845 true x00_7099 x01_7099 true x10_7099 x11_7099 true
                                                 x20_7099 x21_7099)
                        in
                        x_1811)
                   else
                     if x010_8930 <> false then
                       let xs'_1014 (x_1150:int) (k_append_xs'_7121:(bool -> int -> X)) =
                         x_1023 true (x_1150 + 1) false 0
                           (fun (p00_10113:bool) ->
                              fun (p010_10113:bool) ->
                                fun (p011_10113:int) ->
                                  fun (p10_10113:bool) ->
                                    fun (p110_10113:bool) ->
                                      fun (p111_10113:int) -> k_append_xs'_7121 p010_10113 p011_10113)
                       in
                       let rec
                         xs'_x_3843 (x_3817:int) (x_3818:int) (k_append_xs'_x_7165:(bool -> int -> bool -> int -> X)) =
                         let
                           x_6172
                                 (k_append_xs'_x_x_7190:(bool ->
                                                           bool ->
                                                             r011_7189:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_7189:
                                                                    int[\r111_7189. r011_7189 = r111_7189] -> X)) =
                           x_1023 true (x_3817 + 1) true x_3818 k_append_xs'_x_x_7190
                         in
                         x_6172
                           (fun (x00_7202:bool) ->
                              fun (x010_7202:bool) ->
                                fun (x011_7202:int) ->
                                  fun (x10_7202:bool) ->
                                    fun (x110_7202:bool) ->
                                      fun (x111_7202:int) ->
                                        k_append_xs'_x_7165 x010_7202 x011_7202 x110_7202 x111_7202)
                       in
                       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool -> int -> X) -> X)) =
                         let x_1625 (i_1220:int) (k_append_cons_x_7219:(bool -> int -> X)) =
                           if i_1220 = 0 then
                             k_append_cons_x_7219 true x_1221
                           else
                             let x_4613 (k_append_cons_x_x_7232:(bool -> int -> X)) =
                               xs_1222 (i_1220 - 1) k_append_cons_x_x_7232
                             in
                             x_4613 (fun (x0_7257:bool) -> fun (x1_7257:int) -> k_append_cons_x_7219 x0_7257 x1_7257)
                         in
                         let
                           x_1785 (ii00_3086:bool) (ii01_3086:int) (ii10_3086:bool) (ii11_3086:int) 
                                 (k_append_cons_x_7266:(bool ->
                                                          bool ->
                                                            r011_7263:int ->
                                                              bool ->
                                                                bool ->
                                                                  r111_7263:int[\r111_7263. r011_7263 = r111_7263] -> X)) =
                           if ii00_3086 = false then
                             let k_append_cons_x_7315 (x0_10148:bool) (x10_10148:bool) (x11_10148:int) =
                               k_append_cons_x_7266 false true 0 x0_10148 x10_10148 x11_10148
                             in
                             if ii10_3086 = false then
                               k_append_cons_x_7315 false true 0
                             else
                               xs_1222 ii11_3086
                                 (fun (x0_10144:bool) ->
                                    fun (x1_10144:int) -> k_append_cons_x_7315 true x0_10144 x1_10144)
                           else
                             if ii10_3086 = false then
                               x_1625 ii01_3086
                                 (fun (x0_10141:bool) ->
                                    fun (x1_10141:int) -> k_append_cons_x_7266 true x0_10141 x1_10141 false true 0)
                             else
                               let x_4631 (k_append_cons_x_x_7357:(bool -> int -> X)) =
                                 x_1625 ii01_3086 k_append_cons_x_x_7357
                               in
                               x_4631
                                 (fun (x0_7391:bool) ->
                                    fun (x1_7391:int) ->
                                      xs_1222 ii11_3086
                                        (fun (x0_10123:bool) ->
                                           fun (x1_10123:int) ->
                                             k_append_cons_x_7266 true x0_7391 x1_7391 true x0_10123 x1_10123))
                         in
                         x_1785
                       in
                       let
                         x_1788 (ii00_3004:bool) (ii01_3004:int) (ii10_3004:bool) (ii11_3004:int) 
                               (k_append_x_7417:(bool ->
                                                   bool ->
                                                     r011_7416:int ->
                                                       bool ->
                                                         bool -> r111_7416:int[\r111_7416. r011_7416 = r111_7416] -> X)) =
                         if ii00_3004 = false then
                           let k_append_x_7466 (x0_10182:bool) (x10_10182:bool) (x11_10182:int) =
                             k_append_x_7417 false true 0 x0_10182 x10_10182 x11_10182
                           in
                           if ii10_3004 = false then
                             k_append_x_7466 false true 0
                           else
                             x_1747 ii11_3004
                               (fun (x0_10178:bool) -> fun (x1_10178:int) -> k_append_x_7466 true x0_10178 x1_10178)
                         else
                           if ii10_3004 = false then
                             xs'_1014 ii01_3004
                               (fun (x0_10175:bool) ->
                                  fun (x1_10175:int) -> k_append_x_7417 true x0_10175 x1_10175 false true 0)
                           else
                             let x_4730 (k_append_x_x_7509:(bool -> int -> bool -> int -> X)) =
                               xs'_x_3843 ii01_3004 ii11_3004 k_append_x_x_7509
                             in
                             x_4730
                               (fun (x00_7533:bool) ->
                                  fun (x01_7533:int) ->
                                    fun (x10_7533:bool) ->
                                      fun (x11_7533:int) ->
                                        k_append_x_7417 true x00_7533 x01_7533 true x10_7533 x11_7533)
                       in
                       let
                         x_4862
                               (k_append_x_7654:((bool ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          bool ->
                                                            int ->
                                                              (bool ->
                                                                 bool ->
                                                                   r011_7651:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_7651:
                                                                    int[\r111_7651. r011_7651 = r111_7651] ->
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                         append_1059 x_1788 k_append_x_7654
                       in
                       x_4862
                         (fun (x_8573:(bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               bool ->
                                                 int ->
                                                   (bool ->
                                                      bool ->
                                                        r011_8571:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_8571:
                                                                int[\r111_8571. r011_8571 = r111_8571] ->
                                                                bool -> bool -> int -> X) -> X)) ->
                            k_append_6637
                              (let x_1792 (i_2966:int) (k_append_x_7676:(bool -> int -> X)) =
                                 x_8573 true i_2966 false 0 false 0
                                   (fun (p00_10230:bool) ->
                                      fun (p010_10230:bool) ->
                                        fun (p011_10230:int) ->
                                          fun (p10_10230:bool) ->
                                            fun (p110_10230:bool) ->
                                              fun (p111_10230:int) ->
                                                fun (p20_10230:bool) ->
                                                  fun (p210_10230:bool) ->
                                                    fun (p211_10230:int) -> k_append_x_7676 p010_10230 p011_10230)
                               in
                               let x_1793 (i_2956:int) (k_append_x_7723:(bool -> int -> X)) =
                                 x_8573 false 0 true i_2956 false 0
                                   (fun (p00_10249:bool) ->
                                      fun (p010_10249:bool) ->
                                        fun (p011_10249:int) ->
                                          fun (p10_10249:bool) ->
                                            fun (p110_10249:bool) ->
                                              fun (p111_10249:int) ->
                                                fun (p20_10249:bool) ->
                                                  fun (p210_10249:bool) ->
                                                    fun (p211_10249:int) -> k_append_x_7723 p110_10249 p111_10249)
                               in
                               let x_1794 (i_2946:int) (k_append_x_7770:(bool -> int -> X)) =
                                 x_8573 false 0 false 0 true i_2946
                                   (fun (p00_10268:bool) ->
                                      fun (p010_10268:bool) ->
                                        fun (p011_10268:int) ->
                                          fun (p10_10268:bool) ->
                                            fun (p110_10268:bool) ->
                                              fun (p111_10268:int) ->
                                                fun (p20_10268:bool) ->
                                                  fun (p210_10268:bool) ->
                                                    fun (p211_10268:int) -> k_append_x_7770 p210_10268 p211_10268)
                               in
                               let rec
                                 x_x_3895 (x_3857:int) (x_3858:int) 
                                         (k_append_x_x_7818:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_4956
                                         (k_append_x_x_x_7851:(bool ->
                                                                 bool ->
                                                                   r011_7850:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_7850:
                                                                    int[\r111_7850. r011_7850 = r111_7850] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_8573 false 0 true x_3857 false 0 k_append_x_x_x_7851
                                 in
                                 x_4956
                                   (fun (x00_7905:bool) ->
                                      fun (x010_7905:bool) ->
                                        fun (x011_7905:int) ->
                                          fun (x10_7905:bool) ->
                                            fun (x110_7905:bool) ->
                                              fun (x111_7905:int) ->
                                                fun (x20_7905:bool) ->
                                                  fun (x210_7905:bool) ->
                                                    fun (x211_7905:int) ->
                                                      x_8573 false 0 false 0 true x_3858
                                                        (fun (p00_10304:bool) ->
                                                           fun (p010_10304:bool) ->
                                                             fun (p011_10304:int) ->
                                                               fun (p10_10304:bool) ->
                                                                 fun (p110_10304:bool) ->
                                                                   fun (p111_10304:int) ->
                                                                    fun (p20_10304:bool) ->
                                                                    fun (p210_10304:bool) ->
                                                                    fun (p211_10304:int) ->
                                                                    k_append_x_x_7818 x110_7905 x111_7905 p210_10304
                                                                    p211_10304))
                               in
                               let
                                 x_1797 (ii00_2929:bool) (ii01_2929:int) (ii10_2929:bool) (ii11_2929:int) 
                                       (k_append_x_7910:(bool ->
                                                           bool ->
                                                             r011_7907:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_7907:
                                                                    int[\r111_7907. r011_7907 = r111_7907] -> X)) =
                                 if ii00_2929 = false then
                                   let k_append_x_7959 (x0_10331:bool) (x10_10331:bool) (x11_10331:int) =
                                     k_append_x_7910 false true 0 x0_10331 x10_10331 x11_10331
                                   in
                                   if ii10_2929 = false then
                                     k_append_x_7959 false true 0
                                   else
                                     x_1794 ii11_2929
                                       (fun (x0_10327:bool) ->
                                          fun (x1_10327:int) -> k_append_x_7959 true x0_10327 x1_10327)
                                 else
                                   if ii10_2929 = false then
                                     x_1793 ii01_2929
                                       (fun (x0_10324:bool) ->
                                          fun (x1_10324:int) -> k_append_x_7910 true x0_10324 x1_10324 false true 0)
                                   else
                                     let x_4986 (k_append_x_x_8002:(bool -> int -> bool -> int -> X)) =
                                       x_x_3895 ii01_2929 ii11_2929 k_append_x_x_8002
                                     in
                                     x_4986
                                       (fun (x00_8026:bool) ->
                                          fun (x01_8026:int) ->
                                            fun (x10_8026:bool) ->
                                              fun (x11_8026:int) ->
                                                k_append_x_7910 true x00_8026 x01_8026 true x10_8026 x11_8026)
                               in
                               let x_1802 (i_2893:int) (k_append_x_8145:(bool -> int -> X)) =
                                 cons_1225 x011_8930 x_1792 true i_2893 false 0
                                   (fun (p00_10370:bool) ->
                                      fun (p010_10370:bool) ->
                                        fun (p011_10370:int) ->
                                          fun (p10_10370:bool) ->
                                            fun (p110_10370:bool) ->
                                              fun (p111_10370:int) -> k_append_x_8145 p010_10370 p011_10370)
                               in
                               let rec
                                 x_x_x_3948 (x_3909:int) (x_3910:int) (x_3911:int) 
                                           (k_append_x_x_x_8186:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                 let
                                   x_6123
                                         (k_append_x_x_x_x_8211:(bool ->
                                                                   bool ->
                                                                    r011_8210:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8210:
                                                                    int[\r111_8210. r011_8210 = r111_8210] -> X)) =
                                   cons_1225 x011_8930 x_1792 true x_3909 false 0 k_append_x_x_x_x_8211
                                 in
                                 x_6123
                                   (fun (x00_8256:bool) ->
                                      fun (x010_8256:bool) ->
                                        fun (x011_8256:int) ->
                                          fun (x10_8256:bool) ->
                                            fun (x110_8256:bool) ->
                                              fun (x111_8256:int) ->
                                                (let
                                                   x_6114
                                                         (k_append_x_x_x_x_8241:(
                                                         bool ->
                                                           bool ->
                                                             r011_8240:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8240:
                                                                    int[\r111_8240. r011_8240 = r111_8240] -> X)) =
                                                   x_1023 true x_3910 true x_3911 k_append_x_x_x_x_8241
                                                 in
                                                 x_6114
                                                   (fun (x00_8255:bool) ->
                                                      fun (x010_8255:bool) ->
                                                        fun (x011_8255:int) ->
                                                          fun (x10_8255:bool) ->
                                                            fun (x110_8255:bool) ->
                                                              fun (x111_8255:int) ->
                                                                k_append_x_x_x_8186 x010_8256 x011_8256 x010_8255
                                                                  x011_8255 x110_8255 x111_8255)))
                               in
                               let
                                 x_1807 (iii00_2861:bool) (iii01_2861:int) (iii10_2861:bool) (iii11_2861:int) 
                                       (iii20_2861:bool) (iii21_2861:int) 
                                       (k_append_x_8300:(bool ->
                                                           bool ->
                                                             r011_8297:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8297:
                                                                    int[\r111_8297. r011_8297 = r111_8297] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2861 = false then
                                   let x_5349 (k_append_x_x_8332:(bool -> bool -> int -> X)) =
                                     if iii10_2861 = false then
                                       k_append_x_x_8332 false true 0
                                     else
                                       x_1746 iii11_2861
                                         (fun (x0_10459:bool) ->
                                            fun (x1_10459:int) -> k_append_x_x_8332 true x0_10459 x1_10459)
                                   in
                                   x_5349
                                     (fun (x0_8387:bool) ->
                                        fun (x10_8387:bool) ->
                                          fun (x11_8387:int) ->
                                            (let k_append_x_8385 (x0_10467:bool) (x10_10467:bool) (x11_10467:int) =
                                               k_append_x_8300 false true 0 x0_8387 x10_8387 x11_8387 x0_10467
                                                 x10_10467 x11_10467
                                             in
                                             if iii20_2861 = false then
                                               k_append_x_8385 false true 0
                                             else
                                               x_1747 iii21_2861
                                                 (fun (x0_10463:bool) ->
                                                    fun (x1_10463:int) -> k_append_x_8385 true x0_10463 x1_10463)))
                                 else
                                   if iii10_2861 = false then
                                     let x_5287 (k_append_x_x_8394:(bool -> int -> X)) =
                                       x_1802 iii01_2861 k_append_x_x_8394
                                     in
                                     x_5287
                                       (fun (x0_8458:bool) ->
                                          fun (x1_8458:int) ->
                                            (let k_append_x_8456 (x0_10439:bool) (x10_10439:bool) (x11_10439:int) =
                                               k_append_x_8300 true x0_8458 x1_8458 false true 0 x0_10439 x10_10439
                                                 x11_10439
                                             in
                                             if iii20_2861 = false then
                                               k_append_x_8456 false true 0
                                             else
                                               x_1747 iii21_2861
                                                 (fun (x0_10435:bool) ->
                                                    fun (x1_10435:int) -> k_append_x_8456 true x0_10435 x1_10435)))
                                   else
                                     if iii20_2861 = false then
                                       let x_5246 (k_append_x_x_8465:(bool -> int -> X)) =
                                         x_1802 iii01_2861 k_append_x_x_8465
                                       in
                                       x_5246
                                         (fun (x0_8513:bool) ->
                                            fun (x1_8513:int) ->
                                              x_1746 iii11_2861
                                                (fun (x0_10427:bool) ->
                                                   fun (x1_10427:int) ->
                                                     k_append_x_8300 true x0_8513 x1_8513 true x0_10427 x1_10427 false
                                                       true 0))
                                     else
                                       let
                                         x_5214 (k_append_x_x_8522:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                         x_x_x_3948 iii01_2861 iii11_2861 iii21_2861 k_append_x_x_8522
                                       in
                                       x_5214
                                         (fun (x00_8554:bool) ->
                                            fun (x01_8554:int) ->
                                              fun (x10_8554:bool) ->
                                                fun (x11_8554:int) ->
                                                  fun (x20_8554:bool) ->
                                                    fun (x21_8554:int) ->
                                                      k_append_x_8300 true x00_8554 x01_8554 true x10_8554 x11_8554
                                                        true x20_8554 x21_8554)
                               in
                               x_1807))
                     else
                       let x_1610 (k_append_x_8613:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                       x_1610
                         (fun (x_8915:(int -> (bool -> int -> X) -> X)) ->
                            k_append_6637
                              (let
                                 x_1761 (iii00_2432:bool) (iii01_2432:int) (iii10_2432:bool) (iii11_2432:int) 
                                       (iii20_2432:bool) (iii21_2432:int) 
                                       (k_append_x_8621:(bool ->
                                                           bool ->
                                                             r011_8618:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8618:
                                                                    int[\r111_8618. r011_8618 = r111_8618] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2432 = false then
                                   let x_4493 (k_append_x_x_8653:(bool -> bool -> int -> X)) =
                                     if iii10_2432 = false then
                                       k_append_x_x_8653 false true 0
                                     else
                                       x_1746 iii11_2432
                                         (fun (x0_10085:bool) ->
                                            fun (x1_10085:int) -> k_append_x_x_8653 true x0_10085 x1_10085)
                                   in
                                   x_4493
                                     (fun (x0_8708:bool) ->
                                        fun (x10_8708:bool) ->
                                          fun (x11_8708:int) ->
                                            (let k_append_x_8706 (x0_10093:bool) (x10_10093:bool) (x11_10093:int) =
                                               k_append_x_8621 false true 0 x0_8708 x10_8708 x11_8708 x0_10093
                                                 x10_10093 x11_10093
                                             in
                                             if iii20_2432 = false then
                                               k_append_x_8706 false true 0
                                             else
                                               x_1747 iii21_2432
                                                 (fun (x0_10089:bool) ->
                                                    fun (x1_10089:int) -> k_append_x_8706 true x0_10089 x1_10089)))
                                 else
                                   if iii10_2432 = false then
                                     let x_4431 (k_append_x_x_8715:(bool -> int -> X)) =
                                       x_8915 iii01_2432 k_append_x_x_8715
                                     in
                                     x_4431
                                       (fun (x0_8779:bool) ->
                                          fun (x1_8779:int) ->
                                            (let k_append_x_8777 (x0_10065:bool) (x10_10065:bool) (x11_10065:int) =
                                               k_append_x_8621 true x0_8779 x1_8779 false true 0 x0_10065 x10_10065
                                                 x11_10065
                                             in
                                             if iii20_2432 = false then
                                               k_append_x_8777 false true 0
                                             else
                                               x_1747 iii21_2432
                                                 (fun (x0_10061:bool) ->
                                                    fun (x1_10061:int) -> k_append_x_8777 true x0_10061 x1_10061)))
                                   else
                                     if iii20_2432 = false then
                                       let x_4390 (k_append_x_x_8786:(bool -> int -> X)) =
                                         x_8915 iii01_2432 k_append_x_x_8786
                                       in
                                       x_4390
                                         (fun (x0_8834:bool) ->
                                            fun (x1_8834:int) ->
                                              x_1746 iii11_2432
                                                (fun (x0_10053:bool) ->
                                                   fun (x1_10053:int) ->
                                                     k_append_x_8621 true x0_8834 x1_8834 true x0_10053 x1_10053 false
                                                       true 0))
                                     else
                                       let x_4356 (k_append_x_x_8841:(bool -> int -> X)) =
                                         x_8915 iii01_2432 k_append_x_x_8841
                                       in
                                       x_4356
                                         (fun (x0_8896:bool) ->
                                            fun (x1_8896:int) ->
                                              (let x_4366 (k_append_x_x_8853:(bool -> int -> X)) =
                                                 x_1746 iii11_2432 k_append_x_x_8853
                                               in
                                               x_4366
                                                 (fun (x0_8895:bool) ->
                                                    fun (x1_8895:int) ->
                                                      x_1747 iii21_2432
                                                        (fun (x0_10022:bool) ->
                                                           fun (x1_10022:int) ->
                                                             k_append_x_8621 true x0_8896 x1_8896 true x0_8895 x1_8895
                                                               true x0_10022 x1_10022))))
                               in
                               x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_8957:(unit -> X)) =
   let x_5625 (k_main_x_8970:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_8970 in
   x_5625
     (fun (x_9841:(int -> (bool -> int -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_8985:(bool -> int -> X)) = k_main_f_8985 false 0 in
         let
           x_1820 (ix00_2198:bool) (ix01_2198:int) (ix10_2198:bool) (ix11_2198:int) 
                 (k_main_x_8998:(bool ->
                                   bool ->
                                     r011_8997:int ->
                                       bool -> bool -> r111_8997:int[\r111_8997. r011_8997 = r111_8997] -> X)) =
           if ix00_2198 = false then
             let k_main_x_9047 (x0_10585:bool) (x10_10585:bool) (x11_10585:int) =
               k_main_x_8998 false true 0 x0_10585 x10_10585 x11_10585
             in
             if ix10_2198 = false then
               k_main_x_9047 false true 0
             else
               f_1479 ix11_2198 (fun (x0_10581:bool) -> fun (x1_10581:int) -> k_main_x_9047 true x0_10581 x1_10581)
           else
             if ix10_2198 = false then
               x_9841 ix01_2198
                 (fun (x0_10578:bool) -> fun (x1_10578:int) -> k_main_x_8998 true x0_10578 x1_10578 false true 0)
             else
               let x_5637 (k_main_x_x_9089:(bool -> int -> X)) = x_9841 ix01_2198 k_main_x_x_9089 in
               x_5637
                 (fun (x0_9123:bool) ->
                    fun (x1_9123:int) ->
                      f_1479 ix11_2198
                        (fun (x0_10560:bool) ->
                           fun (x1_10560:int) -> k_main_x_8998 true x0_9123 x1_9123 true x0_10560 x1_10560))
         in
         let
           x_5768
                 (k_main_x_9235:((bool ->
                                    int ->
                                      bool ->
                                        int ->
                                          bool ->
                                            int ->
                                              (bool ->
                                                 bool ->
                                                   r011_9232:int ->
                                                     bool ->
                                                       bool ->
                                                         r111_9232:int[\r111_9232. r011_9232 = r111_9232] ->
                                                           bool -> bool -> int -> X) -> X) -> X)) =
           append_1059 x_1820 k_main_x_9235
         in
         x_5768
           (fun (x_9821:(bool ->
                           int ->
                             bool ->
                               int ->
                                 bool ->
                                   int ->
                                     (bool ->
                                        bool ->
                                          r011_9819:int ->
                                            bool ->
                                              bool ->
                                                r111_9819:int[\r111_9819. r011_9819 = r111_9819] ->
                                                  bool -> bool -> int -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_9305:(bool -> int -> X)) =
                 x_9821 false 0 true i_2150 false 0
                   (fun (p00_10652:bool) ->
                      fun (p010_10652:bool) ->
                        fun (p011_10652:int) ->
                          fun (p10_10652:bool) ->
                            fun (p110_10652:bool) ->
                              fun (p111_10652:int) ->
                                fun (p20_10652:bool) ->
                                  fun (p210_10652:bool) -> fun (p211_10652:int) -> k_main_x_9305 p110_10652 p111_10652)
               in
               let x_1826 (i_2140:int) (k_main_x_9354:(bool -> int -> X)) =
                 x_9821 false 0 false 0 true i_2140
                   (fun (p00_10671:bool) ->
                      fun (p010_10671:bool) ->
                        fun (p011_10671:int) ->
                          fun (p10_10671:bool) ->
                            fun (p110_10671:bool) ->
                              fun (p111_10671:int) ->
                                fun (p20_10671:bool) ->
                                  fun (p210_10671:bool) -> fun (p211_10671:int) -> k_main_x_9354 p210_10671 p211_10671)
               in
               let rec x_x_4065 (x_4027:int) (x_4028:int) (k_main_x_x_9403:(bool -> int -> bool -> int -> X)) =
                 let
                   x_5862
                         (k_main_x_x_x_9436:(bool ->
                                               bool ->
                                                 r011_9435:int ->
                                                   bool ->
                                                     bool ->
                                                       r111_9435:int[\r111_9435. r011_9435 = r111_9435] ->
                                                         bool -> bool -> int -> X)) =
                   x_9821 false 0 true x_4027 false 0 k_main_x_x_x_9436
                 in
                 x_5862
                   (fun (x00_9490:bool) ->
                      fun (x010_9490:bool) ->
                        fun (x011_9490:int) ->
                          fun (x10_9490:bool) ->
                            fun (x110_9490:bool) ->
                              fun (x111_9490:int) ->
                                fun (x20_9490:bool) ->
                                  fun (x210_9490:bool) ->
                                    fun (x211_9490:int) ->
                                      x_9821 false 0 false 0 true x_4028
                                        (fun (p00_10707:bool) ->
                                           fun (p010_10707:bool) ->
                                             fun (p011_10707:int) ->
                                               fun (p10_10707:bool) ->
                                                 fun (p110_10707:bool) ->
                                                   fun (p111_10707:int) ->
                                                     fun (p20_10707:bool) ->
                                                       fun (p210_10707:bool) ->
                                                         fun (p211_10707:int) ->
                                                           k_main_x_x_9403 x110_9490 x111_9490 p210_10707 p211_10707))
               in
               let
                 x_1829 (ii00_2123:bool) (ii01_2123:int) (ii10_2123:bool) (ii11_2123:int) 
                       (k_main_x_9498:(bool ->
                                         bool ->
                                           r011_9497:int ->
                                             bool -> bool -> r111_9497:int[\r111_9497. r011_9497 = r111_9497] -> X)) =
                 if ii00_2123 = false then
                   let k_main_x_9547 (x0_10734:bool) (x10_10734:bool) (x11_10734:int) =
                     k_main_x_9498 false true 0 x0_10734 x10_10734 x11_10734
                   in
                   if ii10_2123 = false then
                     k_main_x_9547 false true 0
                   else
                     x_1826 ii11_2123
                       (fun (x0_10730:bool) -> fun (x1_10730:int) -> k_main_x_9547 true x0_10730 x1_10730)
                 else
                   if ii10_2123 = false then
                     x_1825 ii01_2123
                       (fun (x0_10727:bool) -> fun (x1_10727:int) -> k_main_x_9498 true x0_10727 x1_10727 false true 0)
                   else
                     let x_5892 (k_main_x_x_9590:(bool -> int -> bool -> int -> X)) =
                       x_x_4065 ii01_2123 ii11_2123 k_main_x_x_9590
                     in
                     x_5892
                       (fun (x00_9614:bool) ->
                          fun (x01_9614:int) ->
                            fun (x10_9614:bool) ->
                              fun (x11_9614:int) -> k_main_x_9498 true x00_9614 x01_9614 true x10_9614 x11_9614)
               in
               let
                 x_6069
                       (k_main_x_9740:(bool ->
                                         bool ->
                                           r011_9739:int ->
                                             bool ->
                                               bool ->
                                                 r111_9739:int[\r111_9739. r011_9739 = r111_9739] ->
                                                   bool -> bool -> int -> X)) =
                 x_9821 true i_1016 false 0 false 0 k_main_x_9740
               in
               x_6069
                 (fun (x00_9788:bool) ->
                    fun (x010_9788:bool) ->
                      fun (x011_9788:int) ->
                        fun (x10_9788:bool) ->
                          fun (x110_9788:bool) ->
                            fun (x111_9788:int) ->
                              fun (x20_9788:bool) ->
                                fun (x210_9788:bool) ->
                                  fun (x211_9788:int) ->
                                    (let x_6046 (k_main_x_9752:(bool -> int -> X)) = x_9841 i_1016 k_main_x_9752 in
                                     x_6046
                                       (fun (x0_9787:bool) ->
                                          fun (x1_9787:int) ->
                                            (let n_1504 (k_main_n_9763:(int -> X)) =
                                               if x010_9788 <> false then
                                                 k_main_n_9763 x011_9788
                                               else
                                                 _|_
                                             in
                                             n_1504
                                               (fun (n_9786:int) ->
                                                  (let n_1505 (k_main_n_9771:(int -> X)) =
                                                     if x0_9787 <> false then
                                                       k_main_n_9771 x1_9787
                                                     else
                                                       _|_
                                                   in
                                                   n_1505
                                                     (fun (n_9785:int) ->
                                                        (if n_9786 = n_9785 then
                                                           k_main_8957 ()
                                                         else
                                                           {|fail|} () k_main_8957))))))))))))
 in
 let x_6064 (k_x_9852:(int -> X)) = rand_int_cps () k_x_9852 in
 x_6064
   (fun (x_9897:int) ->
      (let x_6066 (k_x_9864:(int -> X)) = rand_int_cps () k_x_9864 in
       x_6066
         (fun (x_9896:int) ->
            (let x_6068 (k_x_9885:(unit -> X)) = main_1015 x_9897 x_9896 k_x_9885 in
             x_6068 (fun (x_9891:unit) -> {end})))))

