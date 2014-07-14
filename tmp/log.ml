MoCHi: Model Checker for Higher-Order Programs
  Build: _726f60f (after 2014-07-14 17:35:32 +0900)
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
TUPLE: (true, x_1625 (snd (fst ii_3086))), (true, xs_1222 (snd (snd ii_3086)))
x_1625
xs_1222
TUPLE: (true, xs'_1014 (snd (fst ii_3004))), (true, x_1747 (snd (snd ii_3004)))
xs'_1014
x_1747
compose: xs'_1014, let x_3159 =
                     x_1023
                       (let x1_3795 = let x1_3787 = true in
                                      let x2_3788 = x_3785 + 1 in
                                      (x1_3787, x2_3788) in
                        let x2_3796 = let x1_3791 = false in
                                      let x2_3792 = 0 in
                                      (x1_3791, x2_3792) in
                        (x1_3795, x2_3796))
                   in
                   let x_1764 = snd (fst x_3159) in
                   x_1764; x_1747, snd
                                   (snd
                                    (x_1023
                                      (let x1_3807 = let x1_3799 = false in
                                                     let x2_3800 = 0 in
                                                     (x1_3799, x2_3800) in
                                       let x2_3808 = let x1_3803 = true in
                                                     let x2_3804 = x_3786 in
                                                     (x1_3803, x2_3804) in
                                       (x1_3807, x2_3808)))); 
PB: x:xs'_1014
CHECK: x_1764
CHECK: snd (fst x_3159)
CHECK: x_1023
         (let x1_3795 = let x1_3787 = true in
                        let x2_3788 = x_3785 + 1 in
                        (x1_3787, x2_3788) in
          let x2_3796 = let x1_3791 = false in
                        let x2_3792 = 0 in
                        (x1_3791, x2_3792) in
          (x1_3795, x2_3796))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3807 = let x1_3799 = false in
                         let x2_3800 = 0 in
                         (x1_3799, x2_3800) in
           let x2_3808 = let x1_3803 = true in
                         let x2_3804 = x_3786 in
                         (x1_3803, x2_3804) in
           (x1_3807, x2_3808))))
compose_let
xs'_1014:let x_3159 =
           x_1023
             (let x1_3795 = let x1_3787 = true in
                            let x2_3788 = x_3785 + 1 in
                            (x1_3787, x2_3788) in
              let x2_3796 = let x1_3791 = false in
                            let x2_3792 = 0 in
                            (x1_3791, x2_3792) in
              (x1_3795, x2_3796))
         in
         let x_1764 = snd (fst x_3159) in
         x_1764

x_1747:snd
       (snd
        (x_1023
          (let x1_3807 = let x1_3799 = false in
                         let x2_3800 = 0 in
                         (x1_3799, x2_3800) in
           let x2_3808 = let x1_3803 = true in
                         let x2_3804 = x_3786 in
                         (x1_3803, x2_3804) in
           (x1_3807, x2_3808))))

ADD: xs'_x_3811
TUPLE: (true, x_1793 (snd (fst ii_2929))), (true, x_1794 (snd (snd ii_2929)))
x_1793
x_1794
compose: x_1793, snd
                 (#1
                  (x_1791
                    (let x1_3839 = let x1_3827 = false in
                                   let x2_3828 = 0 in
                                   (x1_3827, x2_3828) in
                     let x2_3840 = let x1_3831 = true in
                                   let x2_3832 = x_3825 in
                                   (x1_3831, x2_3832) in
                     let x3_3841 = let x1_3835 = false in
                                   let x2_3836 = 0 in
                                   (x1_3835, x2_3836) in
                     (x1_3839, x2_3840, x3_3841)))); x_1794, snd
                                                             (#2
                                                              (x_1791
                                                                (let x1_3857 =
                                                                   let x1_3845 = false in
                                                                   let x2_3846 = 0 in
                                                                   (x1_3845, x2_3846)
                                                                 in
                                                                 let x2_3858 =
                                                                   let x1_3849 = false in
                                                                   let x2_3850 = 0 in
                                                                   (x1_3849, x2_3850)
                                                                 in
                                                                 let x3_3859 =
                                                                   let x1_3853 = true in
                                                                   let x2_3854 = x_3826 in
                                                                   (x1_3853, x2_3854)
                                                                 in
                                                                 (x1_3857, x2_3858, x3_3859)))); 
PB: x:x_1793
CHECK: snd
       (#1
        (x_1791
          (let x1_3839 = let x1_3827 = false in
                         let x2_3828 = 0 in
                         (x1_3827, x2_3828) in
           let x2_3840 = let x1_3831 = true in
                         let x2_3832 = x_3825 in
                         (x1_3831, x2_3832) in
           let x3_3841 = let x1_3835 = false in
                         let x2_3836 = 0 in
                         (x1_3835, x2_3836) in
           (x1_3839, x2_3840, x3_3841))))
PB: x:x_1794
CHECK: snd
       (#2
        (x_1791
          (let x1_3857 = let x1_3845 = false in
                         let x2_3846 = 0 in
                         (x1_3845, x2_3846) in
           let x2_3858 = let x1_3849 = false in
                         let x2_3850 = 0 in
                         (x1_3849, x2_3850) in
           let x3_3859 = let x1_3853 = true in
                         let x2_3854 = x_3826 in
                         (x1_3853, x2_3854) in
           (x1_3857, x2_3858, x3_3859))))
compose_let
x_1793:snd
       (#1
        (x_1791
          (let x1_3839 = let x1_3827 = false in
                         let x2_3828 = 0 in
                         (x1_3827, x2_3828) in
           let x2_3840 = let x1_3831 = true in
                         let x2_3832 = x_3825 in
                         (x1_3831, x2_3832) in
           let x3_3841 = let x1_3835 = false in
                         let x2_3836 = 0 in
                         (x1_3835, x2_3836) in
           (x1_3839, x2_3840, x3_3841))))

x_1794:snd
       (#2
        (x_1791
          (let x1_3857 = let x1_3845 = false in
                         let x2_3846 = 0 in
                         (x1_3845, x2_3846) in
           let x2_3858 = let x1_3849 = false in
                         let x2_3850 = 0 in
                         (x1_3849, x2_3850) in
           let x3_3859 = let x1_3853 = true in
                         let x2_3854 = x_3826 in
                         (x1_3853, x2_3854) in
           (x1_3857, x2_3858, x3_3859))))

ADD: x_x_3863
TUPLE: (true, x_1802 (snd (#0 iii_2861))), (true, x_1746 (snd (#1 iii_2861))), (true, x_1747 (snd (#2 iii_2861)))
x_1802
x_1746
x_1747
compose: x_1802, snd
                 (fst
                  (x_1801
                    (let x1_3888 = let x1_3880 = true in
                                   let x2_3881 = x_3877 in
                                   (x1_3880, x2_3881) in
                     let x2_3889 = let x1_3884 = false in
                                   let x2_3885 = 0 in
                                   (x1_3884, x2_3885) in
                     (x1_3888, x2_3889)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_3900 =
                                                          let x1_3892 = true in
                                                          let x2_3893 = x_3878 in
                                                          (x1_3892, x2_3893)
                                                        in
                                                        let x2_3901 =
                                                          let x1_3896 = false in
                                                          let x2_3897 = 0 in
                                                          (x1_3896, x2_3897)
                                                        in
                                                        (x1_3900, x2_3901)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_3912 = let x1_3904 = false in
                  let x2_3905 = 0 in
                  (x1_3904, x2_3905) in
    let x2_3913 = let x1_3908 = true in
                  let x2_3909 = x_3879 in
                  (x1_3908, x2_3909) in
    (x1_3912, x2_3913)))); 
PB: x:x_1802
CHECK: snd
       (fst
        (x_1801
          (let x1_3888 = let x1_3880 = true in
                         let x2_3881 = x_3877 in
                         (x1_3880, x2_3881) in
           let x2_3889 = let x1_3884 = false in
                         let x2_3885 = 0 in
                         (x1_3884, x2_3885) in
           (x1_3888, x2_3889))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3900 = let x1_3892 = true in
                         let x2_3893 = x_3878 in
                         (x1_3892, x2_3893) in
           let x2_3901 = let x1_3896 = false in
                         let x2_3897 = 0 in
                         (x1_3896, x2_3897) in
           (x1_3900, x2_3901))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3912 = let x1_3904 = false in
                         let x2_3905 = 0 in
                         (x1_3904, x2_3905) in
           let x2_3913 = let x1_3908 = true in
                         let x2_3909 = x_3879 in
                         (x1_3908, x2_3909) in
           (x1_3912, x2_3913))))
compose_let
x_1802:snd
       (fst
        (x_1801
          (let x1_3888 = let x1_3880 = true in
                         let x2_3881 = x_3877 in
                         (x1_3880, x2_3881) in
           let x2_3889 = let x1_3884 = false in
                         let x2_3885 = 0 in
                         (x1_3884, x2_3885) in
           (x1_3888, x2_3889))))

x_1746:snd
       (fst
        (x_1023
          (let x1_3900 = let x1_3892 = true in
                         let x2_3893 = x_3878 in
                         (x1_3892, x2_3893) in
           let x2_3901 = let x1_3896 = false in
                         let x2_3897 = 0 in
                         (x1_3896, x2_3897) in
           (x1_3900, x2_3901))))

x_1747:snd
       (snd
        (x_1023
          (let x1_3912 = let x1_3904 = false in
                         let x2_3905 = 0 in
                         (x1_3904, x2_3905) in
           let x2_3913 = let x1_3908 = true in
                         let x2_3909 = x_3879 in
                         (x1_3908, x2_3909) in
           (x1_3912, x2_3913))))

ADD: x_x_x_3916
TUPLE: (true, x_1747 (snd (#0 iii_3257))), (true, x_1746 (snd (#1 iii_3257))), (true, x_1747 (snd (#2 iii_3257)))
x_1747
x_1746
x_1747
compose: x_1747, snd
                 (snd
                  (x_1023
                    (let x1_3947 = let x1_3939 = false in
                                   let x2_3940 = 0 in
                                   (x1_3939, x2_3940) in
                     let x2_3948 = let x1_3943 = true in
                                   let x2_3944 = x_3936 in
                                   (x1_3943, x2_3944) in
                     (x1_3947, x2_3948)))); x_1746, snd
                                                    (fst
                                                     (x_1023
                                                       (let x1_3959 =
                                                          let x1_3951 = true in
                                                          let x2_3952 = x_3937 in
                                                          (x1_3951, x2_3952)
                                                        in
                                                        let x2_3960 =
                                                          let x1_3955 = false in
                                                          let x2_3956 = 0 in
                                                          (x1_3955, x2_3956)
                                                        in
                                                        (x1_3959, x2_3960)))); x_1747, 
snd
(snd
 (x_1023
   (let x1_3971 = let x1_3963 = false in
                  let x2_3964 = 0 in
                  (x1_3963, x2_3964) in
    let x2_3972 = let x1_3967 = true in
                  let x2_3968 = x_3938 in
                  (x1_3967, x2_3968) in
    (x1_3971, x2_3972)))); 
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3947 = let x1_3939 = false in
                         let x2_3940 = 0 in
                         (x1_3939, x2_3940) in
           let x2_3948 = let x1_3943 = true in
                         let x2_3944 = x_3936 in
                         (x1_3943, x2_3944) in
           (x1_3947, x2_3948))))
PB: x:x_1746
CHECK: snd
       (fst
        (x_1023
          (let x1_3959 = let x1_3951 = true in
                         let x2_3952 = x_3937 in
                         (x1_3951, x2_3952) in
           let x2_3960 = let x1_3955 = false in
                         let x2_3956 = 0 in
                         (x1_3955, x2_3956) in
           (x1_3959, x2_3960))))
PB: x:x_1747
CHECK: snd
       (snd
        (x_1023
          (let x1_3971 = let x1_3963 = false in
                         let x2_3964 = 0 in
                         (x1_3963, x2_3964) in
           let x2_3972 = let x1_3967 = true in
                         let x2_3968 = x_3938 in
                         (x1_3967, x2_3968) in
           (x1_3971, x2_3972))))
compose_let
x_1747:snd
       (snd
        (x_1023
          (let x1_3947 = let x1_3939 = false in
                         let x2_3940 = 0 in
                         (x1_3939, x2_3940) in
           let x2_3948 = let x1_3943 = true in
                         let x2_3944 = x_3936 in
                         (x1_3943, x2_3944) in
           (x1_3947, x2_3948))))

x_1746:snd
       (fst
        (x_1023
          (let x1_3959 = let x1_3951 = true in
                         let x2_3952 = x_3937 in
                         (x1_3951, x2_3952) in
           let x2_3960 = let x1_3955 = false in
                         let x2_3956 = 0 in
                         (x1_3955, x2_3956) in
           (x1_3959, x2_3960))))

x_1747:snd
       (snd
        (x_1023
          (let x1_3971 = let x1_3963 = false in
                         let x2_3964 = 0 in
                         (x1_3963, x2_3964) in
           let x2_3972 = let x1_3967 = true in
                         let x2_3968 = x_3938 in
                         (x1_3967, x2_3968) in
           (x1_3971, x2_3972))))

ADD: x_x_x_3975
TUPLE: (true, x_1812 (snd (fst ix_2198))), (true, f_1479 (snd (snd ix_2198)))
x_1812
TUPLE: (true, x_1825 (snd (fst ii_2123))), (true, x_1826 (snd (snd ii_2123)))
x_1825
x_1826
compose: x_1825, snd
                 (#1
                  (x_1823
                    (let x1_4009 = let x1_3997 = false in
                                   let x2_3998 = 0 in
                                   (x1_3997, x2_3998) in
                     let x2_4010 = let x1_4001 = true in
                                   let x2_4002 = x_3995 in
                                   (x1_4001, x2_4002) in
                     let x3_4011 = let x1_4005 = false in
                                   let x2_4006 = 0 in
                                   (x1_4005, x2_4006) in
                     (x1_4009, x2_4010, x3_4011)))); x_1826, snd
                                                             (#2
                                                              (x_1823
                                                                (let x1_4027 =
                                                                   let x1_4015 = false in
                                                                   let x2_4016 = 0 in
                                                                   (x1_4015, x2_4016)
                                                                 in
                                                                 let x2_4028 =
                                                                   let x1_4019 = false in
                                                                   let x2_4020 = 0 in
                                                                   (x1_4019, x2_4020)
                                                                 in
                                                                 let x3_4029 =
                                                                   let x1_4023 = true in
                                                                   let x2_4024 = x_3996 in
                                                                   (x1_4023, x2_4024)
                                                                 in
                                                                 (x1_4027, x2_4028, x3_4029)))); 
PB: x:x_1825
CHECK: snd
       (#1
        (x_1823
          (let x1_4009 = let x1_3997 = false in
                         let x2_3998 = 0 in
                         (x1_3997, x2_3998) in
           let x2_4010 = let x1_4001 = true in
                         let x2_4002 = x_3995 in
                         (x1_4001, x2_4002) in
           let x3_4011 = let x1_4005 = false in
                         let x2_4006 = 0 in
                         (x1_4005, x2_4006) in
           (x1_4009, x2_4010, x3_4011))))
PB: x:x_1826
CHECK: snd
       (#2
        (x_1823
          (let x1_4027 = let x1_4015 = false in
                         let x2_4016 = 0 in
                         (x1_4015, x2_4016) in
           let x2_4028 = let x1_4019 = false in
                         let x2_4020 = 0 in
                         (x1_4019, x2_4020) in
           let x3_4029 = let x1_4023 = true in
                         let x2_4024 = x_3996 in
                         (x1_4023, x2_4024) in
           (x1_4027, x2_4028, x3_4029))))
compose_let
x_1825:snd
       (#1
        (x_1823
          (let x1_4009 = let x1_3997 = false in
                         let x2_3998 = 0 in
                         (x1_3997, x2_3998) in
           let x2_4010 = let x1_4001 = true in
                         let x2_4002 = x_3995 in
                         (x1_4001, x2_4002) in
           let x3_4011 = let x1_4005 = false in
                         let x2_4006 = 0 in
                         (x1_4005, x2_4006) in
           (x1_4009, x2_4010, x3_4011))))

x_1826:snd
       (#2
        (x_1823
          (let x1_4027 = let x1_4015 = false in
                         let x2_4016 = 0 in
                         (x1_4015, x2_4016) in
           let x2_4028 = let x1_4019 = false in
                         let x2_4020 = 0 in
                         (x1_4019, x2_4020) in
           let x3_4029 = let x1_4023 = true in
                         let x2_4024 = x_3996 in
                         (x1_4023, x2_4024) in
           (x1_4027, x2_4028, x3_4029))))

ADD: x_x_4033
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
  let rec x_x_x_3975 x_3936 x_3937 x_3938 =
    let r_3979 =
      snd
      (snd
       (x_1023
         (let x1_3947 = let x1_3939 = false in
                        let x2_3940 = 0 in
                        (x1_3939, x2_3940) in
          let x2_3948 = let x1_3943 = true in
                        let x2_3944 = x_3936 in
                        (x1_3943, x2_3944) in
          (x1_3947, x2_3948))))
    in
    let r_3980 =
      snd
      (fst
       (x_1023
         (let x1_3959 = let x1_3951 = true in
                        let x2_3952 = x_3937 in
                        (x1_3951, x2_3952) in
          let x2_3960 = let x1_3955 = false in
                        let x2_3956 = 0 in
                        (x1_3955, x2_3956) in
          (x1_3959, x2_3960))))
    in
    let r_3981 =
      snd
      (snd
       (x_1023
         (let x1_3971 = let x1_3963 = false in
                        let x2_3964 = 0 in
                        (x1_3963, x2_3964) in
          let x2_3972 = let x1_3967 = true in
                        let x2_3968 = x_3938 in
                        (x1_3967, x2_3968) in
          (x1_3971, x2_3972))))
    in
    (r_3979, r_3980, r_3981)
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
            let r_3985 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 r_3985), (true, #1 r_3985), (true, #2 r_3985))
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
      let rec xs'_x_3811 x_3785 x_3786 =
        let x_3159 =
          x_1023
            (let x1_3795 = let x1_3787 = true in
                           let x2_3788 = x_3785 + 1 in
                           (x1_3787, x2_3788) in
             let x2_3796 = let x1_3791 = false in
                           let x2_3792 = 0 in
                           (x1_3791, x2_3792) in
             (x1_3795, x2_3796))
        in
        let x_1764 = snd (fst x_3159) in
        let r_3814 = x_1764 in
        let r_3815 =
          snd
          (snd
           (x_1023
             (let x1_3807 = let x1_3799 = false in
                            let x2_3800 = 0 in
                            (x1_3799, x2_3800) in
              let x2_3808 = let x1_3803 = true in
                            let x2_3804 = x_3786 in
                            (x1_3803, x2_3804) in
              (x1_3807, x2_3808))))
        in
        (r_3814, r_3815)
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
            let r_3818 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst r_3818), (true, snd r_3818))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_1791 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_1791 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_1791 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_1791 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3863 x_3825 x_3826 =
        let r_3866 =
          snd
          (#1
           (x_1791
             (let x1_3839 = let x1_3827 = false in
                            let x2_3828 = 0 in
                            (x1_3827, x2_3828) in
              let x2_3840 = let x1_3831 = true in
                            let x2_3832 = x_3825 in
                            (x1_3831, x2_3832) in
              let x3_3841 = let x1_3835 = false in
                            let x2_3836 = 0 in
                            (x1_3835, x2_3836) in
              (x1_3839, x2_3840, x3_3841))))
        in
        let r_3867 =
          snd
          (#2
           (x_1791
             (let x1_3857 = let x1_3845 = false in
                            let x2_3846 = 0 in
                            (x1_3845, x2_3846) in
              let x2_3858 = let x1_3849 = false in
                            let x2_3850 = 0 in
                            (x1_3849, x2_3850) in
              let x3_3859 = let x1_3853 = true in
                            let x2_3854 = x_3826 in
                            (x1_3853, x2_3854) in
              (x1_3857, x2_3858, x3_3859))))
        in
        (r_3866, r_3867)
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
            let r_3870 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst r_3870), (true, snd r_3870))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_1800 = cons_1225 (snd x_1767) in
      let x_1801 = x_1800 x_1792 in
      let x_1802 i_2893 = snd (fst (x_1801 ((true, i_2893), (false, 0)))) in
      let rec x_x_x_3916 x_3877 x_3878 x_3879 =
        let r_3920 =
          snd
          (fst
           (x_1801
             (let x1_3888 = let x1_3880 = true in
                            let x2_3881 = x_3877 in
                            (x1_3880, x2_3881) in
              let x2_3889 = let x1_3884 = false in
                            let x2_3885 = 0 in
                            (x1_3884, x2_3885) in
              (x1_3888, x2_3889))))
        in
        let r_3921 =
          snd
          (fst
           (x_1023
             (let x1_3900 = let x1_3892 = true in
                            let x2_3893 = x_3878 in
                            (x1_3892, x2_3893) in
              let x2_3901 = let x1_3896 = false in
                            let x2_3897 = 0 in
                            (x1_3896, x2_3897) in
              (x1_3900, x2_3901))))
        in
        let r_3922 =
          snd
          (snd
           (x_1023
             (let x1_3912 = let x1_3904 = false in
                            let x2_3905 = 0 in
                            (x1_3904, x2_3905) in
              let x2_3913 = let x1_3908 = true in
                            let x2_3909 = x_3879 in
                            (x1_3908, x2_3909) in
              (x1_3912, x2_3913))))
        in
        (r_3920, r_3921, r_3922)
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
              let r_3926 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 r_3926), (true, #1 r_3926), (true, #2 r_3926))
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
  let rec x_x_4033 x_3995 x_3996 =
    let r_4036 =
      snd
      (#1
       (x_1823
         (let x1_4009 = let x1_3997 = false in
                        let x2_3998 = 0 in
                        (x1_3997, x2_3998) in
          let x2_4010 = let x1_4001 = true in
                        let x2_4002 = x_3995 in
                        (x1_4001, x2_4002) in
          let x3_4011 = let x1_4005 = false in
                        let x2_4006 = 0 in
                        (x1_4005, x2_4006) in
          (x1_4009, x2_4010, x3_4011))))
    in
    let r_4037 =
      snd
      (#2
       (x_1823
         (let x1_4027 = let x1_4015 = false in
                        let x2_4016 = 0 in
                        (x1_4015, x2_4016) in
          let x2_4028 = let x1_4019 = false in
                        let x2_4020 = 0 in
                        (x1_4019, x2_4020) in
          let x3_4029 = let x1_4023 = true in
                        let x2_4024 = x_3996 in
                        (x1_4023, x2_4024) in
          (x1_4027, x2_4028, x3_4029))))
    in
    (r_4036, r_4037)
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
        let r_4040 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst r_4040), (true, snd r_4040))
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
          let x_4049 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4049
                               else
                                 xs_1114 n_1526 in
          x_4049
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4131 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4131))
        else
          if fst (snd ii_3523) = false then
            let x_4090 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4090), (false, (true, 0)))
          else
            let x_4067 = x_1592 (snd (fst ii_3523)) in
            let x_4077 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4067), (true, x_4077))
      in
      x_1732
    in
    let x_4177 = make_list_1008 (n_1009 - 1) in
    let x_4179 = rand_int () in
    let x_4180 = cons_1117 x_4179 in
    let x_4181 = x_4180 x_4177 in
    let x_1739 i_3447 = let x_4198 = x_4181 ((true, i_3447), (false, 0)) in
                        snd (fst x_4198) in
    let x_1740 i_3440 = let x_4217 = x_4181 ((false, 0), (true, i_3440)) in
                        snd (snd x_4217) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_4243 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_4243) in
  let x_1747 i_3303 = let x_4262 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_4262) in
  let rec x_x_x_3975 x_3936 x_3937 x_3938 =
    let x_4276 = x_1023 ((false, 0), (true, x_3936)) in
    let x_4290 = x_1023 ((true, x_3937), (false, 0)) in
    let x_4304 = x_1023 ((false, 0), (true, x_3938)) in
    (snd (snd x_4276), snd (fst x_4290), snd (snd x_4304))
  in
  let x_4326 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_4326)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5949 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_5949))
        else
          if fst (#2 iii_3257) = false then
            let x_5896 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_5896), (false, (true, 0)))
          else
            let x_5861 = x_1746 (snd (#1 iii_3257)) in
            let x_5871 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, x_5861), (true, x_5871))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_5802 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5802), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5767 = x_1747 (snd (#0 iii_3257)) in
            let x_5788 = x_1747 (snd (#2 iii_3257)) in
            ((true, x_5767), (false, (true, 0)), (true, x_5788))
        else
          if fst (#2 iii_3257) = false then
            let x_5726 = x_1747 (snd (#0 iii_3257)) in
            let x_5736 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5726), (true, x_5736), (false, (true, 0)))
          else
            let x_5694 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5694), (true, #1 x_5694), (true, #2 x_5694))
    in
    x_1811
  else
    if fst (snd (fst x_4326)) <> false then
      let xs'_1014 x_1150 = let x_4676 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_4676) in
      let rec xs'_x_3811 x_3785 x_3786 =
        let x_4691 = x_1023 ((true, x_3785 + 1), (false, 0)) in
        let x_4706 = x_1023 ((false, 0), (true, x_3786)) in
        (snd (fst x_4691), snd (snd x_4706))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4717 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4717
                                 else
                                   xs_1222 n_1544 in
            x_4717
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_4799 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_4799))
          else
            if fst (snd ii_3086) = false then
              let x_4758 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4758), (false, (true, 0)))
            else
              let x_4735 = x_1625 (snd (fst ii_3086)) in
              let x_4745 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4735), (true, x_4745))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4914 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_4914))
        else
          if fst (snd ii_3004) = false then
            let x_4873 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4873), (false, (true, 0)))
          else
            let x_4849 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4849), (true, snd x_4849))
      in
      let x_1789 i_2984 = let x_4974 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_4974) in
      let x_1790 i_2977 = let x_4993 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_4993) in
      let x_4996 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_5020 = x_4996 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_5020) in
      let x_1793 i_2956 = let x_5046 = x_4996 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_5046) in
      let x_1794 i_2946 = let x_5072 = x_4996 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_5072) in
      let rec x_x_3863 x_3825 x_3826 =
        let x_5090 = x_4996 ((false, 0), (true, x_3825), (false, 0)) in
        let x_5108 = x_4996 ((false, 0), (false, 0), (true, x_3826)) in
        (snd (#1 x_5090), snd (#2 x_5108))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5185 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5185))
        else
          if fst (snd ii_2929) = false then
            let x_5144 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5144), (false, (true, 0)))
          else
            let x_5120 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5120), (true, snd x_5120))
      in
      let x_1798 i_2909 = let x_5245 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_5245) in
      let x_1799 i_2902 = let x_5264 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_5264) in
      let x_5268 = cons_1225 (snd (snd (fst x_4326))) in
      let x_5269 = x_5268 x_1792 in
      let x_1802 i_2893 = let x_5286 = x_5269 ((true, i_2893), (false, 0)) in
                          snd (fst x_5286) in
      let rec x_x_x_3916 x_3877 x_3878 x_3879 =
        let x_5300 = x_5269 ((true, x_3877), (false, 0)) in
        let x_5314 = x_1023 ((true, x_3878), (false, 0)) in
        let x_5328 = x_1023 ((false, 0), (true, x_3879)) in
        (snd (fst x_5300), snd (fst x_5314), snd (snd x_5328))
      in
      let x_1803 i_2886 = let x_5351 = x_5269 ((false, 0), (true, i_2886)) in
                          snd (snd x_5351) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5618 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5618))
          else
            if fst (#2 iii_2861) = false then
              let x_5565 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_5565), (false, (true, 0)))
            else
              let x_5530 = x_1746 (snd (#1 iii_2861)) in
              let x_5540 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, x_5530), (true, x_5540))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_5471 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5471), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5436 = x_1802 (snd (#0 iii_2861)) in
              let x_5457 = x_1747 (snd (#2 iii_2861)) in
              ((true, x_5436), (false, (true, 0)), (true, x_5457))
          else
            if fst (#2 iii_2861) = false then
              let x_5395 = x_1802 (snd (#0 iii_2861)) in
              let x_5405 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5395), (true, x_5405), (false, (true, 0)))
            else
              let x_5363 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5363), (true, #1 x_5363), (true, #2 x_5363))
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
              let x_4596 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4596))
          else
            if fst (#2 iii_2432) = false then
              let x_4543 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_4543), (false, (true, 0)))
            else
              let x_4508 = x_1746 (snd (#1 iii_2432)) in
              let x_4518 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, x_4508), (true, x_4518))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4449 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4449), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4414 = x_1610 (snd (#0 iii_2432)) in
              let x_4435 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4414), (false, (true, 0)), (true, x_4435))
          else
            if fst (#2 iii_2432) = false then
              let x_4373 = x_1610 (snd (#0 iii_2432)) in
              let x_4383 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4373), (true, x_4383), (false, (true, 0)))
            else
              let x_4339 = x_1610 (snd (#0 iii_2432)) in
              let x_4349 = x_1746 (snd (#1 iii_2432)) in
              let x_4359 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4339), (true, x_4349), (true, x_4359))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6016 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6092 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6092))
    else
      if fst (snd ix_2198) = false then
        let x_6051 = x_6016 (snd (fst ix_2198)) in
        ((true, x_6051), (false, (true, 0)))
      else
        let x_6028 = x_6016 (snd (fst ix_2198)) in
        let x_6038 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6028), (true, x_6038))
  in
  let x_1821 i_2178 = let x_6152 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_6152) in
  let x_1822 x_2171 = let x_6171 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_6171) in
  let x_6174 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_6198 = x_6174 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_6198) in
  let x_1825 i_2150 = let x_6224 = x_6174 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_6224) in
  let x_1826 i_2140 = let x_6250 = x_6174 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_6250) in
  let rec x_x_4033 x_3995 x_3996 =
    let x_6268 = x_6174 ((false, 0), (true, x_3995), (false, 0)) in
    let x_6286 = x_6174 ((false, 0), (false, 0), (true, x_3996)) in
    (snd (#1 x_6268), snd (#2 x_6286))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6363 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6363))
    else
      if fst (snd ii_2123) = false then
        let x_6322 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6322), (false, (true, 0)))
      else
        let x_6298 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6298), (true, snd x_6298))
  in
  let x_1830 i_2103 = let x_6423 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_6423) in
  let x_1831 i_2096 = let x_6442 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6442) in
  let x_6466 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6467 = x_6016 i_1016 in
  let n_1504 = if fst (snd (#0 x_6466)) <> false then
                 snd (snd (#0 x_6466))
               else
                 _|_ in
  let n_1505 = if fst x_6467 <> false then
                 snd x_6467
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6485 = rand_int () in
let x_6487 = rand_int () in
let x_6488 = main_1015 x_6485 in
let x_6489 = x_6488 x_6487 in
let x_1847 = x_6489 in
()

replace[1]: x_6466
APPS: x_6466 = x_6174 ...0... i_1016 ...
USED: x_6466 = x_6174 ...0... i_1016 ...
MUST: x_6466 = x_6174 ...0... i_1016 ...
NEW: x_6490 = x_6174 ((true, i_1016), (false, 0), (false, 0))
replace[1]: x_6442
APPS: x_6442 = x_1829 ...1... i_2096 ...
USED: x_6442 = x_1829 ...1... i_2096 ...
MUST: x_6442 = x_1829 ...1... i_2096 ...
NEW: x_6501 = x_1829 ((false, 0), (true, i_2096))
replace[1]: x_6423
APPS: x_6423 = x_1829 ...0... i_2103 ...
USED: x_6423 = x_1829 ...0... i_2103 ...
MUST: x_6423 = x_1829 ...0... i_2103 ...
NEW: x_6509 = x_1829 ((true, i_2103), (false, 0))
replace[2]: x_6268
APPS: x_6286 = x_6174 ...2... x_3996 ...
APPS: x_6268 = x_6174 ...1... x_3995 ...
USED: x_6286 = x_6174 ...2... x_3996 ...
USED: x_6268 = x_6174 ...1... x_3995 ...
MUST: x_6268 = x_6174 ...1... x_3995 ...
MUST: x_6286 = x_6174 ...2... x_3996 ...
NEW: x_6517 = x_6174 ((false, 0), (true, x_3995), (true, x_3996))
replace[1]: x_6250
APPS: x_6250 = x_6174 ...2... i_2140 ...
USED: x_6250 = x_6174 ...2... i_2140 ...
MUST: x_6250 = x_6174 ...2... i_2140 ...
NEW: x_6529 = x_6174 ((false, 0), (false, 0), (true, i_2140))
replace[1]: x_6224
APPS: x_6224 = x_6174 ...1... i_2150 ...
USED: x_6224 = x_6174 ...1... i_2150 ...
MUST: x_6224 = x_6174 ...1... i_2150 ...
NEW: x_6540 = x_6174 ((false, 0), (true, i_2150), (false, 0))
replace[1]: x_6198
APPS: x_6198 = x_6174 ...0... i_2160 ...
USED: x_6198 = x_6174 ...0... i_2160 ...
MUST: x_6198 = x_6174 ...0... i_2160 ...
NEW: x_6551 = x_6174 ((true, i_2160), (false, 0), (false, 0))
replace[1]: x_6171
APPS: x_6171 = x_1820 ...1... x_2171 ...
USED: x_6171 = x_1820 ...1... x_2171 ...
MUST: x_6171 = x_1820 ...1... x_2171 ...
NEW: x_6562 = x_1820 ((false, 0), (true, x_2171))
replace[1]: x_6152
APPS: x_6152 = x_1820 ...0... i_2178 ...
USED: x_6152 = x_1820 ...0... i_2178 ...
MUST: x_6152 = x_1820 ...0... i_2178 ...
NEW: x_6570 = x_1820 ((true, i_2178), (false, 0))
replace[1]: x_5351
APPS: x_5351 = x_5269 ...1... i_2886 ...
USED: x_5351 = x_5269 ...1... i_2886 ...
MUST: x_5351 = x_5269 ...1... i_2886 ...
NEW: x_6578 = x_5269 ((false, 0), (true, i_2886))
replace[2]: x_5314
APPS: x_5328 = x_1023 ...1... x_3879 ...
APPS: x_5314 = x_1023 ...0... x_3878 ...
APPS: x_4326 = x_1023 ...0... 0 ...
USED: x_5328 = x_1023 ...1... x_3879 ...
USED: x_5314 = x_1023 ...0... x_3878 ...
MUST: x_5314 = x_1023 ...0... x_3878 ...
MUST: x_5328 = x_1023 ...1... x_3879 ...
NEW: x_6586 = x_1023 ((true, x_3878), (true, x_3879))
replace[1]: x_5300
APPS: x_5300 = x_5269 ...0... x_3877 ...
USED: x_5300 = x_5269 ...0... x_3877 ...
MUST: x_5300 = x_5269 ...0... x_3877 ...
NEW: x_6595 = x_5269 ((true, x_3877), (false, 0))
replace[1]: x_5286
APPS: x_5286 = x_5269 ...0... i_2893 ...
USED: x_5286 = x_5269 ...0... i_2893 ...
MUST: x_5286 = x_5269 ...0... i_2893 ...
NEW: x_6603 = x_5269 ((true, i_2893), (false, 0))
replace[1]: x_5264
APPS: x_5264 = x_1797 ...1... i_2902 ...
USED: x_5264 = x_1797 ...1... i_2902 ...
MUST: x_5264 = x_1797 ...1... i_2902 ...
NEW: x_6611 = x_1797 ((false, 0), (true, i_2902))
replace[1]: x_5245
APPS: x_5245 = x_1797 ...0... i_2909 ...
USED: x_5245 = x_1797 ...0... i_2909 ...
MUST: x_5245 = x_1797 ...0... i_2909 ...
NEW: x_6619 = x_1797 ((true, i_2909), (false, 0))
replace[2]: x_5090
APPS: x_5108 = x_4996 ...2... x_3826 ...
APPS: x_5090 = x_4996 ...1... x_3825 ...
USED: x_5108 = x_4996 ...2... x_3826 ...
USED: x_5090 = x_4996 ...1... x_3825 ...
MUST: x_5090 = x_4996 ...1... x_3825 ...
MUST: x_5108 = x_4996 ...2... x_3826 ...
NEW: x_6627 = x_4996 ((false, 0), (true, x_3825), (true, x_3826))
replace[1]: x_5072
APPS: x_5072 = x_4996 ...2... i_2946 ...
USED: x_5072 = x_4996 ...2... i_2946 ...
MUST: x_5072 = x_4996 ...2... i_2946 ...
NEW: x_6639 = x_4996 ((false, 0), (false, 0), (true, i_2946))
replace[1]: x_5046
APPS: x_5046 = x_4996 ...1... i_2956 ...
USED: x_5046 = x_4996 ...1... i_2956 ...
MUST: x_5046 = x_4996 ...1... i_2956 ...
NEW: x_6650 = x_4996 ((false, 0), (true, i_2956), (false, 0))
replace[1]: x_5020
APPS: x_5020 = x_4996 ...0... i_2966 ...
USED: x_5020 = x_4996 ...0... i_2966 ...
MUST: x_5020 = x_4996 ...0... i_2966 ...
NEW: x_6661 = x_4996 ((true, i_2966), (false, 0), (false, 0))
replace[1]: x_4993
APPS: x_4993 = x_1788 ...1... i_2977 ...
USED: x_4993 = x_1788 ...1... i_2977 ...
MUST: x_4993 = x_1788 ...1... i_2977 ...
NEW: x_6672 = x_1788 ((false, 0), (true, i_2977))
replace[1]: x_4974
APPS: x_4974 = x_1788 ...0... i_2984 ...
USED: x_4974 = x_1788 ...0... i_2984 ...
MUST: x_4974 = x_1788 ...0... i_2984 ...
NEW: x_6680 = x_1788 ((true, i_2984), (false, 0))
replace[2]: x_4691
APPS: x_4706 = x_1023 ...1... x_3786 ...
APPS: x_4691 = x_1023 ...0... x_3785 + 1 ...
APPS: x_4326 = x_1023 ...0... 0 ...
USED: x_4706 = x_1023 ...1... x_3786 ...
USED: x_4691 = x_1023 ...0... x_3785 + 1 ...
MUST: x_4691 = x_1023 ...0... x_3785 + 1 ...
MUST: x_4706 = x_1023 ...1... x_3786 ...
NEW: x_6688 = x_1023 ((true, x_3785 + 1), (true, x_3786))
replace[1]: x_4676
APPS: x_4676 = x_1023 ...0... x_1150 + 1 ...
APPS: x_4326 = x_1023 ...0... 0 ...
USED: x_4676 = x_1023 ...0... x_1150 + 1 ...
MUST: x_4676 = x_1023 ...0... x_1150 + 1 ...
NEW: x_6697 = x_1023 ((true, x_1150 + 1), (false, 0))
replace[1]: x_4326
APPS: x_4326 = x_1023 ...0... 0 ...
USED: x_4326 = x_1023 ...0... 0 ...
MUST: x_4326 = x_1023 ...0... 0 ...
NEW: x_6705 = x_1023 ((true, 0), (false, 0))
replace[3]: x_4276
APPS: x_4304 = x_1023 ...1... x_3938 ...
APPS: x_4290 = x_1023 ...0... x_3937 ...
APPS: x_4276 = x_1023 ...1... x_3936 ...
USED: x_4304 = x_1023 ...1... x_3938 ...
USED: x_4290 = x_1023 ...0... x_3937 ...
USED: x_4276 = x_1023 ...1... x_3936 ...
MUST: x_4276 = x_1023 ...1... x_3936 ...
MUST: x_4290 = x_1023 ...0... x_3937 ...
MUST: x_4304 = x_1023 ...1... x_3938 ...
replace[2]: x_4290
APPS: x_4304 = x_1023 ...1... x_3938 ...
APPS: x_4290 = x_1023 ...0... x_3937 ...
USED: x_4304 = x_1023 ...1... x_3938 ...
USED: x_4290 = x_1023 ...0... x_3937 ...
MUST: x_4290 = x_1023 ...0... x_3937 ...
MUST: x_4304 = x_1023 ...1... x_3938 ...
NEW: x_6714 = x_1023 ((true, x_3937), (true, x_3938))
replace[1]: x_4262
APPS: x_4262 = x_1023 ...1... i_3303 ...
USED: x_4262 = x_1023 ...1... i_3303 ...
MUST: x_4262 = x_1023 ...1... i_3303 ...
NEW: x_6723 = x_1023 ((false, 0), (true, i_3303))
replace[1]: x_4243
APPS: x_4243 = x_1023 ...0... i_3310 ...
USED: x_4243 = x_1023 ...0... i_3310 ...
MUST: x_4243 = x_1023 ...0... i_3310 ...
NEW: x_6731 = x_1023 ((true, i_3310), (false, 0))
replace[1]: x_4217
APPS: x_4217 = x_4181 ...1... i_3440 ...
USED: x_4217 = x_4181 ...1... i_3440 ...
MUST: x_4217 = x_4181 ...1... i_3440 ...
NEW: x_6739 = x_4181 ((false, 0), (true, i_3440))
replace[1]: x_4198
APPS: x_4198 = x_4181 ...0... i_3447 ...
USED: x_4198 = x_4181 ...0... i_3447 ...
MUST: x_4198 = x_4181 ...0... i_3447 ...
NEW: x_6747 = x_4181 ((true, i_3447), (false, 0))
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
          let x_4049 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4049
                               else
                                 xs_1114 n_1526 in
          x_4049
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4131 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4131))
        else
          if fst (snd ii_3523) = false then
            let x_4090 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4090), (false, (true, 0)))
          else
            let x_4067 = x_1592 (snd (fst ii_3523)) in
            let x_4077 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4067), (true, x_4077))
      in
      x_1732
    in
    let x_4177 = make_list_1008 (n_1009 - 1) in
    let x_4179 = rand_int () in
    let x_4180 = cons_1117 x_4179 in
    let x_4181 = x_4180 x_4177 in
    let x_1739 i_3447 =
      let x_4198 = x_4181 ((true, i_3447), (false, 0)) in
      let x_6747 = x_4181 ((true, i_3447), (false, 0)) in
      snd (fst x_6747)
    in
    let x_1740 i_3440 =
      let x_4217 = x_4181 ((false, 0), (true, i_3440)) in
      let x_6739 = x_4181 ((false, 0), (true, i_3440)) in
      snd (snd x_6739)
    in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 =
    let x_4243 = x_1023 ((true, i_3310), (false, 0)) in
    let x_6731 = x_1023 ((true, i_3310), (false, 0)) in
    snd (fst x_6731)
  in
  let x_1747 i_3303 =
    let x_4262 = x_1023 ((false, 0), (true, i_3303)) in
    let x_6723 = x_1023 ((false, 0), (true, i_3303)) in
    snd (snd x_6723)
  in
  let rec x_x_x_3975 x_3936 x_3937 x_3938 =
    let x_4276 = x_1023 ((false, 0), (true, x_3936)) in
    let x_4290 = x_1023 ((true, x_3937), (false, 0)) in
    let x_4304 = x_1023 ((false, 0), (true, x_3938)) in
    let x_6714 = x_1023 ((true, x_3937), (true, x_3938)) in
    (snd (snd x_4276), snd (fst x_6714), snd (snd x_6714))
  in
  let x_4326 = x_1023 ((true, 0), (false, 0)) in
  let x_6705 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6705)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5949 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_5949))
        else
          if fst (#2 iii_3257) = false then
            let x_5896 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_5896), (false, (true, 0)))
          else
            let x_5861 = x_1746 (snd (#1 iii_3257)) in
            let x_5871 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, x_5861), (true, x_5871))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_5802 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5802), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5767 = x_1747 (snd (#0 iii_3257)) in
            let x_5788 = x_1747 (snd (#2 iii_3257)) in
            ((true, x_5767), (false, (true, 0)), (true, x_5788))
        else
          if fst (#2 iii_3257) = false then
            let x_5726 = x_1747 (snd (#0 iii_3257)) in
            let x_5736 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5726), (true, x_5736), (false, (true, 0)))
          else
            let x_5694 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5694), (true, #1 x_5694), (true, #2 x_5694))
    in
    x_1811
  else
    if fst (snd (fst x_6705)) <> false then
      let xs'_1014 x_1150 =
        let x_4676 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        let x_6697 = x_1023 ((true, x_1150 + 1), (false, 0)) in
        snd (fst x_6697)
      in
      let rec xs'_x_3811 x_3785 x_3786 =
        let x_4691 = x_1023 ((true, x_3785 + 1), (false, 0)) in
        let x_4706 = x_1023 ((false, 0), (true, x_3786)) in
        let x_6688 = x_1023 ((true, x_3785 + 1), (true, x_3786)) in
        (snd (fst x_6688), snd (snd x_6688))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4717 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4717
                                 else
                                   xs_1222 n_1544 in
            x_4717
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_4799 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_4799))
          else
            if fst (snd ii_3086) = false then
              let x_4758 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4758), (false, (true, 0)))
            else
              let x_4735 = x_1625 (snd (fst ii_3086)) in
              let x_4745 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4735), (true, x_4745))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4914 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_4914))
        else
          if fst (snd ii_3004) = false then
            let x_4873 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4873), (false, (true, 0)))
          else
            let x_4849 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4849), (true, snd x_4849))
      in
      let x_1789 i_2984 =
        let x_4974 = x_1788 ((true, i_2984), (false, 0)) in
        let x_6680 = x_1788 ((true, i_2984), (false, 0)) in
        snd (fst x_6680)
      in
      let x_1790 i_2977 =
        let x_4993 = x_1788 ((false, 0), (true, i_2977)) in
        let x_6672 = x_1788 ((false, 0), (true, i_2977)) in
        snd (snd x_6672)
      in
      let x_4996 = append_1059 x_1788 in
      let x_1792 i_2966 =
        let x_5020 = x_4996 ((true, i_2966), (false, 0), (false, 0)) in
        let x_6661 = x_4996 ((true, i_2966), (false, 0), (false, 0)) in
        snd (#0 x_6661)
      in
      let x_1793 i_2956 =
        let x_5046 = x_4996 ((false, 0), (true, i_2956), (false, 0)) in
        let x_6650 = x_4996 ((false, 0), (true, i_2956), (false, 0)) in
        snd (#1 x_6650)
      in
      let x_1794 i_2946 =
        let x_5072 = x_4996 ((false, 0), (false, 0), (true, i_2946)) in
        let x_6639 = x_4996 ((false, 0), (false, 0), (true, i_2946)) in
        snd (#2 x_6639)
      in
      let rec x_x_3863 x_3825 x_3826 =
        let x_5090 = x_4996 ((false, 0), (true, x_3825), (false, 0)) in
        let x_5108 = x_4996 ((false, 0), (false, 0), (true, x_3826)) in
        let x_6627 = x_4996 ((false, 0), (true, x_3825), (true, x_3826)) in
        (snd (#1 x_6627), snd (#2 x_6627))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5185 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5185))
        else
          if fst (snd ii_2929) = false then
            let x_5144 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5144), (false, (true, 0)))
          else
            let x_5120 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5120), (true, snd x_5120))
      in
      let x_1798 i_2909 =
        let x_5245 = x_1797 ((true, i_2909), (false, 0)) in
        let x_6619 = x_1797 ((true, i_2909), (false, 0)) in
        snd (fst x_6619)
      in
      let x_1799 i_2902 =
        let x_5264 = x_1797 ((false, 0), (true, i_2902)) in
        let x_6611 = x_1797 ((false, 0), (true, i_2902)) in
        snd (snd x_6611)
      in
      let x_5268 = cons_1225 (snd (snd (fst x_6705))) in
      let x_5269 = x_5268 x_1792 in
      let x_1802 i_2893 =
        let x_5286 = x_5269 ((true, i_2893), (false, 0)) in
        let x_6603 = x_5269 ((true, i_2893), (false, 0)) in
        snd (fst x_6603)
      in
      let rec x_x_x_3916 x_3877 x_3878 x_3879 =
        let x_5300 = x_5269 ((true, x_3877), (false, 0)) in
        let x_6595 = x_5269 ((true, x_3877), (false, 0)) in
        let x_5314 = x_1023 ((true, x_3878), (false, 0)) in
        let x_5328 = x_1023 ((false, 0), (true, x_3879)) in
        let x_6586 = x_1023 ((true, x_3878), (true, x_3879)) in
        (snd (fst x_6595), snd (fst x_6586), snd (snd x_6586))
      in
      let x_1803 i_2886 =
        let x_5351 = x_5269 ((false, 0), (true, i_2886)) in
        let x_6578 = x_5269 ((false, 0), (true, i_2886)) in
        snd (snd x_6578)
      in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5618 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5618))
          else
            if fst (#2 iii_2861) = false then
              let x_5565 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_5565), (false, (true, 0)))
            else
              let x_5530 = x_1746 (snd (#1 iii_2861)) in
              let x_5540 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, x_5530), (true, x_5540))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_5471 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5471), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5436 = x_1802 (snd (#0 iii_2861)) in
              let x_5457 = x_1747 (snd (#2 iii_2861)) in
              ((true, x_5436), (false, (true, 0)), (true, x_5457))
          else
            if fst (#2 iii_2861) = false then
              let x_5395 = x_1802 (snd (#0 iii_2861)) in
              let x_5405 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5395), (true, x_5405), (false, (true, 0)))
            else
              let x_5363 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5363), (true, #1 x_5363), (true, #2 x_5363))
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
              let x_4596 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4596))
          else
            if fst (#2 iii_2432) = false then
              let x_4543 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_4543), (false, (true, 0)))
            else
              let x_4508 = x_1746 (snd (#1 iii_2432)) in
              let x_4518 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, x_4508), (true, x_4518))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4449 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4449), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4414 = x_1610 (snd (#0 iii_2432)) in
              let x_4435 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4414), (false, (true, 0)), (true, x_4435))
          else
            if fst (#2 iii_2432) = false then
              let x_4373 = x_1610 (snd (#0 iii_2432)) in
              let x_4383 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4373), (true, x_4383), (false, (true, 0)))
            else
              let x_4339 = x_1610 (snd (#0 iii_2432)) in
              let x_4349 = x_1746 (snd (#1 iii_2432)) in
              let x_4359 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4339), (true, x_4349), (true, x_4359))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6016 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6092 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6092))
    else
      if fst (snd ix_2198) = false then
        let x_6051 = x_6016 (snd (fst ix_2198)) in
        ((true, x_6051), (false, (true, 0)))
      else
        let x_6028 = x_6016 (snd (fst ix_2198)) in
        let x_6038 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6028), (true, x_6038))
  in
  let x_1821 i_2178 =
    let x_6152 = x_1820 ((true, i_2178), (false, 0)) in
    let x_6570 = x_1820 ((true, i_2178), (false, 0)) in
    snd (fst x_6570)
  in
  let x_1822 x_2171 =
    let x_6171 = x_1820 ((false, 0), (true, x_2171)) in
    let x_6562 = x_1820 ((false, 0), (true, x_2171)) in
    snd (snd x_6562)
  in
  let x_6174 = append_1059 x_1820 in
  let x_1824 i_2160 =
    let x_6198 = x_6174 ((true, i_2160), (false, 0), (false, 0)) in
    let x_6551 = x_6174 ((true, i_2160), (false, 0), (false, 0)) in
    snd (#0 x_6551)
  in
  let x_1825 i_2150 =
    let x_6224 = x_6174 ((false, 0), (true, i_2150), (false, 0)) in
    let x_6540 = x_6174 ((false, 0), (true, i_2150), (false, 0)) in
    snd (#1 x_6540)
  in
  let x_1826 i_2140 =
    let x_6250 = x_6174 ((false, 0), (false, 0), (true, i_2140)) in
    let x_6529 = x_6174 ((false, 0), (false, 0), (true, i_2140)) in
    snd (#2 x_6529)
  in
  let rec x_x_4033 x_3995 x_3996 =
    let x_6268 = x_6174 ((false, 0), (true, x_3995), (false, 0)) in
    let x_6286 = x_6174 ((false, 0), (false, 0), (true, x_3996)) in
    let x_6517 = x_6174 ((false, 0), (true, x_3995), (true, x_3996)) in
    (snd (#1 x_6517), snd (#2 x_6517))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6363 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6363))
    else
      if fst (snd ii_2123) = false then
        let x_6322 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6322), (false, (true, 0)))
      else
        let x_6298 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6298), (true, snd x_6298))
  in
  let x_1830 i_2103 =
    let x_6423 = x_1829 ((true, i_2103), (false, 0)) in
    let x_6509 = x_1829 ((true, i_2103), (false, 0)) in
    snd (fst x_6509)
  in
  let x_1831 i_2096 =
    let x_6442 = x_1829 ((false, 0), (true, i_2096)) in
    let x_6501 = x_1829 ((false, 0), (true, i_2096)) in
    snd (snd x_6501)
  in
  let x_6466 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6490 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6467 = x_6016 i_1016 in
  let n_1504 = if fst (snd (#0 x_6490)) <> false then
                 snd (snd (#0 x_6490))
               else
                 _|_ in
  let n_1505 = if fst x_6467 <> false then
                 snd x_6467
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6485 = rand_int () in
let x_6487 = rand_int () in
let x_6488 = main_1015 x_6485 in
let x_6489 = x_6488 x_6487 in
let x_1847 = x_6489 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_6485; is_subsumed: 
rand_int (), x_6488 x_6487; is_subsumed: main_1015 x_6485, x_6489; is_subsumed: 
rand_int (), x_6489; is_subsumed: rand_int (), x_6489; is_subsumed: make_list_1008 n_1017, 
append_1059 x_1820; is_subsumed: make_list_1008 n_1017, x_6174 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_6174 ((true, i_1016), (false, 0), (false, 0)), x_6174 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6174 ((true, i_1016), (false, 0), (false, 0)); x_6466 |-> x_6490
is_subsumed: x_6174 ((true, i_1016), (false, 0), (false, 0)), x_6016 i_1016; is_subsumed: 
x_6174 ((true, i_1016), (false, 0), (false, 0)), x_6016 i_1016; is_subsumed: 
append_1059 x_1820, x_6016 i_1016; is_subsumed: x_6016 i_1016, if fst (snd (#0 x_6490)) <> false then
                                                                 snd (snd (#0 x_6490))
                                                               else
                                                                 _|_; is_subsumed: 
x_6174 ((true, i_1016), (false, 0), (false, 0)), if fst (snd (#0 x_6490)) <> false then
                                                   snd (snd (#0 x_6490))
                                                 else
                                                   _|_; is_subsumed: 
append_1059 x_1820, if fst (snd (#0 x_6490)) <> false then
                      snd (snd (#0 x_6490))
                    else
                      _|_; is_subsumed: make_list_1008 n_1017, if fst (snd (#0 x_6490)) <> false then
                                                                 snd (snd (#0 x_6490))
                                                               else
                                                                 _|_; is_subsumed: 
if fst (snd (#0 x_6490)) <> false then
  snd (snd (#0 x_6490))
else
  _|_, if fst x_6467 <> false then
         snd x_6467
       else
         _|_; is_subsumed: x_6174 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_6467 <> false then
  snd x_6467
else
  _|_; is_subsumed: x_6174 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_6467 <> false then
  snd x_6467
else
  _|_; is_subsumed: append_1059 x_1820, if fst x_6467 <> false then
                                          snd x_6467
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_6467 <> false then
  snd x_6467
else
  _|_; is_subsumed: append_1059 x_1820, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
x_1829 ((false, 0), (true, i_2096)), x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
append_1059 x_1820, x_1829 ((false, 0), (true, i_2096)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((false, 0), (true, i_2096)); x_6442 |-> x_6501
is_subsumed: append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
x_1829 ((true, i_2103), (false, 0)), x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
append_1059 x_1820, x_1829 ((true, i_2103), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1829 ((true, i_2103), (false, 0)); x_6423 |-> x_6509
is_subsumed: append_1059 x_1820, x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
make_list_1008 n_1017, x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)); is_subsumed: 
append_1059 x_1820, x_1825 (snd (fst ii_2123)); is_subsumed: make_list_1008 n_1017, 
x_1825 (snd (fst ii_2123)); is_subsumed: append_1059 x_1820, x_1826 (snd (snd ii_2123)); is_subsumed: 
make_list_1008 n_1017, x_1826 (snd (snd ii_2123)); is_subsumed: make_list_1008 n_1017, 
x_6174 ((false, 0), (true, x_3995), (false, 0)); is_subsumed: x_6174 ((false, 0), (true, x_3995), (false, 0)), 
x_6174 ((false, 0), (false, 0), (true, x_3996)); is_subsumed: make_list_1008 n_1017, 
x_6174 ((false, 0), (false, 0), (true, x_3996)); is_subsumed: x_6174 ((false, 0), (false, 0), (true, x_3996)), 
x_6174 ((false, 0), (true, x_3995), (true, x_3996)); is_subsumed: x_6174 ((false, 0), (true, x_3995), (false, 0)), 
x_6174 ((false, 0), (true, x_3995), (true, x_3996)); is_subsumed: make_list_1008 n_1017, 
x_6174 ((false, 0), (true, x_3995), (true, x_3996)); x_6286 |-> x_6517
x_6268 |-> x_6517
is_subsumed: make_list_1008 n_1017, x_6174 ((false, 0), (false, 0), (true, i_2140)); is_subsumed: 
x_6174 ((false, 0), (false, 0), (true, i_2140)), x_6174 ((false, 0), (false, 0), (true, i_2140)); is_subsumed: 
make_list_1008 n_1017, x_6174 ((false, 0), (false, 0), (true, i_2140)); x_6250 |-> x_6529
is_subsumed: make_list_1008 n_1017, x_6174 ((false, 0), (true, i_2150), (false, 0)); is_subsumed: 
x_6174 ((false, 0), (true, i_2150), (false, 0)), x_6174 ((false, 0), (true, i_2150), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6174 ((false, 0), (true, i_2150), (false, 0)); x_6224 |-> x_6540
is_subsumed: make_list_1008 n_1017, x_6174 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: 
x_6174 ((true, i_2160), (false, 0), (false, 0)), x_6174 ((true, i_2160), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_6174 ((true, i_2160), (false, 0), (false, 0)); x_6198 |-> x_6551
is_subsumed: make_list_1008 n_1017, x_1820 ((false, 0), (true, x_2171)); is_subsumed: 
x_1820 ((false, 0), (true, x_2171)), x_1820 ((false, 0), (true, x_2171)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((false, 0), (true, x_2171)); x_6171 |-> x_6562
is_subsumed: make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
x_1820 ((true, i_2178), (false, 0)), x_1820 ((true, i_2178), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1820 ((true, i_2178), (false, 0)); x_6152 |-> x_6570
is_subsumed: x_6016 (snd (fst ix_2198)), f_1479 (snd (snd ix_2198)); is_subsumed: 
make_list_1008 n_1017, f_1479 (snd (snd ix_2198)); is_subsumed: make_list_1008 n_1017, 
f_1479 (snd (snd ix_2198)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_4326 |-> x_6705
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
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1746 (snd (#1 iii_2432)), 
x_1747 (snd (#2 iii_2432)); is_subsumed: _|_, x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: _|_, 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1746 (snd (#1 iii_2432)); is_subsumed: _|_, x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2432)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1788; is_subsumed: x_1023 ((true, 0), (false, 0)), 
append_1059 x_1788; is_subsumed: append_1059 x_1788, cons_1225 (snd (snd (fst x_6705))); is_subsumed: 
x_1023 ((true, 0), (false, 0)), cons_1225 (snd (snd (fst x_6705))); is_subsumed: 
append_1059 x_1788, x_5268 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_5268 x_1792; is_subsumed: x_1023 ((true, 0), (false, 0)), x_5268 x_1792; is_subsumed: 
x_5268 x_1792, x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
append_1059 x_1788, x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1802 (snd (#0 iii_2861)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1746 (snd (#1 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1746 (snd (#1 iii_2861)); is_subsumed: append_1059 x_1788, x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1802 (snd (#0 iii_2861)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1747 (snd (#2 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1747 (snd (#2 iii_2861)); is_subsumed: append_1059 x_1788, x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1802 (snd (#0 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1802 (snd (#0 iii_2861)); is_subsumed: append_1059 x_1788, x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1802 (snd (#0 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1746 (snd (#1 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1746 (snd (#1 iii_2861)); is_subsumed: append_1059 x_1788, x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1746 (snd (#1 iii_2861)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1747 (snd (#2 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1747 (snd (#2 iii_2861)); is_subsumed: append_1059 x_1788, x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1746 (snd (#1 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1746 (snd (#1 iii_2861)); is_subsumed: append_1059 x_1788, x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_2861)); is_subsumed: 
x_5268 x_1792, x_1747 (snd (#2 iii_2861)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1747 (snd (#2 iii_2861)); is_subsumed: append_1059 x_1788, x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_2861)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
append_1059 x_1788, x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
x_5269 ((false, 0), (true, i_2886)), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
append_1059 x_1788, x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((false, 0), (true, i_2886)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((false, 0), (true, i_2886)); x_5351 |-> x_6578
is_subsumed: cons_1225 (snd (snd (fst x_6705))), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
x_5269 ((true, x_3877), (false, 0)), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, x_3877), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, x_3877), (false, 0)); x_5300 |-> x_6595
is_subsumed: x_5269 ((true, x_3877), (false, 0)), x_1023 ((true, x_3878), (false, 0)); is_subsumed: 
x_5269 ((true, x_3877), (false, 0)), x_1023 ((true, x_3878), (false, 0)); is_subsumed: 
x_5268 x_1792, x_1023 ((true, x_3878), (false, 0)); is_subsumed: cons_1225 (snd (snd (fst x_6705))), 
x_1023 ((true, x_3878), (false, 0)); is_subsumed: append_1059 x_1788, 
x_1023 ((true, x_3878), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3878), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_3878), (false, 0)); is_subsumed: x_1023 ((true, x_3878), (false, 0)), 
x_1023 ((false, 0), (true, x_3879)); is_subsumed: x_5269 ((true, x_3877), (false, 0)), 
x_1023 ((false, 0), (true, x_3879)); is_subsumed: x_5269 ((true, x_3877), (false, 0)), 
x_1023 ((false, 0), (true, x_3879)); is_subsumed: x_5268 x_1792, x_1023 ((false, 0), (true, x_3879)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_1023 ((false, 0), (true, x_3879)); is_subsumed: 
append_1059 x_1788, x_1023 ((false, 0), (true, x_3879)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3879)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3879)); is_subsumed: 
x_1023 ((false, 0), (true, x_3879)), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_1023 ((true, x_3878), (false, 0)), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_5269 ((true, x_3877), (false, 0)), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_5269 ((true, x_3877), (false, 0)), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_5268 x_1792, x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
append_1059 x_1788, x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3878), (true, x_3879)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3878), (true, x_3879)); x_5328 |-> x_6586
x_5314 |-> x_6586
is_subsumed: cons_1225 (snd (snd (fst x_6705))), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
x_5269 ((true, i_2893), (false, 0)), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
cons_1225 (snd (snd (fst x_6705))), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
append_1059 x_1788, x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, i_2893), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5269 ((true, i_2893), (false, 0)); x_5286 |-> x_6603
is_subsumed: append_1059 x_1788, x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1797 ((false, 0), (true, i_2902)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
append_1059 x_1788, x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((false, 0), (true, i_2902)); x_5264 |-> x_6611
is_subsumed: append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1797 ((true, i_2909), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
append_1059 x_1788, x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1797 ((true, i_2909), (false, 0)); x_5245 |-> x_6619
is_subsumed: append_1059 x_1788, x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)); is_subsumed: 
append_1059 x_1788, x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1793 (snd (fst ii_2929)); is_subsumed: append_1059 x_1788, x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1794 (snd (snd ii_2929)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, x_3825), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, x_3825), (false, 0)); is_subsumed: 
x_4996 ((false, 0), (true, x_3825), (false, 0)), x_4996 ((false, 0), (false, 0), (true, x_3826)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, x_3826)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, x_3826)); is_subsumed: 
x_4996 ((false, 0), (false, 0), (true, x_3826)), x_4996 ((false, 0), (true, x_3825), (true, x_3826)); is_subsumed: 
x_4996 ((false, 0), (true, x_3825), (false, 0)), x_4996 ((false, 0), (true, x_3825), (true, x_3826)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, x_3825), (true, x_3826)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, x_3825), (true, x_3826)); x_5108 |-> x_6627
x_5090 |-> x_6627
is_subsumed: x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_4996 ((false, 0), (false, 0), (true, i_2946)), x_4996 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, i_2946)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (false, 0), (true, i_2946)); x_5072 |-> x_6639
is_subsumed: x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_4996 ((false, 0), (true, i_2956), (false, 0)), x_4996 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, i_2956), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((false, 0), (true, i_2956), (false, 0)); x_5046 |-> x_6650
is_subsumed: x_1023 ((true, 0), (false, 0)), x_4996 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_4996 ((true, i_2966), (false, 0), (false, 0)), x_4996 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((true, i_2966), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_4996 ((true, i_2966), (false, 0), (false, 0)); x_5020 |-> x_6661
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1788 ((false, 0), (true, i_2977)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((false, 0), (true, i_2977)); x_4993 |-> x_6672
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1788 ((true, i_2984), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1788 ((true, i_2984), (false, 0)); x_4974 |-> x_6680
is_subsumed: x_1023 ((true, 0), (false, 0)), xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)); is_subsumed: 
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
xs_1222 (i_1220 - 1); is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3785 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3785 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_3785 + 1), (false, 0)), x_1023 ((false, 0), (true, x_3786)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3786)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3786)); is_subsumed: 
x_1023 ((false, 0), (true, x_3786)), x_1023 ((true, x_3785 + 1), (true, x_3786)); is_subsumed: 
x_1023 ((true, x_3785 + 1), (false, 0)), x_1023 ((true, x_3785 + 1), (true, x_3786)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3785 + 1), (true, x_3786)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3785 + 1), (true, x_3786)); x_4706 |-> x_6688
x_4691 |-> x_6688
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_1150 + 1), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1150 + 1), (false, 0)); x_4676 |-> x_6697
is_subsumed: x_1023 ((true, 0), (false, 0)), x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1747 (snd (#0 iii_3257)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1747 (snd (#0 iii_3257)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#0 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1746 (snd (#1 iii_3257)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1746 (snd (#1 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1747 (snd (#2 iii_3257)); is_subsumed: 
x_1023 ((false, 0), (true, x_3936)), x_1023 ((true, x_3937), (false, 0)); is_subsumed: 
x_1023 ((true, x_3937), (false, 0)), x_1023 ((false, 0), (true, x_3938)); is_subsumed: 
x_1023 ((false, 0), (true, x_3936)), x_1023 ((false, 0), (true, x_3938)); is_subsumed: 
x_1023 ((false, 0), (true, x_3938)), x_1023 ((true, x_3937), (true, x_3938)); is_subsumed: 
x_1023 ((true, x_3937), (false, 0)), x_1023 ((true, x_3937), (true, x_3938)); is_subsumed: 
x_1023 ((false, 0), (true, x_3936)), x_1023 ((true, x_3937), (true, x_3938)); x_4304 |-> x_6714
x_4290 |-> x_6714
is_subsumed: x_1023 ((false, 0), (true, i_3303)), x_1023 ((false, 0), (true, i_3303)); x_4262 |-> x_6723
is_subsumed: x_1023 ((true, i_3310), (false, 0)), x_1023 ((true, i_3310), (false, 0)); x_4243 |-> x_6731
is_subsumed: make_list_1008 (n_1009 - 1), rand_int (); is_subsumed: make_list_1008 (n_1009 - 1), 
cons_1117 x_4179; is_subsumed: rand_int (), x_4180 x_4177; is_subsumed: 
cons_1117 x_4179, x_4181 ((false, 0), (true, i_3440)); is_subsumed: rand_int (), 
x_4181 ((false, 0), (true, i_3440)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4181 ((false, 0), (true, i_3440)); is_subsumed: x_4181 ((false, 0), (true, i_3440)), 
x_4181 ((false, 0), (true, i_3440)); is_subsumed: cons_1117 x_4179, x_4181 ((false, 0), (true, i_3440)); is_subsumed: 
rand_int (), x_4181 ((false, 0), (true, i_3440)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4181 ((false, 0), (true, i_3440)); x_4217 |-> x_6739
is_subsumed: cons_1117 x_4179, x_4181 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4181 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4181 ((true, i_3447), (false, 0)); is_subsumed: x_4181 ((true, i_3447), (false, 0)), 
x_4181 ((true, i_3447), (false, 0)); is_subsumed: cons_1117 x_4179, x_4181 ((true, i_3447), (false, 0)); is_subsumed: 
rand_int (), x_4181 ((true, i_3447), (false, 0)); is_subsumed: make_list_1008 (n_1009 - 1), 
x_4181 ((true, i_3447), (false, 0)); x_4198 |-> x_6747
is_subsumed: x_1592 (snd (fst ii_3523)), xs_1114 (snd (snd ii_3523)); 
x_4217; x_4198; x_4243; x_4262; x_4304; x_4290; x_4676; x_4691; x_4706; x_4974; x_4993; x_5020; x_5046; x_5072; 
x_5090; x_5108; x_5245; x_5264; x_5286; x_5314; x_5328; x_5300; x_5351; x_4326; x_6466; x_6442; x_6423; x_6286; 
x_6268; x_6250; x_6224; x_6198; x_6171; x_6152
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
          let x_4049 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4049
                               else
                                 xs_1114 n_1526 in
          x_4049
      in
      let x_1732 ii_3523 =
        if fst (fst ii_3523) = false then
          if fst (snd ii_3523) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4131 = xs_1114 (snd (snd ii_3523)) in
            ((false, (true, 0)), (true, x_4131))
        else
          if fst (snd ii_3523) = false then
            let x_4090 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4090), (false, (true, 0)))
          else
            let x_4067 = x_1592 (snd (fst ii_3523)) in
            let x_4077 = xs_1114 (snd (snd ii_3523)) in
            ((true, x_4067), (true, x_4077))
      in
      x_1732
    in
    let x_4177 = make_list_1008 (n_1009 - 1) in
    let x_4179 = rand_int () in
    let x_4180 = cons_1117 x_4179 in
    let x_4181 = x_4180 x_4177 in
    let x_1739 i_3447 = let x_6747 = x_4181 ((true, i_3447), (false, 0)) in
                        snd (fst x_6747) in
    let x_1740 i_3440 = let x_6739 = x_4181 ((false, 0), (true, i_3440)) in
                        snd (snd x_6739) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = let x_6731 = x_1023 ((true, i_3310), (false, 0)) in
                      snd (fst x_6731) in
  let x_1747 i_3303 = let x_6723 = x_1023 ((false, 0), (true, i_3303)) in
                      snd (snd x_6723) in
  let rec x_x_x_3975 x_3936 x_3937 x_3938 =
    let x_4276 = x_1023 ((false, 0), (true, x_3936)) in
    let x_6714 = x_1023 ((true, x_3937), (true, x_3938)) in
    (snd (snd x_4276), snd (fst x_6714), snd (snd x_6714))
  in
  let x_6705 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6705)) = false then
    let x_1811 iii_3257 =
      if fst (#0 iii_3257) = false then
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5949 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_5949))
        else
          if fst (#2 iii_3257) = false then
            let x_5896 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_5896), (false, (true, 0)))
          else
            let x_5861 = x_1746 (snd (#1 iii_3257)) in
            let x_5871 = x_1747 (snd (#2 iii_3257)) in
            ((false, (true, 0)), (true, x_5861), (true, x_5871))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            let x_5802 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5802), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5767 = x_1747 (snd (#0 iii_3257)) in
            let x_5788 = x_1747 (snd (#2 iii_3257)) in
            ((true, x_5767), (false, (true, 0)), (true, x_5788))
        else
          if fst (#2 iii_3257) = false then
            let x_5726 = x_1747 (snd (#0 iii_3257)) in
            let x_5736 = x_1746 (snd (#1 iii_3257)) in
            ((true, x_5726), (true, x_5736), (false, (true, 0)))
          else
            let x_5694 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5694), (true, #1 x_5694), (true, #2 x_5694))
    in
    x_1811
  else
    if fst (snd (fst x_6705)) <> false then
      let xs'_1014 x_1150 = let x_6697 = x_1023 ((true, x_1150 + 1), (false, 0)) in
                            snd (fst x_6697) in
      let rec xs'_x_3811 x_3785 x_3786 =
        let x_6688 = x_1023 ((true, x_3785 + 1), (true, x_3786)) in
        (snd (fst x_6688), snd (snd x_6688))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4717 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4717
                                 else
                                   xs_1222 n_1544 in
            x_4717
        in
        let x_1785 ii_3086 =
          if fst (fst ii_3086) = false then
            if fst (snd ii_3086) = false then
              ((false, (true, 0)), (false, (true, 0)))
            else
              let x_4799 = xs_1222 (snd (snd ii_3086)) in
              ((false, (true, 0)), (true, x_4799))
          else
            if fst (snd ii_3086) = false then
              let x_4758 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4758), (false, (true, 0)))
            else
              let x_4735 = x_1625 (snd (fst ii_3086)) in
              let x_4745 = xs_1222 (snd (snd ii_3086)) in
              ((true, x_4735), (true, x_4745))
        in
        x_1785
      in
      let x_1788 ii_3004 =
        if fst (fst ii_3004) = false then
          if fst (snd ii_3004) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_4914 = x_1747 (snd (snd ii_3004)) in
            ((false, (true, 0)), (true, x_4914))
        else
          if fst (snd ii_3004) = false then
            let x_4873 = xs'_1014 (snd (fst ii_3004)) in
            ((true, x_4873), (false, (true, 0)))
          else
            let x_4849 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4849), (true, snd x_4849))
      in
      let x_1789 i_2984 = let x_6680 = x_1788 ((true, i_2984), (false, 0)) in
                          snd (fst x_6680) in
      let x_1790 i_2977 = let x_6672 = x_1788 ((false, 0), (true, i_2977)) in
                          snd (snd x_6672) in
      let x_4996 = append_1059 x_1788 in
      let x_1792 i_2966 = let x_6661 = x_4996 ((true, i_2966), (false, 0), (false, 0)) in
                          snd (#0 x_6661) in
      let x_1793 i_2956 = let x_6650 = x_4996 ((false, 0), (true, i_2956), (false, 0)) in
                          snd (#1 x_6650) in
      let x_1794 i_2946 = let x_6639 = x_4996 ((false, 0), (false, 0), (true, i_2946)) in
                          snd (#2 x_6639) in
      let rec x_x_3863 x_3825 x_3826 =
        let x_6627 = x_4996 ((false, 0), (true, x_3825), (true, x_3826)) in
        (snd (#1 x_6627), snd (#2 x_6627))
      in
      let x_1797 ii_2929 =
        if fst (fst ii_2929) = false then
          if fst (snd ii_2929) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5185 = x_1794 (snd (snd ii_2929)) in
            ((false, (true, 0)), (true, x_5185))
        else
          if fst (snd ii_2929) = false then
            let x_5144 = x_1793 (snd (fst ii_2929)) in
            ((true, x_5144), (false, (true, 0)))
          else
            let x_5120 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5120), (true, snd x_5120))
      in
      let x_1798 i_2909 = let x_6619 = x_1797 ((true, i_2909), (false, 0)) in
                          snd (fst x_6619) in
      let x_1799 i_2902 = let x_6611 = x_1797 ((false, 0), (true, i_2902)) in
                          snd (snd x_6611) in
      let x_5268 = cons_1225 (snd (snd (fst x_6705))) in
      let x_5269 = x_5268 x_1792 in
      let x_1802 i_2893 = let x_6603 = x_5269 ((true, i_2893), (false, 0)) in
                          snd (fst x_6603) in
      let rec x_x_x_3916 x_3877 x_3878 x_3879 =
        let x_6595 = x_5269 ((true, x_3877), (false, 0)) in
        let x_6586 = x_1023 ((true, x_3878), (true, x_3879)) in
        (snd (fst x_6595), snd (fst x_6586), snd (snd x_6586))
      in
      let x_1803 i_2886 = let x_6578 = x_5269 ((false, 0), (true, i_2886)) in
                          snd (snd x_6578) in
      let x_1807 iii_2861 =
        if fst (#0 iii_2861) = false then
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5618 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_5618))
          else
            if fst (#2 iii_2861) = false then
              let x_5565 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_5565), (false, (true, 0)))
            else
              let x_5530 = x_1746 (snd (#1 iii_2861)) in
              let x_5540 = x_1747 (snd (#2 iii_2861)) in
              ((false, (true, 0)), (true, x_5530), (true, x_5540))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              let x_5471 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5471), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5436 = x_1802 (snd (#0 iii_2861)) in
              let x_5457 = x_1747 (snd (#2 iii_2861)) in
              ((true, x_5436), (false, (true, 0)), (true, x_5457))
          else
            if fst (#2 iii_2861) = false then
              let x_5395 = x_1802 (snd (#0 iii_2861)) in
              let x_5405 = x_1746 (snd (#1 iii_2861)) in
              ((true, x_5395), (true, x_5405), (false, (true, 0)))
            else
              let x_5363 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5363), (true, #1 x_5363), (true, #2 x_5363))
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
              let x_4596 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4596))
          else
            if fst (#2 iii_2432) = false then
              let x_4543 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_4543), (false, (true, 0)))
            else
              let x_4508 = x_1746 (snd (#1 iii_2432)) in
              let x_4518 = x_1747 (snd (#2 iii_2432)) in
              ((false, (true, 0)), (true, x_4508), (true, x_4518))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              let x_4449 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4449), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4414 = x_1610 (snd (#0 iii_2432)) in
              let x_4435 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4414), (false, (true, 0)), (true, x_4435))
          else
            if fst (#2 iii_2432) = false then
              let x_4373 = x_1610 (snd (#0 iii_2432)) in
              let x_4383 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4373), (true, x_4383), (false, (true, 0)))
            else
              let x_4339 = x_1610 (snd (#0 iii_2432)) in
              let x_4349 = x_1746 (snd (#1 iii_2432)) in
              let x_4359 = x_1747 (snd (#2 iii_2432)) in
              ((true, x_4339), (true, x_4349), (true, x_4359))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6016 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6092 = f_1479 (snd (snd ix_2198)) in
        ((false, (true, 0)), (true, x_6092))
    else
      if fst (snd ix_2198) = false then
        let x_6051 = x_6016 (snd (fst ix_2198)) in
        ((true, x_6051), (false, (true, 0)))
      else
        let x_6028 = x_6016 (snd (fst ix_2198)) in
        let x_6038 = f_1479 (snd (snd ix_2198)) in
        ((true, x_6028), (true, x_6038))
  in
  let x_1821 i_2178 = let x_6570 = x_1820 ((true, i_2178), (false, 0)) in
                      snd (fst x_6570) in
  let x_1822 x_2171 = let x_6562 = x_1820 ((false, 0), (true, x_2171)) in
                      snd (snd x_6562) in
  let x_6174 = append_1059 x_1820 in
  let x_1824 i_2160 = let x_6551 = x_6174 ((true, i_2160), (false, 0), (false, 0)) in
                      snd (#0 x_6551) in
  let x_1825 i_2150 = let x_6540 = x_6174 ((false, 0), (true, i_2150), (false, 0)) in
                      snd (#1 x_6540) in
  let x_1826 i_2140 = let x_6529 = x_6174 ((false, 0), (false, 0), (true, i_2140)) in
                      snd (#2 x_6529) in
  let rec x_x_4033 x_3995 x_3996 =
    let x_6517 = x_6174 ((false, 0), (true, x_3995), (true, x_3996)) in
    (snd (#1 x_6517), snd (#2 x_6517))
  in
  let x_1829 ii_2123 =
    if fst (fst ii_2123) = false then
      if fst (snd ii_2123) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6363 = x_1826 (snd (snd ii_2123)) in
        ((false, (true, 0)), (true, x_6363))
    else
      if fst (snd ii_2123) = false then
        let x_6322 = x_1825 (snd (fst ii_2123)) in
        ((true, x_6322), (false, (true, 0)))
      else
        let x_6298 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6298), (true, snd x_6298))
  in
  let x_1830 i_2103 = let x_6509 = x_1829 ((true, i_2103), (false, 0)) in
                      snd (fst x_6509) in
  let x_1831 i_2096 = let x_6501 = x_1829 ((false, 0), (true, i_2096)) in
                      snd (snd x_6501) in
  let x_6490 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6467 = x_6016 i_1016 in
  let n_1504 = if fst (snd (#0 x_6490)) <> false then
                 snd (snd (#0 x_6490))
               else
                 _|_ in
  let n_1505 = if fst x_6467 <> false then
                 snd x_6467
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6485 = rand_int () in
let x_6487 = rand_int () in
let x_6488 = main_1015 x_6485 in
let x_6489 = x_6488 x_6487 in
let x_1847 = x_6489 in
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
          let x_4049 = xs_1114 (i_1112 - 1) in
          let xs_1525 n_1526 = if n_1526 = i_1112 - 1 then
                                 x_4049
                               else
                                 xs_1114 n_1526 in
          x_4049
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
            let x_4067 = x_1592 (snd (fst ii_3523)) in
            ((true, x_4067), (true, xs_1114 (snd (snd ii_3523))))
      in
      x_1732
    in
    let x_4177 = make_list_1008 (n_1009 - 1) in
    let x_4179 = rand_int () in
    let x_4180 = cons_1117 x_4179 in
    let x_4181 = x_4180 x_4177 in
    let x_1739 i_3447 = snd (fst (x_4181 ((true, i_3447), (false, 0)))) in
    let x_1740 i_3440 = snd (snd (x_4181 ((false, 0), (true, i_3440)))) in
    x_1739
in
let rec append_1059 x_1023 =
  let x_1746 i_3310 = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
  let x_1747 i_3303 = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
  let rec x_x_x_3975 x_3936 x_3937 x_3938 =
    let x_4276 = x_1023 ((false, 0), (true, x_3936)) in
    let x_6714 = x_1023 ((true, x_3937), (true, x_3938)) in
    (snd (snd x_4276), snd (fst x_6714), snd (snd x_6714))
  in
  let x_6705 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_6705)) = false then
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
            let x_5861 = x_1746 (snd (#1 iii_3257)) in
            ((false, (true, 0)), (true, x_5861), (true, x_1747 (snd (#2 iii_3257))))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            let x_5767 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5767), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
        else
          if fst (#2 iii_3257) = false then
            let x_5726 = x_1747 (snd (#0 iii_3257)) in
            ((true, x_5726), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
          else
            let x_5694 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 x_5694), (true, #1 x_5694), (true, #2 x_5694))
    in
    x_1811
  else
    if fst (snd (fst x_6705)) <> false then
      let xs'_1014 x_1150 = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
      let rec xs'_x_3811 x_3785 x_3786 =
        let x_6688 = x_1023 ((true, x_3785 + 1), (true, x_3786)) in
        (snd (fst x_6688), snd (snd x_6688))
      in
      let cons_1225 x_1221 xs_1222 =
        let x_1625 i_1220 =
          if i_1220 = 0 then
            (true, x_1221)
          else
            let x_4717 = xs_1222 (i_1220 - 1) in
            let xs_1543 n_1544 = if n_1544 = i_1220 - 1 then
                                   x_4717
                                 else
                                   xs_1222 n_1544 in
            x_4717
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
              let x_4735 = x_1625 (snd (fst ii_3086)) in
              ((true, x_4735), (true, xs_1222 (snd (snd ii_3086))))
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
            let x_4849 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
            ((true, fst x_4849), (true, snd x_4849))
      in
      let x_1789 i_2984 = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
      let x_1790 i_2977 = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
      let x_4996 = append_1059 x_1788 in
      let x_1792 i_2966 = snd (#0 (x_4996 ((true, i_2966), (false, 0), (false, 0)))) in
      let x_1793 i_2956 = snd (#1 (x_4996 ((false, 0), (true, i_2956), (false, 0)))) in
      let x_1794 i_2946 = snd (#2 (x_4996 ((false, 0), (false, 0), (true, i_2946)))) in
      let rec x_x_3863 x_3825 x_3826 =
        let x_6627 = x_4996 ((false, 0), (true, x_3825), (true, x_3826)) in
        (snd (#1 x_6627), snd (#2 x_6627))
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
            let x_5120 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
            ((true, fst x_5120), (true, snd x_5120))
      in
      let x_1798 i_2909 = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
      let x_1799 i_2902 = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
      let x_5268 = cons_1225 (snd (snd (fst x_6705))) in
      let x_5269 = x_5268 x_1792 in
      let x_1802 i_2893 = snd (fst (x_5269 ((true, i_2893), (false, 0)))) in
      let rec x_x_x_3916 x_3877 x_3878 x_3879 =
        let x_6595 = x_5269 ((true, x_3877), (false, 0)) in
        let x_6586 = x_1023 ((true, x_3878), (true, x_3879)) in
        (snd (fst x_6595), snd (fst x_6586), snd (snd x_6586))
      in
      let x_1803 i_2886 = snd (snd (x_5269 ((false, 0), (true, i_2886)))) in
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
              let x_5530 = x_1746 (snd (#1 iii_2861)) in
              ((false, (true, 0)), (true, x_5530), (true, x_1747 (snd (#2 iii_2861))))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5436 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5436), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
          else
            if fst (#2 iii_2861) = false then
              let x_5395 = x_1802 (snd (#0 iii_2861)) in
              ((true, x_5395), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
            else
              let x_5363 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 x_5363), (true, #1 x_5363), (true, #2 x_5363))
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
              let x_4508 = x_1746 (snd (#1 iii_2432)) in
              ((false, (true, 0)), (true, x_4508), (true, x_1747 (snd (#2 iii_2432))))
        else
          if fst (#1 iii_2432) = false then
            if fst (#2 iii_2432) = false then
              ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4414 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4414), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
          else
            if fst (#2 iii_2432) = false then
              let x_4373 = x_1610 (snd (#0 iii_2432)) in
              ((true, x_4373), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
            else
              let x_4339 = x_1610 (snd (#0 iii_2432)) in
              let x_4349 = x_1746 (snd (#1 iii_2432)) in
              ((true, x_4339), (true, x_4349), (true, x_1747 (snd (#2 iii_2432))))
      in
      x_1761
in
let main_1015 i_1016 n_1017 =
  let x_6016 = make_list_1008 n_1017 in
  let f_1479 x_1329 = (false, 0) in
  let x_1820 ix_2198 =
    if fst (fst ix_2198) = false then
      if fst (snd ix_2198) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
    else
      if fst (snd ix_2198) = false then
        ((true, x_6016 (snd (fst ix_2198))), (false, (true, 0)))
      else
        let x_6028 = x_6016 (snd (fst ix_2198)) in
        ((true, x_6028), (true, f_1479 (snd (snd ix_2198))))
  in
  let x_1821 i_2178 = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
  let x_1822 x_2171 = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
  let x_6174 = append_1059 x_1820 in
  let x_1824 i_2160 = snd (#0 (x_6174 ((true, i_2160), (false, 0), (false, 0)))) in
  let x_1825 i_2150 = snd (#1 (x_6174 ((false, 0), (true, i_2150), (false, 0)))) in
  let x_1826 i_2140 = snd (#2 (x_6174 ((false, 0), (false, 0), (true, i_2140)))) in
  let rec x_x_4033 x_3995 x_3996 =
    let x_6517 = x_6174 ((false, 0), (true, x_3995), (true, x_3996)) in
    (snd (#1 x_6517), snd (#2 x_6517))
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
        let x_6298 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
        ((true, fst x_6298), (true, snd x_6298))
  in
  let x_1830 i_2103 = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
  let x_1831 i_2096 = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
  let x_6490 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
  let x_6467 = x_6016 i_1016 in
  let n_1504 = if fst (snd (#0 x_6490)) <> false then
                 snd (snd (#0 x_6490))
               else
                 _|_ in
  let n_1505 = if fst x_6467 <> false then
                 snd x_6467
               else
                 _|_ in
  if n_1504 = n_1505 then
    ()
  else
    {fail} ()
in
let x_6485 = rand_int () in
let x_6487 = rand_int () in
let x_6488 = main_1015 x_6485 in
let x_6489 = x_6488 x_6487 in
let x_1847 = x_6489 in
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
           let x_4049 = xs_1114 (i_1112 - 1) in
           let xs_1525 (n_1526:int) = if n_1526 = i_1112 - 1 then
                                        x_4049
                                      else
                                        xs_1114 n_1526 in
           x_4049
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
             let x_4067 = x_1592 (snd (fst ii_3523)) in
             ((true, x_4067), (true, xs_1114 (snd (snd ii_3523))))
       in
       x_1732
     in
     let x_4177 = make_list_1008 (n_1009 - 1) in
     let x_4179 = rand_int () in
     let x_4180 = cons_1117 x_4179 in
     let x_4181 = x_4180 x_4177 in
     let x_1739 (i_3447:int) = snd (fst (x_4181 ((true, i_3447), (false, 0)))) in
     let x_1740 (i_3440:int) = snd (snd (x_4181 ((false, 0), (true, i_3440)))) in
     x_1739
 in
 let rec append_1059 (x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1746 (i_3310:int) = snd (fst (x_1023 ((true, i_3310), (false, 0)))) in
   let x_1747 (i_3303:int) = snd (snd (x_1023 ((false, 0), (true, i_3303)))) in
   let rec x_x_x_3975 (x_3936:int) (x_3937:int) (x_3938:int) =
     let x_4276 = x_1023 ((false, 0), (true, x_3936)) in
     let x_6714 = x_1023 ((true, x_3937), (true, x_3938)) in
     (snd (snd x_4276), snd (fst x_6714), snd (snd x_6714))
   in
   let x_6705 = x_1023 ((true, 0), (false, 0)) in
   if fst (snd (fst x_6705)) = false then
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
             let x_5861 = x_1746 (snd (#1 iii_3257)) in
             ((false, (true, 0)), (true, x_5861), (true, x_1747 (snd (#2 iii_3257))))
       else
         if fst (#1 iii_3257) = false then
           if fst (#2 iii_3257) = false then
             ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
           else
             let x_5767 = x_1747 (snd (#0 iii_3257)) in
             ((true, x_5767), (false, (true, 0)), (true, x_1747 (snd (#2 iii_3257))))
         else
           if fst (#2 iii_3257) = false then
             let x_5726 = x_1747 (snd (#0 iii_3257)) in
             ((true, x_5726), (true, x_1746 (snd (#1 iii_3257))), (false, (true, 0)))
           else
             let x_5694 = x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) in
             ((true, #0 x_5694), (true, #1 x_5694), (true, #2 x_5694))
     in
     x_1811
   else
     if fst (snd (fst x_6705)) <> false then
       let xs'_1014 (x_1150:int) = snd (fst (x_1023 ((true, x_1150 + 1), (false, 0)))) in
       let rec xs'_x_3811 (x_3785:int) (x_3786:int) =
         let x_6688 = x_1023 ((true, x_3785 + 1), (true, x_3786)) in
         (snd (fst x_6688), snd (snd x_6688))
       in
       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool * int))) =
         let x_1625 (i_1220:int) =
           if i_1220 = 0 then
             (true, x_1221)
           else
             let x_4717 = xs_1222 (i_1220 - 1) in
             let xs_1543 (n_1544:int) = if n_1544 = i_1220 - 1 then
                                          x_4717
                                        else
                                          xs_1222 n_1544 in
             x_4717
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
               let x_4735 = x_1625 (snd (fst ii_3086)) in
               ((true, x_4735), (true, xs_1222 (snd (snd ii_3086))))
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
             let x_4849 = xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) in
             ((true, fst x_4849), (true, snd x_4849))
       in
       let x_1789 (i_2984:int) = snd (fst (x_1788 ((true, i_2984), (false, 0)))) in
       let x_1790 (i_2977:int) = snd (snd (x_1788 ((false, 0), (true, i_2977)))) in
       let x_4996 = append_1059 x_1788 in
       let x_1792 (i_2966:int) = snd (#0 (x_4996 ((true, i_2966), (false, 0), (false, 0)))) in
       let x_1793 (i_2956:int) = snd (#1 (x_4996 ((false, 0), (true, i_2956), (false, 0)))) in
       let x_1794 (i_2946:int) = snd (#2 (x_4996 ((false, 0), (false, 0), (true, i_2946)))) in
       let rec x_x_3863 (x_3825:int) (x_3826:int) =
         let x_6627 = x_4996 ((false, 0), (true, x_3825), (true, x_3826)) in
         (snd (#1 x_6627), snd (#2 x_6627))
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
             let x_5120 = x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) in
             ((true, fst x_5120), (true, snd x_5120))
       in
       let x_1798 (i_2909:int) = snd (fst (x_1797 ((true, i_2909), (false, 0)))) in
       let x_1799 (i_2902:int) = snd (snd (x_1797 ((false, 0), (true, i_2902)))) in
       let x_5268 = cons_1225 (snd (snd (fst x_6705))) in
       let x_5269 = x_5268 x_1792 in
       let x_1802 (i_2893:int) = snd (fst (x_5269 ((true, i_2893), (false, 0)))) in
       let rec x_x_x_3916 (x_3877:int) (x_3878:int) (x_3879:int) =
         let x_6595 = x_5269 ((true, x_3877), (false, 0)) in
         let x_6586 = x_1023 ((true, x_3878), (true, x_3879)) in
         (snd (fst x_6595), snd (fst x_6586), snd (snd x_6586))
       in
       let x_1803 (i_2886:int) = snd (snd (x_5269 ((false, 0), (true, i_2886)))) in
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
               let x_5530 = x_1746 (snd (#1 iii_2861)) in
               ((false, (true, 0)), (true, x_5530), (true, x_1747 (snd (#2 iii_2861))))
         else
           if fst (#1 iii_2861) = false then
             if fst (#2 iii_2861) = false then
               ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
             else
               let x_5436 = x_1802 (snd (#0 iii_2861)) in
               ((true, x_5436), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2861))))
           else
             if fst (#2 iii_2861) = false then
               let x_5395 = x_1802 (snd (#0 iii_2861)) in
               ((true, x_5395), (true, x_1746 (snd (#1 iii_2861))), (false, (true, 0)))
             else
               let x_5363 = x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) in
               ((true, #0 x_5363), (true, #1 x_5363), (true, #2 x_5363))
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
               let x_4508 = x_1746 (snd (#1 iii_2432)) in
               ((false, (true, 0)), (true, x_4508), (true, x_1747 (snd (#2 iii_2432))))
         else
           if fst (#1 iii_2432) = false then
             if fst (#2 iii_2432) = false then
               ((true, x_1610 (snd (#0 iii_2432))), (false, (true, 0)), (false, (true, 0)))
             else
               let x_4414 = x_1610 (snd (#0 iii_2432)) in
               ((true, x_4414), (false, (true, 0)), (true, x_1747 (snd (#2 iii_2432))))
           else
             if fst (#2 iii_2432) = false then
               let x_4373 = x_1610 (snd (#0 iii_2432)) in
               ((true, x_4373), (true, x_1746 (snd (#1 iii_2432))), (false, (true, 0)))
             else
               let x_4339 = x_1610 (snd (#0 iii_2432)) in
               let x_4349 = x_1746 (snd (#1 iii_2432)) in
               ((true, x_4339), (true, x_4349), (true, x_1747 (snd (#2 iii_2432))))
       in
       x_1761
 in
 let main_1015 (i_1016:int) (n_1017:int) =
   let x_6016 = make_list_1008 n_1017 in
   let f_1479 (x_1329:int) = (false, 0) in
   let x_1820 (ix_2198:((bool * int) * (bool * int))) =
     if fst (fst ix_2198) = false then
       if fst (snd ix_2198) = false then
         ((false, (true, 0)), (false, (true, 0)))
       else
         ((false, (true, 0)), (true, f_1479 (snd (snd ix_2198))))
     else
       if fst (snd ix_2198) = false then
         ((true, x_6016 (snd (fst ix_2198))), (false, (true, 0)))
       else
         let x_6028 = x_6016 (snd (fst ix_2198)) in
         ((true, x_6028), (true, f_1479 (snd (snd ix_2198))))
   in
   let x_1821 (i_2178:int) = snd (fst (x_1820 ((true, i_2178), (false, 0)))) in
   let x_1822 (x_2171:int) = snd (snd (x_1820 ((false, 0), (true, x_2171)))) in
   let x_6174 = append_1059 x_1820 in
   let x_1824 (i_2160:int) = snd (#0 (x_6174 ((true, i_2160), (false, 0), (false, 0)))) in
   let x_1825 (i_2150:int) = snd (#1 (x_6174 ((false, 0), (true, i_2150), (false, 0)))) in
   let x_1826 (i_2140:int) = snd (#2 (x_6174 ((false, 0), (false, 0), (true, i_2140)))) in
   let rec x_x_4033 (x_3995:int) (x_3996:int) =
     let x_6517 = x_6174 ((false, 0), (true, x_3995), (true, x_3996)) in
     (snd (#1 x_6517), snd (#2 x_6517))
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
         let x_6298 = x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) in
         ((true, fst x_6298), (true, snd x_6298))
   in
   let x_1830 (i_2103:int) = snd (fst (x_1829 ((true, i_2103), (false, 0)))) in
   let x_1831 (i_2096:int) = snd (snd (x_1829 ((false, 0), (true, i_2096)))) in
   let x_6490 = x_6174 ((true, i_1016), (false, 0), (false, 0)) in
   let x_6467 = x_6016 i_1016 in
   let n_1504 = if fst (snd (#0 x_6490)) <> false then
                  snd (snd (#0 x_6490))
                else
                  _|_ in
   let n_1505 = if fst x_6467 <> false then
                  snd x_6467
                else
                  _|_ in
   if n_1504 = n_1505 then
     ()
   else
     {fail} ()
 in
 let x_6485 = rand_int () in
 let x_6487 = rand_int () in
 let x_6488 = main_1015 x_6485 in
 let x_6489 = x_6488 x_6487 in
 let x_1847 = x_6489 in
 ()

CPS:
 let rec make_list_1008 (n_1009:int) (k_make_list_6767:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6767 (fun (x_1123:int) -> fun (k_make_list_6769:((bool * int) -> X)) -> k_make_list_6769 (false, 0))
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> ((bool * int) -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_6788:((bool * int) -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_6788 (true, x_1113)
         else
           let x_4049 (k_make_list_cons_x_x_6801:((bool * int) -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_6801 in
           x_4049 (fun (x_6826:(bool * int)) -> k_make_list_cons_x_6788 x_6826)
       in
       let
         x_1732 (ii_3523:((bool * int) * (bool * int))) 
               (k_make_list_cons_x_6835:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
         if fst (fst ii_3523) = false then
           if fst (snd ii_3523) = false then
             k_make_list_cons_x_6835 ((false, (true, 0)), (false, (true, 0)))
           else
             xs_1114 (snd (snd ii_3523))
               (fun (x_10965:(bool * int)) -> k_make_list_cons_x_6835 ((false, (true, 0)), (true, x_10965)))
         else
           if fst (snd ii_3523) = false then
             x_1592 (snd (fst ii_3523))
               (fun (x_10962:(bool * int)) -> k_make_list_cons_x_6835 ((true, x_10962), (false, (true, 0))))
           else
             let x_4067 (k_make_list_cons_x_x_6944:((bool * int) -> X)) =
               x_1592 (snd (fst ii_3523)) k_make_list_cons_x_x_6944
             in
             x_4067
               (fun (x_6978:(bool * int)) ->
                  xs_1114 (snd (snd ii_3523))
                    (fun (x_10944:(bool * int)) -> k_make_list_cons_x_6835 ((true, x_6978), (true, x_10944))))
       in
       x_1732
     in
     let x_4177 (k_make_list_x_7013:((int -> ((bool * int) -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_7013
     in
     x_4177
       (fun (x_7153:(int -> ((bool * int) -> X) -> X)) ->
          (let x_4179 (k_make_list_x_7034:(int -> X)) = rand_int_cps () k_make_list_x_7034 in
           x_4179
             (fun (x_7149:int) ->
                k_make_list_6767
                  (let x_1739 (i_3447:int) (k_make_list_x_7072:((bool * int) -> X)) =
                     ((cons_1117 x_7149) x_7153) ((true, i_3447), (false, 0))
                       (fun (p_10999:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_make_list_x_7072 (snd (fst p_10999)))
                   in
                   x_1739))))
 in
 let rec
   append_1059 (x_1023:(((bool * int) * (bool * int)) -> (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) 
              (k_append_7185:((((bool * int) * (bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                -> X)) =
   let x_1746 (i_3310:int) (k_append_x_7192:((bool * int) -> X)) =
     x_1023 ((true, i_3310), (false, 0))
       (fun (p_11019:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7192 (snd (fst p_11019)))
   in
   let x_1747 (i_3303:int) (k_append_x_7236:((bool * int) -> X)) =
     x_1023 ((false, 0), (true, i_3303))
       (fun (p_11029:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7236 (snd (snd p_11029)))
   in
   let rec
     x_x_x_3975 (x_3936:int) (x_3937:int) (x_3938:int) 
               (k_append_x_x_x_7280:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     let x_4276 (k_append_x_x_x_x_7305:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1023 ((false, 0), (true, x_3936)) k_append_x_x_x_x_7305
     in
     x_4276
       (fun (x_7350:((bool * (bool * int)) * (bool * (bool * int)))) ->
          (let x_6714 (k_append_x_x_x_x_7335:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
             x_1023 ((true, x_3937), (true, x_3938)) k_append_x_x_x_x_7335
           in
           x_6714
             (fun (x_7349:((bool * (bool * int)) * (bool * (bool * int)))) ->
                k_append_x_x_x_7280 (snd (snd x_7350), snd (fst x_7349), snd (snd x_7349)))))
   in
   let x_6705 (k_append_x_7383:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
     x_1023 ((true, 0), (false, 0)) k_append_x_7383
   in
   x_6705
     (fun (x_9958:((bool * (bool * int)) * (bool * (bool * int)))) ->
        (if fst (snd (fst x_9958)) = false then
           k_append_7185
             (let
                x_1811 (iii_3257:((bool * int) * (bool * int) * (bool * int))) 
                      (k_append_x_7393:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                if fst (#0 iii_3257) = false then
                  if fst (#1 iii_3257) = false then
                    if fst (#2 iii_3257) = false then
                      k_append_x_7393 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                    else
                      x_1747 (snd (#2 iii_3257))
                        (fun (x_11914:(bool * int)) ->
                           k_append_x_7393 ((false, (true, 0)), (false, (true, 0)), (true, x_11914)))
                  else
                    if fst (#2 iii_3257) = false then
                      x_1746 (snd (#1 iii_3257))
                        (fun (x_11901:(bool * int)) ->
                           k_append_x_7393 ((false, (true, 0)), (true, x_11901), (false, (true, 0))))
                    else
                      let x_5861 (k_append_x_x_7544:((bool * int) -> X)) = x_1746 (snd (#1 iii_3257)) k_append_x_x_7544 in
                      x_5861
                        (fun (x_7592:(bool * int)) ->
                           x_1747 (snd (#2 iii_3257))
                             (fun (x_11859:(bool * int)) ->
                                k_append_x_7393 ((false, (true, 0)), (true, x_7592), (true, x_11859))))
                else
                  if fst (#1 iii_3257) = false then
                    if fst (#2 iii_3257) = false then
                      x_1747 (snd (#0 iii_3257))
                        (fun (x_11856:(bool * int)) ->
                           k_append_x_7393 ((true, x_11856), (false, (true, 0)), (false, (true, 0))))
                    else
                      let x_5767 (k_append_x_x_7655:((bool * int) -> X)) = x_1747 (snd (#0 iii_3257)) k_append_x_x_7655 in
                      x_5767
                        (fun (x_7703:(bool * int)) ->
                           x_1747 (snd (#2 iii_3257))
                             (fun (x_11807:(bool * int)) ->
                                k_append_x_7393 ((true, x_7703), (false, (true, 0)), (true, x_11807))))
                  else
                    if fst (#2 iii_3257) = false then
                      let x_5726 (k_append_x_x_7714:((bool * int) -> X)) = x_1747 (snd (#0 iii_3257)) k_append_x_x_7714 in
                      x_5726
                        (fun (x_7762:(bool * int)) ->
                           x_1746 (snd (#1 iii_3257))
                             (fun (x_11799:(bool * int)) ->
                                k_append_x_7393 ((true, x_7762), (true, x_11799), (false, (true, 0)))))
                    else
                      let x_5694 (k_append_x_x_7771:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                        x_x_x_3975 (snd (#0 iii_3257)) (snd (#1 iii_3257)) (snd (#2 iii_3257)) k_append_x_x_7771
                      in
                      x_5694
                        (fun (x_7803:((bool * int) * (bool * int) * (bool * int))) ->
                           k_append_x_7393 ((true, #0 x_7803), (true, #1 x_7803), (true, #2 x_7803)))
              in
              x_1811)
         else
           if fst (snd (fst x_9958)) <> false then
             let xs'_1014 (x_1150:int) (k_append_xs'_7825:((bool * int) -> X)) =
               x_1023 ((true, x_1150 + 1), (false, 0))
                 (fun (p_11269:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_7825 (snd (fst p_11269)))
             in
             let rec xs'_x_3811 (x_3785:int) (x_3786:int) (k_append_xs'_x_7869:(((bool * int) * (bool * int)) -> X)) =
               let x_6688 (k_append_xs'_x_x_7894:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_1023 ((true, x_3785 + 1), (true, x_3786)) k_append_xs'_x_x_7894
               in
               x_6688
                 (fun (x_7906:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    k_append_xs'_x_7869 (snd (fst x_7906), snd (snd x_7906)))
             in
             let cons_1225 (x_1221:int) (xs_1222:(int -> ((bool * int) -> X) -> X)) =
               let x_1625 (i_1220:int) (k_append_cons_x_7923:((bool * int) -> X)) =
                 if i_1220 = 0 then
                   k_append_cons_x_7923 (true, x_1221)
                 else
                   let x_4717 (k_append_cons_x_x_7936:((bool * int) -> X)) =
                     xs_1222 (i_1220 - 1) k_append_cons_x_x_7936
                   in
                   x_4717 (fun (x_7961:(bool * int)) -> k_append_cons_x_7923 x_7961)
               in
               let
                 x_1785 (ii_3086:((bool * int) * (bool * int))) 
                       (k_append_cons_x_7970:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_3086) = false then
                   if fst (snd ii_3086) = false then
                     k_append_cons_x_7970 ((false, (true, 0)), (false, (true, 0)))
                   else
                     xs_1222 (snd (snd ii_3086))
                       (fun (x_11300:(bool * int)) -> k_append_cons_x_7970 ((false, (true, 0)), (true, x_11300)))
                 else
                   if fst (snd ii_3086) = false then
                     x_1625 (snd (fst ii_3086))
                       (fun (x_11297:(bool * int)) -> k_append_cons_x_7970 ((true, x_11297), (false, (true, 0))))
                   else
                     let x_4735 (k_append_cons_x_x_8079:((bool * int) -> X)) =
                       x_1625 (snd (fst ii_3086)) k_append_cons_x_x_8079
                     in
                     x_4735
                       (fun (x_8113:(bool * int)) ->
                          xs_1222 (snd (snd ii_3086))
                            (fun (x_11279:(bool * int)) -> k_append_cons_x_7970 ((true, x_8113), (true, x_11279))))
               in
               x_1785
             in
             let
               x_1788 (ii_3004:((bool * int) * (bool * int))) 
                     (k_append_x_8139:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
               if fst (fst ii_3004) = false then
                 if fst (snd ii_3004) = false then
                   k_append_x_8139 ((false, (true, 0)), (false, (true, 0)))
                 else
                   x_1747 (snd (snd ii_3004))
                     (fun (x_11345:(bool * int)) -> k_append_x_8139 ((false, (true, 0)), (true, x_11345)))
               else
                 if fst (snd ii_3004) = false then
                   xs'_1014 (snd (fst ii_3004))
                     (fun (x_11342:(bool * int)) -> k_append_x_8139 ((true, x_11342), (false, (true, 0))))
                 else
                   let x_4849 (k_append_x_x_8249:(((bool * int) * (bool * int)) -> X)) =
                     xs'_x_3811 (snd (fst ii_3004)) (snd (snd ii_3004)) k_append_x_x_8249
                   in
                   x_4849
                     (fun (x_8273:((bool * int) * (bool * int))) ->
                        k_append_x_8139 ((true, fst x_8273), (true, snd x_8273)))
             in
             let
               x_4996
                     (k_append_x_8394:((((bool * int) * (bool * int) * (bool * int)) ->
                                          (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)
                                            -> X) -> X)) = append_1059 x_1788 k_append_x_8394
             in
             x_4996
               (fun (x_9445:(((bool * int) * (bool * int) * (bool * int)) ->
                               (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                  k_append_7185
                    (let x_1792 (i_2966:int) (k_append_x_8416:((bool * int) -> X)) =
                       x_9445 ((true, i_2966), (false, 0), (false, 0))
                         (fun (p_11408:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8416 (snd (#0 p_11408)))
                     in
                     let x_1793 (i_2956:int) (k_append_x_8463:((bool * int) -> X)) =
                       x_9445 ((false, 0), (true, i_2956), (false, 0))
                         (fun (p_11427:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8463 (snd (#1 p_11427)))
                     in
                     let x_1794 (i_2946:int) (k_append_x_8510:((bool * int) -> X)) =
                       x_9445 ((false, 0), (false, 0), (true, i_2946))
                         (fun (p_11446:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8510 (snd (#2 p_11446)))
                     in
                     let rec
                       x_x_3863 (x_3825:int) (x_3826:int) (k_append_x_x_8558:(((bool * int) * (bool * int)) -> X)) =
                       let
                         x_6627
                               (k_append_x_x_x_8591:(((bool * (bool * int)) * (
                                                      bool * (bool * int)) * (
                                                      bool * (bool * int))) -> X)) =
                         x_9445 ((false, 0), (true, x_3825), (true, x_3826)) k_append_x_x_x_8591
                       in
                       x_6627
                         (fun (x_8603:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_x_8558 (snd (#1 x_8603), snd (#2 x_8603)))
                     in
                     let
                       x_1797 (ii_2929:((bool * int) * (bool * int))) 
                             (k_append_x_8608:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       if fst (fst ii_2929) = false then
                         if fst (snd ii_2929) = false then
                           k_append_x_8608 ((false, (true, 0)), (false, (true, 0)))
                         else
                           x_1794 (snd (snd ii_2929))
                             (fun (x_11484:(bool * int)) -> k_append_x_8608 ((false, (true, 0)), (true, x_11484)))
                       else
                         if fst (snd ii_2929) = false then
                           x_1793 (snd (fst ii_2929))
                             (fun (x_11481:(bool * int)) -> k_append_x_8608 ((true, x_11481), (false, (true, 0))))
                         else
                           let x_5120 (k_append_x_x_8718:(((bool * int) * (bool * int)) -> X)) =
                             x_x_3863 (snd (fst ii_2929)) (snd (snd ii_2929)) k_append_x_x_8718
                           in
                           x_5120
                             (fun (x_8742:((bool * int) * (bool * int))) ->
                                k_append_x_8608 ((true, fst x_8742), (true, snd x_8742)))
                     in
                     let x_1802 (i_2893:int) (k_append_x_8861:((bool * int) -> X)) =
                       ((cons_1225 (snd (snd (fst x_9958)))) x_1792) 
                         ((true, i_2893), (false, 0))
                         (fun (p_11538:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8861 (snd (fst p_11538)))
                     in
                     let rec
                       x_x_x_3916 (x_3877:int) (x_3878:int) (x_3879:int) 
                                 (k_append_x_x_x_8902:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                       let x_6595 (k_append_x_x_x_x_8927:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1225 (snd (snd (fst x_9958)))) x_1792) 
                           ((true, x_3877), (false, 0)) k_append_x_x_x_x_8927
                       in
                       x_6595
                         (fun (x_8972:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_6586 (k_append_x_x_x_x_8957:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                               x_1023 ((true, x_3878), (true, x_3879)) k_append_x_x_x_x_8957
                             in
                             x_6586
                               (fun (x_8971:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                  k_append_x_x_x_8902 (snd (fst x_8972), snd (fst x_8971), snd (snd x_8971)))))
                     in
                     let
                       x_1807 (iii_2861:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_9016:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_2861) = false then
                         if fst (#1 iii_2861) = false then
                           if fst (#2 iii_2861) = false then
                             k_append_x_9016 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                           else
                             x_1747 (snd (#2 iii_2861))
                               (fun (x_11710:(bool * int)) ->
                                  k_append_x_9016 ((false, (true, 0)), (false, (true, 0)), (true, x_11710)))
                         else
                           if fst (#2 iii_2861) = false then
                             x_1746 (snd (#1 iii_2861))
                               (fun (x_11697:(bool * int)) ->
                                  k_append_x_9016 ((false, (true, 0)), (true, x_11697), (false, (true, 0))))
                           else
                             let x_5530 (k_append_x_x_9167:((bool * int) -> X)) =
                               x_1746 (snd (#1 iii_2861)) k_append_x_x_9167
                             in
                             x_5530
                               (fun (x_9215:(bool * int)) ->
                                  x_1747 (snd (#2 iii_2861))
                                    (fun (x_11655:(bool * int)) ->
                                       k_append_x_9016 ((false, (true, 0)), (true, x_9215), (true, x_11655))))
                       else
                         if fst (#1 iii_2861) = false then
                           if fst (#2 iii_2861) = false then
                             x_1802 (snd (#0 iii_2861))
                               (fun (x_11652:(bool * int)) ->
                                  k_append_x_9016 ((true, x_11652), (false, (true, 0)), (false, (true, 0))))
                           else
                             let x_5436 (k_append_x_x_9278:((bool * int) -> X)) =
                               x_1802 (snd (#0 iii_2861)) k_append_x_x_9278
                             in
                             x_5436
                               (fun (x_9326:(bool * int)) ->
                                  x_1747 (snd (#2 iii_2861))
                                    (fun (x_11603:(bool * int)) ->
                                       k_append_x_9016 ((true, x_9326), (false, (true, 0)), (true, x_11603))))
                         else
                           if fst (#2 iii_2861) = false then
                             let x_5395 (k_append_x_x_9337:((bool * int) -> X)) =
                               x_1802 (snd (#0 iii_2861)) k_append_x_x_9337
                             in
                             x_5395
                               (fun (x_9385:(bool * int)) ->
                                  x_1746 (snd (#1 iii_2861))
                                    (fun (x_11595:(bool * int)) ->
                                       k_append_x_9016 ((true, x_9385), (true, x_11595), (false, (true, 0)))))
                           else
                             let x_5363 (k_append_x_x_9394:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                               x_x_x_3916 (snd (#0 iii_2861)) (snd (#1 iii_2861)) (snd (#2 iii_2861)) k_append_x_x_9394
                             in
                             x_5363
                               (fun (x_9426:((bool * int) * (bool * int) * (bool * int))) ->
                                  k_append_x_9016 ((true, #0 x_9426), (true, #1 x_9426), (true, #2 x_9426)))
                     in
                     x_1807))
           else
             let x_1610 (k_append_x_9485:((int -> ((bool * int) -> X) -> X) -> X)) = _|_ in
             x_1610
               (fun (x_9943:(int -> ((bool * int) -> X) -> X)) ->
                  k_append_7185
                    (let
                       x_1761 (iii_2432:((bool * int) * (bool * int) * (bool * int))) 
                             (k_append_x_9493:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_2432) = false then
                         if fst (#1 iii_2432) = false then
                           if fst (#2 iii_2432) = false then
                             k_append_x_9493 ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
                           else
                             x_1747 (snd (#2 iii_2432))
                               (fun (x_11201:(bool * int)) ->
                                  k_append_x_9493 ((false, (true, 0)), (false, (true, 0)), (true, x_11201)))
                         else
                           if fst (#2 iii_2432) = false then
                             x_1746 (snd (#1 iii_2432))
                               (fun (x_11188:(bool * int)) ->
                                  k_append_x_9493 ((false, (true, 0)), (true, x_11188), (false, (true, 0))))
                           else
                             let x_4508 (k_append_x_x_9644:((bool * int) -> X)) =
                               x_1746 (snd (#1 iii_2432)) k_append_x_x_9644
                             in
                             x_4508
                               (fun (x_9692:(bool * int)) ->
                                  x_1747 (snd (#2 iii_2432))
                                    (fun (x_11146:(bool * int)) ->
                                       k_append_x_9493 ((false, (true, 0)), (true, x_9692), (true, x_11146))))
                       else
                         if fst (#1 iii_2432) = false then
                           if fst (#2 iii_2432) = false then
                             x_9943 (snd (#0 iii_2432))
                               (fun (x_11143:(bool * int)) ->
                                  k_append_x_9493 ((true, x_11143), (false, (true, 0)), (false, (true, 0))))
                           else
                             let x_4414 (k_append_x_x_9755:((bool * int) -> X)) =
                               x_9943 (snd (#0 iii_2432)) k_append_x_x_9755
                             in
                             x_4414
                               (fun (x_9803:(bool * int)) ->
                                  x_1747 (snd (#2 iii_2432))
                                    (fun (x_11094:(bool * int)) ->
                                       k_append_x_9493 ((true, x_9803), (false, (true, 0)), (true, x_11094))))
                         else
                           if fst (#2 iii_2432) = false then
                             let x_4373 (k_append_x_x_9814:((bool * int) -> X)) =
                               x_9943 (snd (#0 iii_2432)) k_append_x_x_9814
                             in
                             x_4373
                               (fun (x_9862:(bool * int)) ->
                                  x_1746 (snd (#1 iii_2432))
                                    (fun (x_11086:(bool * int)) ->
                                       k_append_x_9493 ((true, x_9862), (true, x_11086), (false, (true, 0)))))
                           else
                             let x_4339 (k_append_x_x_9869:((bool * int) -> X)) =
                               x_9943 (snd (#0 iii_2432)) k_append_x_x_9869
                             in
                             x_4339
                               (fun (x_9924:(bool * int)) ->
                                  (let x_4349 (k_append_x_x_9881:((bool * int) -> X)) =
                                     x_1746 (snd (#1 iii_2432)) k_append_x_x_9881
                                   in
                                   x_4349
                                     (fun (x_9923:(bool * int)) ->
                                        x_1747 (snd (#2 iii_2432))
                                          (fun (x_11055:(bool * int)) ->
                                             k_append_x_9493 ((true, x_9924), (true, x_9923), (true, x_11055))))))
                     in
                     x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_9985:(unit -> X)) =
   let x_6016 (k_main_x_9998:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_9998 in
   x_6016
     (fun (x_10863:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_10013:((bool * int) -> X)) = k_main_f_10013 (false, 0) in
         let
           x_1820 (ix_2198:((bool * int) * (bool * int))) 
                 (k_main_x_10026:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
           if fst (fst ix_2198) = false then
             if fst (snd ix_2198) = false then
               k_main_x_10026 ((false, (true, 0)), (false, (true, 0)))
             else
               f_1479 (snd (snd ix_2198))
                 (fun (x_11995:(bool * int)) -> k_main_x_10026 ((false, (true, 0)), (true, x_11995)))
           else
             if fst (snd ix_2198) = false then
               x_10863 (snd (fst ix_2198))
                 (fun (x_11992:(bool * int)) -> k_main_x_10026 ((true, x_11992), (false, (true, 0))))
             else
               let x_6028 (k_main_x_x_10135:((bool * int) -> X)) = x_10863 (snd (fst ix_2198)) k_main_x_x_10135 in
               x_6028
                 (fun (x_10169:(bool * int)) ->
                    f_1479 (snd (snd ix_2198))
                      (fun (x_11974:(bool * int)) -> k_main_x_10026 ((true, x_10169), (true, x_11974))))
         in
         let
           x_6174
                 (k_main_x_10281:((((bool * int) * (bool * int) * (bool * int)) ->
                                     (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) ->
                                       X) -> X)) = append_1059 x_1820 k_main_x_10281
         in
         x_6174
           (fun (x_10843:(((bool * int) * (bool * int) * (bool * int)) ->
                            (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_10351:((bool * int) -> X)) =
                 x_10843 ((false, 0), (true, i_2150), (false, 0))
                   (fun (p_12077:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_10351 (snd (#1 p_12077)))
               in
               let x_1826 (i_2140:int) (k_main_x_10400:((bool * int) -> X)) =
                 x_10843 ((false, 0), (false, 0), (true, i_2140))
                   (fun (p_12096:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_10400 (snd (#2 p_12096)))
               in
               let rec x_x_4033 (x_3995:int) (x_3996:int) (k_main_x_x_10449:(((bool * int) * (bool * int)) -> X)) =
                 let
                   x_6517
                         (k_main_x_x_x_10482:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                -> X)) =
                   x_10843 ((false, 0), (true, x_3995), (true, x_3996)) k_main_x_x_x_10482
                 in
                 x_6517
                   (fun (x_10494:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_x_10449 (snd (#1 x_10494), snd (#2 x_10494)))
               in
               let
                 x_1829 (ii_2123:((bool * int) * (bool * int))) 
                       (k_main_x_10502:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_2123) = false then
                   if fst (snd ii_2123) = false then
                     k_main_x_10502 ((false, (true, 0)), (false, (true, 0)))
                   else
                     x_1826 (snd (snd ii_2123))
                       (fun (x_12134:(bool * int)) -> k_main_x_10502 ((false, (true, 0)), (true, x_12134)))
                 else
                   if fst (snd ii_2123) = false then
                     x_1825 (snd (fst ii_2123))
                       (fun (x_12131:(bool * int)) -> k_main_x_10502 ((true, x_12131), (false, (true, 0))))
                   else
                     let x_6298 (k_main_x_x_10612:(((bool * int) * (bool * int)) -> X)) =
                       x_x_4033 (snd (fst ii_2123)) (snd (snd ii_2123)) k_main_x_x_10612
                     in
                     x_6298
                       (fun (x_10636:((bool * int) * (bool * int))) ->
                          k_main_x_10502 ((true, fst x_10636), (true, snd x_10636)))
               in
               let
                 x_6490 (k_main_x_10762:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_10843 ((true, i_1016), (false, 0), (false, 0)) k_main_x_10762
               in
               x_6490
                 (fun (x_10810:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let x_6467 (k_main_x_10774:((bool * int) -> X)) = x_10863 i_1016 k_main_x_10774 in
                     x_6467
                       (fun (x_10809:(bool * int)) ->
                          (let n_1504 (k_main_n_10785:(int -> X)) =
                             if fst (snd (#0 x_10810)) <> false then
                               k_main_n_10785 (snd (snd (#0 x_10810)))
                             else
                               _|_
                           in
                           n_1504
                             (fun (n_10808:int) ->
                                (let n_1505 (k_main_n_10793:(int -> X)) =
                                   if fst x_10809 <> false then
                                     k_main_n_10793 (snd x_10809)
                                   else
                                     _|_
                                 in
                                 n_1505
                                   (fun (n_10807:int) ->
                                      (if n_10808 = n_10807 then
                                         k_main_9985 ()
                                       else
                                         {|fail|} () k_main_9985))))))))))))
 in
 let x_6485 (k_x_10874:(int -> X)) = rand_int_cps () k_x_10874 in
 x_6485
   (fun (x_10919:int) ->
      (let x_6487 (k_x_10886:(int -> X)) = rand_int_cps () k_x_10886 in
       x_6487
         (fun (x_10918:int) ->
            (let x_6489 (k_x_10907:(unit -> X)) = (main_1015 x_10919) x_10918 k_x_10907 in
             x_6489 (fun (x_10913:unit) -> {end})))))

remove_pair:
 let rec make_list_1008 (n_1009:int) (k_make_list_6767:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1009 < 0 then
     k_make_list_6767 (fun (x_1123:int) -> fun (k_make_list_6769:(bool -> int -> X)) -> k_make_list_6769 false 0)
   else
     let cons_1117 (x_1113:int) (xs_1114:(int -> (bool -> int -> X) -> X)) =
       let x_1592 (i_1112:int) (k_make_list_cons_x_6788:(bool -> int -> X)) =
         if i_1112 = 0 then
           k_make_list_cons_x_6788 true x_1113
         else
           let x_4049 (k_make_list_cons_x_x_6801:(bool -> int -> X)) = xs_1114 (i_1112 - 1) k_make_list_cons_x_x_6801 in
           x_4049 (fun (x0_6826:bool) -> fun (x1_6826:int) -> k_make_list_cons_x_6788 x0_6826 x1_6826)
       in
       let
         x_1732 (ii00_3523:bool) (ii01_3523:int) (ii10_3523:bool) (ii11_3523:int) 
               (k_make_list_cons_x_6835:(bool ->
                                           bool ->
                                             r011_6832:int ->
                                               bool -> bool -> r111_6832:int[\r111_6832. r011_6832 = r111_6832] -> X)) =
         if ii00_3523 = false then
           if ii10_3523 = false then
             k_make_list_cons_x_6835 false true 0 false true 0
           else
             xs_1114 ii11_3523
               (fun (x0_10965:bool) ->
                  fun (x1_10965:int) -> k_make_list_cons_x_6835 false true 0 true x0_10965 x1_10965)
         else
           if ii10_3523 = false then
             x_1592 ii01_3523
               (fun (x0_10962:bool) ->
                  fun (x1_10962:int) -> k_make_list_cons_x_6835 true x0_10962 x1_10962 false true 0)
           else
             let x_4067 (k_make_list_cons_x_x_6944:(bool -> int -> X)) = x_1592 ii01_3523 k_make_list_cons_x_x_6944 in
             x_4067
               (fun (x0_6978:bool) ->
                  fun (x1_6978:int) ->
                    xs_1114 ii11_3523
                      (fun (x0_10944:bool) ->
                         fun (x1_10944:int) -> k_make_list_cons_x_6835 true x0_6978 x1_6978 true x0_10944 x1_10944))
       in
       x_1732
     in
     let x_4177 (k_make_list_x_7013:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1008 (n_1009 - 1) k_make_list_x_7013
     in
     x_4177
       (fun (x_7153:(int -> (bool -> int -> X) -> X)) ->
          (let x_4179 (k_make_list_x_7034:(int -> X)) = rand_int_cps () k_make_list_x_7034 in
           x_4179
             (fun (x_7149:int) ->
                k_make_list_6767
                  (let x_1739 (i_3447:int) (k_make_list_x_7072:(bool -> int -> X)) =
                     cons_1117 x_7149 x_7153 true i_3447 false 0
                       (fun (p00_10999:bool) ->
                          fun (p010_10999:bool) ->
                            fun (p011_10999:int) ->
                              fun (p10_10999:bool) ->
                                fun (p110_10999:bool) ->
                                  fun (p111_10999:int) -> k_make_list_x_7072 p010_10999 p011_10999)
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
                                    r011_7179:int ->
                                      bool -> bool -> r111_7179:int[\r111_7179. r011_7179 = r111_7179] -> X) -> X))
              (k_append_7185:((bool ->
                                 int ->
                                   bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_7182:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_7182:int[\r111_7182. r011_7182 = r111_7182] ->
                                                        bool -> bool -> int -> X) -> X) -> X)) =
   let x_1746 (i_3310:int) (k_append_x_7192:(bool -> int -> X)) =
     x_1023 true i_3310 false 0
       (fun (p00_11019:bool) ->
          fun (p010_11019:bool) ->
            fun (p011_11019:int) ->
              fun (p10_11019:bool) ->
                fun (p110_11019:bool) -> fun (p111_11019:int) -> k_append_x_7192 p010_11019 p011_11019)
   in
   let x_1747 (i_3303:int) (k_append_x_7236:(bool -> int -> X)) =
     x_1023 false 0 true i_3303
       (fun (p00_11029:bool) ->
          fun (p010_11029:bool) ->
            fun (p011_11029:int) ->
              fun (p10_11029:bool) ->
                fun (p110_11029:bool) -> fun (p111_11029:int) -> k_append_x_7236 p110_11029 p111_11029)
   in
   let rec
     x_x_x_3975 (x_3936:int) (x_3937:int) (x_3938:int) 
               (k_append_x_x_x_7280:(bool -> int -> bool -> int -> bool -> int -> X)) =
     let
       x_4276
             (k_append_x_x_x_x_7305:(bool ->
                                       bool ->
                                         r011_7304:int ->
                                           bool -> bool -> r111_7304:int[\r111_7304. r011_7304 = r111_7304] -> X)) =
       x_1023 false 0 true x_3936 k_append_x_x_x_x_7305
     in
     x_4276
       (fun (x00_7350:bool) ->
          fun (x010_7350:bool) ->
            fun (x011_7350:int) ->
              fun (x10_7350:bool) ->
                fun (x110_7350:bool) ->
                  fun (x111_7350:int) ->
                    (let
                       x_6714
                             (k_append_x_x_x_x_7335:(bool ->
                                                       bool ->
                                                         r011_7334:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_7334:int[\r111_7334. r011_7334 = r111_7334] -> X)) =
                       x_1023 true x_3937 true x_3938 k_append_x_x_x_x_7335
                     in
                     x_6714
                       (fun (x00_7349:bool) ->
                          fun (x010_7349:bool) ->
                            fun (x011_7349:int) ->
                              fun (x10_7349:bool) ->
                                fun (x110_7349:bool) ->
                                  fun (x111_7349:int) ->
                                    k_append_x_x_x_7280 x110_7350 x111_7350 x010_7349 x011_7349 x110_7349 x111_7349)))
   in
   let
     x_6705
           (k_append_x_7383:(bool ->
                               bool ->
                                 r011_7382:int -> bool -> bool -> r111_7382:int[\r111_7382. r011_7382 = r111_7382] -> X)) =
     x_1023 true 0 false 0 k_append_x_7383
   in
   x_6705
     (fun (x00_9958:bool) ->
        fun (x010_9958:bool) ->
          fun (x011_9958:int) ->
            fun (x10_9958:bool) ->
              fun (x110_9958:bool) ->
                fun (x111_9958:int) ->
                  (if x010_9958 = false then
                     k_append_7185
                       (let
                          x_1811 (iii00_3257:bool) (iii01_3257:int) (iii10_3257:bool) (iii11_3257:int) 
                                (iii20_3257:bool) (iii21_3257:int) 
                                (k_append_x_7393:(bool ->
                                                    bool ->
                                                      r011_7390:int ->
                                                        bool ->
                                                          bool ->
                                                            r111_7390:
                                                              int[\r111_7390. r011_7390 = r111_7390] ->
                                                              bool -> bool -> int -> X)) =
                          if iii00_3257 = false then
                            if iii10_3257 = false then
                              if iii20_3257 = false then
                                k_append_x_7393 false true 0 false true 0 false true 0
                              else
                                x_1747 iii21_3257
                                  (fun (x0_11914:bool) ->
                                     fun (x1_11914:int) ->
                                       k_append_x_7393 false true 0 false true 0 true x0_11914 x1_11914)
                            else
                              if iii20_3257 = false then
                                x_1746 iii11_3257
                                  (fun (x0_11901:bool) ->
                                     fun (x1_11901:int) ->
                                       k_append_x_7393 false true 0 true x0_11901 x1_11901 false true 0)
                              else
                                let x_5861 (k_append_x_x_7544:(bool -> int -> X)) = x_1746 iii11_3257 k_append_x_x_7544 in
                                x_5861
                                  (fun (x0_7592:bool) ->
                                     fun (x1_7592:int) ->
                                       x_1747 iii21_3257
                                         (fun (x0_11859:bool) ->
                                            fun (x1_11859:int) ->
                                              k_append_x_7393 false true 0 true x0_7592 x1_7592 true x0_11859 x1_11859))
                          else
                            if iii10_3257 = false then
                              if iii20_3257 = false then
                                x_1747 iii01_3257
                                  (fun (x0_11856:bool) ->
                                     fun (x1_11856:int) ->
                                       k_append_x_7393 true x0_11856 x1_11856 false true 0 false true 0)
                              else
                                let x_5767 (k_append_x_x_7655:(bool -> int -> X)) = x_1747 iii01_3257 k_append_x_x_7655 in
                                x_5767
                                  (fun (x0_7703:bool) ->
                                     fun (x1_7703:int) ->
                                       x_1747 iii21_3257
                                         (fun (x0_11807:bool) ->
                                            fun (x1_11807:int) ->
                                              k_append_x_7393 true x0_7703 x1_7703 false true 0 true x0_11807 x1_11807))
                            else
                              if iii20_3257 = false then
                                let x_5726 (k_append_x_x_7714:(bool -> int -> X)) = x_1747 iii01_3257 k_append_x_x_7714 in
                                x_5726
                                  (fun (x0_7762:bool) ->
                                     fun (x1_7762:int) ->
                                       x_1746 iii11_3257
                                         (fun (x0_11799:bool) ->
                                            fun (x1_11799:int) ->
                                              k_append_x_7393 true x0_7762 x1_7762 true x0_11799 x1_11799 false true 0))
                              else
                                let x_5694 (k_append_x_x_7771:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                  x_x_x_3975 iii01_3257 iii11_3257 iii21_3257 k_append_x_x_7771
                                in
                                x_5694
                                  (fun (x00_7803:bool) ->
                                     fun (x01_7803:int) ->
                                       fun (x10_7803:bool) ->
                                         fun (x11_7803:int) ->
                                           fun (x20_7803:bool) ->
                                             fun (x21_7803:int) ->
                                               k_append_x_7393 true x00_7803 x01_7803 true x10_7803 x11_7803 true
                                                 x20_7803 x21_7803)
                        in
                        x_1811)
                   else
                     if x010_9958 <> false then
                       let xs'_1014 (x_1150:int) (k_append_xs'_7825:(bool -> int -> X)) =
                         x_1023 true (x_1150 + 1) false 0
                           (fun (p00_11269:bool) ->
                              fun (p010_11269:bool) ->
                                fun (p011_11269:int) ->
                                  fun (p10_11269:bool) ->
                                    fun (p110_11269:bool) ->
                                      fun (p111_11269:int) -> k_append_xs'_7825 p010_11269 p011_11269)
                       in
                       let rec
                         xs'_x_3811 (x_3785:int) (x_3786:int) (k_append_xs'_x_7869:(bool -> int -> bool -> int -> X)) =
                         let
                           x_6688
                                 (k_append_xs'_x_x_7894:(bool ->
                                                           bool ->
                                                             r011_7893:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_7893:
                                                                    int[\r111_7893. r011_7893 = r111_7893] -> X)) =
                           x_1023 true (x_3785 + 1) true x_3786 k_append_xs'_x_x_7894
                         in
                         x_6688
                           (fun (x00_7906:bool) ->
                              fun (x010_7906:bool) ->
                                fun (x011_7906:int) ->
                                  fun (x10_7906:bool) ->
                                    fun (x110_7906:bool) ->
                                      fun (x111_7906:int) ->
                                        k_append_xs'_x_7869 x010_7906 x011_7906 x110_7906 x111_7906)
                       in
                       let cons_1225 (x_1221:int) (xs_1222:(int -> (bool -> int -> X) -> X)) =
                         let x_1625 (i_1220:int) (k_append_cons_x_7923:(bool -> int -> X)) =
                           if i_1220 = 0 then
                             k_append_cons_x_7923 true x_1221
                           else
                             let x_4717 (k_append_cons_x_x_7936:(bool -> int -> X)) =
                               xs_1222 (i_1220 - 1) k_append_cons_x_x_7936
                             in
                             x_4717 (fun (x0_7961:bool) -> fun (x1_7961:int) -> k_append_cons_x_7923 x0_7961 x1_7961)
                         in
                         let
                           x_1785 (ii00_3086:bool) (ii01_3086:int) (ii10_3086:bool) (ii11_3086:int) 
                                 (k_append_cons_x_7970:(bool ->
                                                          bool ->
                                                            r011_7967:int ->
                                                              bool ->
                                                                bool ->
                                                                  r111_7967:int[\r111_7967. r011_7967 = r111_7967] -> X)) =
                           if ii00_3086 = false then
                             if ii10_3086 = false then
                               k_append_cons_x_7970 false true 0 false true 0
                             else
                               xs_1222 ii11_3086
                                 (fun (x0_11300:bool) ->
                                    fun (x1_11300:int) -> k_append_cons_x_7970 false true 0 true x0_11300 x1_11300)
                           else
                             if ii10_3086 = false then
                               x_1625 ii01_3086
                                 (fun (x0_11297:bool) ->
                                    fun (x1_11297:int) -> k_append_cons_x_7970 true x0_11297 x1_11297 false true 0)
                             else
                               let x_4735 (k_append_cons_x_x_8079:(bool -> int -> X)) =
                                 x_1625 ii01_3086 k_append_cons_x_x_8079
                               in
                               x_4735
                                 (fun (x0_8113:bool) ->
                                    fun (x1_8113:int) ->
                                      xs_1222 ii11_3086
                                        (fun (x0_11279:bool) ->
                                           fun (x1_11279:int) ->
                                             k_append_cons_x_7970 true x0_8113 x1_8113 true x0_11279 x1_11279))
                         in
                         x_1785
                       in
                       let
                         x_1788 (ii00_3004:bool) (ii01_3004:int) (ii10_3004:bool) (ii11_3004:int) 
                               (k_append_x_8139:(bool ->
                                                   bool ->
                                                     r011_8138:int ->
                                                       bool ->
                                                         bool -> r111_8138:int[\r111_8138. r011_8138 = r111_8138] -> X)) =
                         if ii00_3004 = false then
                           if ii10_3004 = false then
                             k_append_x_8139 false true 0 false true 0
                           else
                             x_1747 ii11_3004
                               (fun (x0_11345:bool) ->
                                  fun (x1_11345:int) -> k_append_x_8139 false true 0 true x0_11345 x1_11345)
                         else
                           if ii10_3004 = false then
                             xs'_1014 ii01_3004
                               (fun (x0_11342:bool) ->
                                  fun (x1_11342:int) -> k_append_x_8139 true x0_11342 x1_11342 false true 0)
                           else
                             let x_4849 (k_append_x_x_8249:(bool -> int -> bool -> int -> X)) =
                               xs'_x_3811 ii01_3004 ii11_3004 k_append_x_x_8249
                             in
                             x_4849
                               (fun (x00_8273:bool) ->
                                  fun (x01_8273:int) ->
                                    fun (x10_8273:bool) ->
                                      fun (x11_8273:int) ->
                                        k_append_x_8139 true x00_8273 x01_8273 true x10_8273 x11_8273)
                       in
                       let
                         x_4996
                               (k_append_x_8394:((bool ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          bool ->
                                                            int ->
                                                              (bool ->
                                                                 bool ->
                                                                   r011_8391:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8391:
                                                                    int[\r111_8391. r011_8391 = r111_8391] ->
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                         append_1059 x_1788 k_append_x_8394
                       in
                       x_4996
                         (fun (x_9445:(bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               bool ->
                                                 int ->
                                                   (bool ->
                                                      bool ->
                                                        r011_9443:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_9443:
                                                                int[\r111_9443. r011_9443 = r111_9443] ->
                                                                bool -> bool -> int -> X) -> X)) ->
                            k_append_7185
                              (let x_1792 (i_2966:int) (k_append_x_8416:(bool -> int -> X)) =
                                 x_9445 true i_2966 false 0 false 0
                                   (fun (p00_11408:bool) ->
                                      fun (p010_11408:bool) ->
                                        fun (p011_11408:int) ->
                                          fun (p10_11408:bool) ->
                                            fun (p110_11408:bool) ->
                                              fun (p111_11408:int) ->
                                                fun (p20_11408:bool) ->
                                                  fun (p210_11408:bool) ->
                                                    fun (p211_11408:int) -> k_append_x_8416 p010_11408 p011_11408)
                               in
                               let x_1793 (i_2956:int) (k_append_x_8463:(bool -> int -> X)) =
                                 x_9445 false 0 true i_2956 false 0
                                   (fun (p00_11427:bool) ->
                                      fun (p010_11427:bool) ->
                                        fun (p011_11427:int) ->
                                          fun (p10_11427:bool) ->
                                            fun (p110_11427:bool) ->
                                              fun (p111_11427:int) ->
                                                fun (p20_11427:bool) ->
                                                  fun (p210_11427:bool) ->
                                                    fun (p211_11427:int) -> k_append_x_8463 p110_11427 p111_11427)
                               in
                               let x_1794 (i_2946:int) (k_append_x_8510:(bool -> int -> X)) =
                                 x_9445 false 0 false 0 true i_2946
                                   (fun (p00_11446:bool) ->
                                      fun (p010_11446:bool) ->
                                        fun (p011_11446:int) ->
                                          fun (p10_11446:bool) ->
                                            fun (p110_11446:bool) ->
                                              fun (p111_11446:int) ->
                                                fun (p20_11446:bool) ->
                                                  fun (p210_11446:bool) ->
                                                    fun (p211_11446:int) -> k_append_x_8510 p210_11446 p211_11446)
                               in
                               let rec
                                 x_x_3863 (x_3825:int) (x_3826:int) 
                                         (k_append_x_x_8558:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_6627
                                         (k_append_x_x_x_8591:(bool ->
                                                                 bool ->
                                                                   r011_8590:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8590:
                                                                    int[\r111_8590. r011_8590 = r111_8590] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_9445 false 0 true x_3825 true x_3826 k_append_x_x_x_8591
                                 in
                                 x_6627
                                   (fun (x00_8603:bool) ->
                                      fun (x010_8603:bool) ->
                                        fun (x011_8603:int) ->
                                          fun (x10_8603:bool) ->
                                            fun (x110_8603:bool) ->
                                              fun (x111_8603:int) ->
                                                fun (x20_8603:bool) ->
                                                  fun (x210_8603:bool) ->
                                                    fun (x211_8603:int) ->
                                                      k_append_x_x_8558 x110_8603 x111_8603 x210_8603 x211_8603)
                               in
                               let
                                 x_1797 (ii00_2929:bool) (ii01_2929:int) (ii10_2929:bool) (ii11_2929:int) 
                                       (k_append_x_8608:(bool ->
                                                           bool ->
                                                             r011_8605:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8605:
                                                                    int[\r111_8605. r011_8605 = r111_8605] -> X)) =
                                 if ii00_2929 = false then
                                   if ii10_2929 = false then
                                     k_append_x_8608 false true 0 false true 0
                                   else
                                     x_1794 ii11_2929
                                       (fun (x0_11484:bool) ->
                                          fun (x1_11484:int) -> k_append_x_8608 false true 0 true x0_11484 x1_11484)
                                 else
                                   if ii10_2929 = false then
                                     x_1793 ii01_2929
                                       (fun (x0_11481:bool) ->
                                          fun (x1_11481:int) -> k_append_x_8608 true x0_11481 x1_11481 false true 0)
                                   else
                                     let x_5120 (k_append_x_x_8718:(bool -> int -> bool -> int -> X)) =
                                       x_x_3863 ii01_2929 ii11_2929 k_append_x_x_8718
                                     in
                                     x_5120
                                       (fun (x00_8742:bool) ->
                                          fun (x01_8742:int) ->
                                            fun (x10_8742:bool) ->
                                              fun (x11_8742:int) ->
                                                k_append_x_8608 true x00_8742 x01_8742 true x10_8742 x11_8742)
                               in
                               let x_1802 (i_2893:int) (k_append_x_8861:(bool -> int -> X)) =
                                 cons_1225 x011_9958 x_1792 true i_2893 false 0
                                   (fun (p00_11538:bool) ->
                                      fun (p010_11538:bool) ->
                                        fun (p011_11538:int) ->
                                          fun (p10_11538:bool) ->
                                            fun (p110_11538:bool) ->
                                              fun (p111_11538:int) -> k_append_x_8861 p010_11538 p011_11538)
                               in
                               let rec
                                 x_x_x_3916 (x_3877:int) (x_3878:int) (x_3879:int) 
                                           (k_append_x_x_x_8902:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                 let
                                   x_6595
                                         (k_append_x_x_x_x_8927:(bool ->
                                                                   bool ->
                                                                    r011_8926:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8926:
                                                                    int[\r111_8926. r011_8926 = r111_8926] -> X)) =
                                   cons_1225 x011_9958 x_1792 true x_3877 false 0 k_append_x_x_x_x_8927
                                 in
                                 x_6595
                                   (fun (x00_8972:bool) ->
                                      fun (x010_8972:bool) ->
                                        fun (x011_8972:int) ->
                                          fun (x10_8972:bool) ->
                                            fun (x110_8972:bool) ->
                                              fun (x111_8972:int) ->
                                                (let
                                                   x_6586
                                                         (k_append_x_x_x_x_8957:(
                                                         bool ->
                                                           bool ->
                                                             r011_8956:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8956:
                                                                    int[\r111_8956. r011_8956 = r111_8956] -> X)) =
                                                   x_1023 true x_3878 true x_3879 k_append_x_x_x_x_8957
                                                 in
                                                 x_6586
                                                   (fun (x00_8971:bool) ->
                                                      fun (x010_8971:bool) ->
                                                        fun (x011_8971:int) ->
                                                          fun (x10_8971:bool) ->
                                                            fun (x110_8971:bool) ->
                                                              fun (x111_8971:int) ->
                                                                k_append_x_x_x_8902 x010_8972 x011_8972 x010_8971
                                                                  x011_8971 x110_8971 x111_8971)))
                               in
                               let
                                 x_1807 (iii00_2861:bool) (iii01_2861:int) (iii10_2861:bool) (iii11_2861:int) 
                                       (iii20_2861:bool) (iii21_2861:int) 
                                       (k_append_x_9016:(bool ->
                                                           bool ->
                                                             r011_9013:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9013:
                                                                    int[\r111_9013. r011_9013 = r111_9013] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2861 = false then
                                   if iii10_2861 = false then
                                     if iii20_2861 = false then
                                       k_append_x_9016 false true 0 false true 0 false true 0
                                     else
                                       x_1747 iii21_2861
                                         (fun (x0_11710:bool) ->
                                            fun (x1_11710:int) ->
                                              k_append_x_9016 false true 0 false true 0 true x0_11710 x1_11710)
                                   else
                                     if iii20_2861 = false then
                                       x_1746 iii11_2861
                                         (fun (x0_11697:bool) ->
                                            fun (x1_11697:int) ->
                                              k_append_x_9016 false true 0 true x0_11697 x1_11697 false true 0)
                                     else
                                       let x_5530 (k_append_x_x_9167:(bool -> int -> X)) =
                                         x_1746 iii11_2861 k_append_x_x_9167
                                       in
                                       x_5530
                                         (fun (x0_9215:bool) ->
                                            fun (x1_9215:int) ->
                                              x_1747 iii21_2861
                                                (fun (x0_11655:bool) ->
                                                   fun (x1_11655:int) ->
                                                     k_append_x_9016 false true 0 true x0_9215 x1_9215 true x0_11655
                                                       x1_11655))
                                 else
                                   if iii10_2861 = false then
                                     if iii20_2861 = false then
                                       x_1802 iii01_2861
                                         (fun (x0_11652:bool) ->
                                            fun (x1_11652:int) ->
                                              k_append_x_9016 true x0_11652 x1_11652 false true 0 false true 0)
                                     else
                                       let x_5436 (k_append_x_x_9278:(bool -> int -> X)) =
                                         x_1802 iii01_2861 k_append_x_x_9278
                                       in
                                       x_5436
                                         (fun (x0_9326:bool) ->
                                            fun (x1_9326:int) ->
                                              x_1747 iii21_2861
                                                (fun (x0_11603:bool) ->
                                                   fun (x1_11603:int) ->
                                                     k_append_x_9016 true x0_9326 x1_9326 false true 0 true x0_11603
                                                       x1_11603))
                                   else
                                     if iii20_2861 = false then
                                       let x_5395 (k_append_x_x_9337:(bool -> int -> X)) =
                                         x_1802 iii01_2861 k_append_x_x_9337
                                       in
                                       x_5395
                                         (fun (x0_9385:bool) ->
                                            fun (x1_9385:int) ->
                                              x_1746 iii11_2861
                                                (fun (x0_11595:bool) ->
                                                   fun (x1_11595:int) ->
                                                     k_append_x_9016 true x0_9385 x1_9385 true x0_11595 x1_11595 false
                                                       true 0))
                                     else
                                       let
                                         x_5363 (k_append_x_x_9394:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                         x_x_x_3916 iii01_2861 iii11_2861 iii21_2861 k_append_x_x_9394
                                       in
                                       x_5363
                                         (fun (x00_9426:bool) ->
                                            fun (x01_9426:int) ->
                                              fun (x10_9426:bool) ->
                                                fun (x11_9426:int) ->
                                                  fun (x20_9426:bool) ->
                                                    fun (x21_9426:int) ->
                                                      k_append_x_9016 true x00_9426 x01_9426 true x10_9426 x11_9426
                                                        true x20_9426 x21_9426)
                               in
                               x_1807))
                     else
                       let x_1610 (k_append_x_9485:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                       x_1610
                         (fun (x_9943:(int -> (bool -> int -> X) -> X)) ->
                            k_append_7185
                              (let
                                 x_1761 (iii00_2432:bool) (iii01_2432:int) (iii10_2432:bool) (iii11_2432:int) 
                                       (iii20_2432:bool) (iii21_2432:int) 
                                       (k_append_x_9493:(bool ->
                                                           bool ->
                                                             r011_9490:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9490:
                                                                    int[\r111_9490. r011_9490 = r111_9490] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_2432 = false then
                                   if iii10_2432 = false then
                                     if iii20_2432 = false then
                                       k_append_x_9493 false true 0 false true 0 false true 0
                                     else
                                       x_1747 iii21_2432
                                         (fun (x0_11201:bool) ->
                                            fun (x1_11201:int) ->
                                              k_append_x_9493 false true 0 false true 0 true x0_11201 x1_11201)
                                   else
                                     if iii20_2432 = false then
                                       x_1746 iii11_2432
                                         (fun (x0_11188:bool) ->
                                            fun (x1_11188:int) ->
                                              k_append_x_9493 false true 0 true x0_11188 x1_11188 false true 0)
                                     else
                                       let x_4508 (k_append_x_x_9644:(bool -> int -> X)) =
                                         x_1746 iii11_2432 k_append_x_x_9644
                                       in
                                       x_4508
                                         (fun (x0_9692:bool) ->
                                            fun (x1_9692:int) ->
                                              x_1747 iii21_2432
                                                (fun (x0_11146:bool) ->
                                                   fun (x1_11146:int) ->
                                                     k_append_x_9493 false true 0 true x0_9692 x1_9692 true x0_11146
                                                       x1_11146))
                                 else
                                   if iii10_2432 = false then
                                     if iii20_2432 = false then
                                       x_9943 iii01_2432
                                         (fun (x0_11143:bool) ->
                                            fun (x1_11143:int) ->
                                              k_append_x_9493 true x0_11143 x1_11143 false true 0 false true 0)
                                     else
                                       let x_4414 (k_append_x_x_9755:(bool -> int -> X)) =
                                         x_9943 iii01_2432 k_append_x_x_9755
                                       in
                                       x_4414
                                         (fun (x0_9803:bool) ->
                                            fun (x1_9803:int) ->
                                              x_1747 iii21_2432
                                                (fun (x0_11094:bool) ->
                                                   fun (x1_11094:int) ->
                                                     k_append_x_9493 true x0_9803 x1_9803 false true 0 true x0_11094
                                                       x1_11094))
                                   else
                                     if iii20_2432 = false then
                                       let x_4373 (k_append_x_x_9814:(bool -> int -> X)) =
                                         x_9943 iii01_2432 k_append_x_x_9814
                                       in
                                       x_4373
                                         (fun (x0_9862:bool) ->
                                            fun (x1_9862:int) ->
                                              x_1746 iii11_2432
                                                (fun (x0_11086:bool) ->
                                                   fun (x1_11086:int) ->
                                                     k_append_x_9493 true x0_9862 x1_9862 true x0_11086 x1_11086 false
                                                       true 0))
                                     else
                                       let x_4339 (k_append_x_x_9869:(bool -> int -> X)) =
                                         x_9943 iii01_2432 k_append_x_x_9869
                                       in
                                       x_4339
                                         (fun (x0_9924:bool) ->
                                            fun (x1_9924:int) ->
                                              (let x_4349 (k_append_x_x_9881:(bool -> int -> X)) =
                                                 x_1746 iii11_2432 k_append_x_x_9881
                                               in
                                               x_4349
                                                 (fun (x0_9923:bool) ->
                                                    fun (x1_9923:int) ->
                                                      x_1747 iii21_2432
                                                        (fun (x0_11055:bool) ->
                                                           fun (x1_11055:int) ->
                                                             k_append_x_9493 true x0_9924 x1_9924 true x0_9923 x1_9923
                                                               true x0_11055 x1_11055))))
                               in
                               x_1761))))
 in
 let main_1015 (i_1016:int) (n_1017:int) (k_main_9985:(unit -> X)) =
   let x_6016 (k_main_x_9998:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1008 n_1017 k_main_x_9998 in
   x_6016
     (fun (x_10863:(int -> (bool -> int -> X) -> X)) ->
        (let f_1479 (x_1329:int) (k_main_f_10013:(bool -> int -> X)) = k_main_f_10013 false 0 in
         let
           x_1820 (ix00_2198:bool) (ix01_2198:int) (ix10_2198:bool) (ix11_2198:int) 
                 (k_main_x_10026:(bool ->
                                    bool ->
                                      r011_10025:int ->
                                        bool -> bool -> r111_10025:int[\r111_10025. r011_10025 = r111_10025] -> X)) =
           if ix00_2198 = false then
             if ix10_2198 = false then
               k_main_x_10026 false true 0 false true 0
             else
               f_1479 ix11_2198
                 (fun (x0_11995:bool) -> fun (x1_11995:int) -> k_main_x_10026 false true 0 true x0_11995 x1_11995)
           else
             if ix10_2198 = false then
               x_10863 ix01_2198
                 (fun (x0_11992:bool) -> fun (x1_11992:int) -> k_main_x_10026 true x0_11992 x1_11992 false true 0)
             else
               let x_6028 (k_main_x_x_10135:(bool -> int -> X)) = x_10863 ix01_2198 k_main_x_x_10135 in
               x_6028
                 (fun (x0_10169:bool) ->
                    fun (x1_10169:int) ->
                      f_1479 ix11_2198
                        (fun (x0_11974:bool) ->
                           fun (x1_11974:int) -> k_main_x_10026 true x0_10169 x1_10169 true x0_11974 x1_11974))
         in
         let
           x_6174
                 (k_main_x_10281:((bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               (bool ->
                                                  bool ->
                                                    r011_10278:int ->
                                                      bool ->
                                                        bool ->
                                                          r111_10278:
                                                            int[\r111_10278. r011_10278 = r111_10278] ->
                                                            bool -> bool -> int -> X) -> X) -> X)) =
           append_1059 x_1820 k_main_x_10281
         in
         x_6174
           (fun (x_10843:(bool ->
                            int ->
                              bool ->
                                int ->
                                  bool ->
                                    int ->
                                      (bool ->
                                         bool ->
                                           r011_10841:int ->
                                             bool ->
                                               bool ->
                                                 r111_10841:int[\r111_10841. r011_10841 = r111_10841] ->
                                                   bool -> bool -> int -> X) -> X)) ->
              (let x_1825 (i_2150:int) (k_main_x_10351:(bool -> int -> X)) =
                 x_10843 false 0 true i_2150 false 0
                   (fun (p00_12077:bool) ->
                      fun (p010_12077:bool) ->
                        fun (p011_12077:int) ->
                          fun (p10_12077:bool) ->
                            fun (p110_12077:bool) ->
                              fun (p111_12077:int) ->
                                fun (p20_12077:bool) ->
                                  fun (p210_12077:bool) -> fun (p211_12077:int) -> k_main_x_10351 p110_12077 p111_12077)
               in
               let x_1826 (i_2140:int) (k_main_x_10400:(bool -> int -> X)) =
                 x_10843 false 0 false 0 true i_2140
                   (fun (p00_12096:bool) ->
                      fun (p010_12096:bool) ->
                        fun (p011_12096:int) ->
                          fun (p10_12096:bool) ->
                            fun (p110_12096:bool) ->
                              fun (p111_12096:int) ->
                                fun (p20_12096:bool) ->
                                  fun (p210_12096:bool) -> fun (p211_12096:int) -> k_main_x_10400 p210_12096 p211_12096)
               in
               let rec x_x_4033 (x_3995:int) (x_3996:int) (k_main_x_x_10449:(bool -> int -> bool -> int -> X)) =
                 let
                   x_6517
                         (k_main_x_x_x_10482:(bool ->
                                                bool ->
                                                  r011_10481:int ->
                                                    bool ->
                                                      bool ->
                                                        r111_10481:int[\r111_10481. r011_10481 = r111_10481] ->
                                                          bool -> bool -> int -> X)) =
                   x_10843 false 0 true x_3995 true x_3996 k_main_x_x_x_10482
                 in
                 x_6517
                   (fun (x00_10494:bool) ->
                      fun (x010_10494:bool) ->
                        fun (x011_10494:int) ->
                          fun (x10_10494:bool) ->
                            fun (x110_10494:bool) ->
                              fun (x111_10494:int) ->
                                fun (x20_10494:bool) ->
                                  fun (x210_10494:bool) ->
                                    fun (x211_10494:int) ->
                                      k_main_x_x_10449 x110_10494 x111_10494 x210_10494 x211_10494)
               in
               let
                 x_1829 (ii00_2123:bool) (ii01_2123:int) (ii10_2123:bool) (ii11_2123:int) 
                       (k_main_x_10502:(bool ->
                                          bool ->
                                            r011_10501:int ->
                                              bool -> bool -> r111_10501:int[\r111_10501. r011_10501 = r111_10501] -> X)) =
                 if ii00_2123 = false then
                   if ii10_2123 = false then
                     k_main_x_10502 false true 0 false true 0
                   else
                     x_1826 ii11_2123
                       (fun (x0_12134:bool) ->
                          fun (x1_12134:int) -> k_main_x_10502 false true 0 true x0_12134 x1_12134)
                 else
                   if ii10_2123 = false then
                     x_1825 ii01_2123
                       (fun (x0_12131:bool) ->
                          fun (x1_12131:int) -> k_main_x_10502 true x0_12131 x1_12131 false true 0)
                   else
                     let x_6298 (k_main_x_x_10612:(bool -> int -> bool -> int -> X)) =
                       x_x_4033 ii01_2123 ii11_2123 k_main_x_x_10612
                     in
                     x_6298
                       (fun (x00_10636:bool) ->
                          fun (x01_10636:int) ->
                            fun (x10_10636:bool) ->
                              fun (x11_10636:int) -> k_main_x_10502 true x00_10636 x01_10636 true x10_10636 x11_10636)
               in
               let
                 x_6490
                       (k_main_x_10762:(bool ->
                                          bool ->
                                            r011_10761:int ->
                                              bool ->
                                                bool ->
                                                  r111_10761:int[\r111_10761. r011_10761 = r111_10761] ->
                                                    bool -> bool -> int -> X)) =
                 x_10843 true i_1016 false 0 false 0 k_main_x_10762
               in
               x_6490
                 (fun (x00_10810:bool) ->
                    fun (x010_10810:bool) ->
                      fun (x011_10810:int) ->
                        fun (x10_10810:bool) ->
                          fun (x110_10810:bool) ->
                            fun (x111_10810:int) ->
                              fun (x20_10810:bool) ->
                                fun (x210_10810:bool) ->
                                  fun (x211_10810:int) ->
                                    (let x_6467 (k_main_x_10774:(bool -> int -> X)) = x_10863 i_1016 k_main_x_10774 in
                                     x_6467
                                       (fun (x0_10809:bool) ->
                                          fun (x1_10809:int) ->
                                            (let n_1504 (k_main_n_10785:(int -> X)) =
                                               if x010_10810 <> false then
                                                 k_main_n_10785 x011_10810
                                               else
                                                 _|_
                                             in
                                             n_1504
                                               (fun (n_10808:int) ->
                                                  (let n_1505 (k_main_n_10793:(int -> X)) =
                                                     if x0_10809 <> false then
                                                       k_main_n_10793 x1_10809
                                                     else
                                                       _|_
                                                   in
                                                   n_1505
                                                     (fun (n_10807:int) ->
                                                        (if n_10808 = n_10807 then
                                                           k_main_9985 ()
                                                         else
                                                           {|fail|} () k_main_9985))))))))))))
 in
 let x_6485 (k_x_10874:(int -> X)) = rand_int_cps () k_x_10874 in
 x_6485
   (fun (x_10919:int) ->
      (let x_6487 (k_x_10886:(int -> X)) = rand_int_cps () k_x_10886 in
       x_6487
         (fun (x_10918:int) ->
            (let x_6489 (k_x_10907:(unit -> X)) = main_1015 x_10919 x_10918 k_x_10907 in
             x_6489 (fun (x_10913:unit) -> {end})))))

