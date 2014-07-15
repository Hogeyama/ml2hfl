MoCHi: Model Checker for Higher-Order Programs
  Build: 7412b8f (2014-07-15 18:33:31 +0900)
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

ADD: x_x_3820
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

ADD: xs'_x_3867
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

ADD: x_x_3919
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

ADD: x_x_x_3972
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

ADD: x_x_4018
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

ADD: x_x_4063
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

ADD: x_x_4108
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

ADD: x_x_x_4166
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

ADD: x_x_4212
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

ADD: x_x_4257
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

ADD: x_x_4302
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

ADD: x_x_4361
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
            ((false, (true, 0)), (true, #1 r_4303), (true, #2 r_4303))
      else
        if fst (#1 iii_3257) = false then
          if fst (#2 iii_3257) = false then
            ((true, x_1747 (snd (#0 iii_3257))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_4258 = x_x_4257 (snd (#0 iii_3257)) (snd (#2 iii_3257)) in
            ((true, #0 r_4258), (false, (true, 0)), (true, #2 r_4258))
        else
          if fst (#2 iii_3257) = false then
            let r_4213 = x_x_4212 (snd (#0 iii_3257)) (snd (#1 iii_3257)) in
            ((true, #0 r_4213), (true, #1 r_4213), (false, (true, 0)))
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
              ((false, (true, 0)), (true, #1 r_4109), (true, #2 r_4109))
        else
          if fst (#1 iii_2861) = false then
            if fst (#2 iii_2861) = false then
              ((true, x_1802 (snd (#0 iii_2861))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_4064 = x_x_4063 (snd (#0 iii_2861)) (snd (#2 iii_2861)) in
              ((true, #0 r_4064), (false, (true, 0)), (true, #2 r_4064))
          else
            if fst (#2 iii_2861) = false then
              let r_4019 = x_x_4018 (snd (#0 iii_2861)) (snd (#1 iii_2861)) in
              ((true, #0 r_4019), (true, #1 r_4019), (false, (true, 0)))
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
              ((false, (true, 0)), (true, #1 r_3821), (true, #2 r_3821))
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

check: (let (r_4261:(bool * int)) =
          ((#1 (#1 ((x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))
          (let (x1_4241:(bool * int)) =
             (let (x1_4233:bool) = (false:bool) 
              in
              (let (x2_4234:int) = (0:int) 
               in
                 (((x1_4233:bool), (x2_4234:int)):(bool * int)):(bool * int)):(
             bool * int)) 
           in
           (let (x2_4242:(bool * int)) =
              (let (x1_4237:bool) = (true:bool) 
               in
               (let (x2_4238:int) = (x_4231:int) 
                in
                  (((x1_4237:bool), (x2_4238:int)):(bool * int)):(bool * int)):(
              bool * int)) 
            in
              (((x1_4241:(bool * int)), (x2_4242:(bool * int))):((bool * int) * (bool * int))):(
           (bool * int) * (bool * int))):((bool * int) * (bool * int))):(
          (bool * (bool * int)) * (bool * (bool * int)))):(bool * (bool * int)))):(
          bool * int)) 
        in
        (let (r_4262:(bool * int)) =
           ((#1 (#1 ((x_1023:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))
           (let (x1_4253:(bool * int)) =
              (let (x1_4245:bool) = (false:bool) 
               in
               (let (x2_4246:int) = (0:int) 
                in
                  (((x1_4245:bool), (x2_4246:int)):(bool * int)):(bool * int)):(
              bool * int)) 
            in
            (let (x2_4254:(bool * int)) =
               (let (x1_4249:bool) = (true:bool) 
                in
                (let (x2_4250:int) = (x_4232:int) 
                 in
                   (((x1_4249:bool), (x2_4250:int)):(bool * int)):(bool * int)):(
               bool * int)) 
             in
               (((x1_4253:(bool * int)), (x2_4254:(bool * int))):((bool * int) * (bool * int))):(
            (bool * int) * (bool * int))):((bool * int) * (bool * int))):(
           (bool * (bool * int)) * (bool * (bool * int)))):(bool * (bool * int)))):(
           bool * int)) 
         in
           (((r_4261:(bool * int)), (r_4262:(bool * int))):((bool * int) * (bool * int))):(
        (bool * int) * (bool * int))):((bool * int) * (bool * int))), (
(bool * int) * (bool * int) * (bool * int))
Fatal error: exception Assert_failure("type_check.ml", 19, 59)
Raised at file "type_check.ml", line 19, characters 59-71
Called from file "list.ml", line 73, characters 12-15
Called from file "type_check.ml", line 76, characters 6-63
Called from file "list.ml", line 73, characters 12-15
Called from file "type_check.ml", line 76, characters 6-63
Called from file "util.ml", line 10, characters 24-27
Called from file "main_loop.ml", line 9, characters 10-13
Called from file "util.ml", line 19, characters 15-18
Called from file "main_loop.ml", line 212, characters 35-53
Called from file "mochi.ml", line 368, characters 9-17
