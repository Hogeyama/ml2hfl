MoCHi: Model Checker for Higher-Order Programs
  Build: _33d7fbb (after 2014-07-11 16:31:10 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil2_nth.ml -bool-init-empty -disable-rc -color -tupling -gchi -list-option
           -abs-remove-false

parsed:
 let rec append_1008 x_1014 =
   match x_1014 with
   | (xs_1009, ys_1010) ->
       (match xs_1009 with
        | [] -> ys_1010
        | x_1011::xs'_1012 -> x_1011::append_1008 (xs'_1012, ys_1010))
 in
 let rec make_list_1013 n_1014 = if n_1014 < 0 then
                                   []
                                 else
                                   rand_int ()::make_list_1013 (n_1014 - 1) in
 let rec list_eq_1015 xsys_1016 =
   match xsys_1016 with
   | ([], []) -> true
   | (x_1017::xs'_1018, y_1019::ys'_1020) -> x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
   | _ -> false
 in
 let main_1021 i_1022 n_1023 =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1008 (xs_1024, []) in
   if List.nth ys_1025 i_1022 = List.nth xs_1024 i_1022 then
     ()
   else
     {fail} ()
 in
 ()

set_target:
 let rec append_1008 (x_1014:(!!! list * !!! list)) =
   match x_1014 with
   | (xs_1009, ys_1010) ->
       (match xs_1009 with
        | [] -> ys_1010
        | x_1011::xs'_1012 -> x_1011::append_1008 (xs'_1012, ys_1010))
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let rec list_eq_1015 (xsys_1016:(!!! list * !!! list)) =
   match xsys_1016 with
   | ([], []) -> true
   | (x_1017::xs'_1018, y_1019::ys'_1020) -> x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
   | _ -> false
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1008 (xs_1024, []) in
   if List.nth ys_1025 i_1022 = List.nth xs_1024 i_1022 then
     ()
   else
     {fail} ()
 in
 let main_1081 = let arg1_1077 = rand_int () in
                 let arg2_1079 = rand_int () in
                 main_1021 arg1_1077 arg2_1079 in
 ()

make_ext_funs:
 let List.nth_1082 (x_1083:int list) (x_1084:int) = rand_int () in
 let rec append_1008 (x_1014:(!!! list * !!! list)) =
   match x_1014 with
   | (xs_1009, ys_1010) ->
       (match xs_1009 with
        | [] -> ys_1010
        | x_1011::xs'_1012 -> x_1011::append_1008 (xs'_1012, ys_1010))
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let rec list_eq_1015 (xsys_1016:(!!! list * !!! list)) =
   match xsys_1016 with
   | ([], []) -> true
   | (x_1017::xs'_1018, y_1019::ys'_1020) -> x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
   | _ -> false
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1008 (xs_1024, []) in
   if List.nth_1082 ys_1025 i_1022 = List.nth_1082 xs_1024 i_1022 then
     ()
   else
     {fail} ()
 in
 let main_1081 = let arg1_1077 = rand_int () in
                 let arg2_1079 = rand_int () in
                 main_1021 arg1_1077 arg2_1079 in
 ()

copy_poly:
 let List.nth_1082 (x_1083:int list) (x_1084:int) = rand_int () in
 let rec append_1085 (x_1014:(int list * int list)) =
   match x_1014 with
   | (xs_1009, ys_1010) ->
       (match xs_1009 with
        | [] -> ys_1010
        | x_1011::xs'_1012 -> x_1011::append_1085 (xs'_1012, ys_1010))
 in
 let rec make_list_1013 (n_1014:int) = if n_1014 < 0 then
                                         []
                                       else
                                         rand_int ()::make_list_1013 (n_1014 - 1) in
 let rec list_eq_1015 (xsys_1016:(unit list * unit list)) =
   match xsys_1016 with
   | ([], []) -> true
   | (x_1017::xs'_1018, y_1019::ys'_1020) -> x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
   | _ -> false
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1085 (xs_1024, []) in
   if List.nth_1082 ys_1025 i_1022 = List.nth_1082 xs_1024 i_1022 then
     ()
   else
     {fail} ()
 in
 let main_1081 = let arg1_1077 = rand_int () in
                 let arg2_1079 = rand_int () in
                 main_1021 arg1_1077 arg2_1079 in
 ()

encode_list:
 let List.nth_1082 (x_1083:(int -> (bool * int))) (x_1084:int) = rand_int () in
 let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1009 = fst x_1014 in
   let ys_1010 = snd x_1014 in
   if fst (xs_1009 0) = false then
     ys_1010
   else
     if fst (xs_1009 0) <> false then
       let xs'_1012 (x_1141:int) = xs_1009 (x_1141 + 1) in
       let x_1011 = snd (xs_1009 0) in
       let cons_1216 (x_1212:int) (xs_1213:(int -> (bool * int))) (i_1211:int) =
         if i_1211 = 0 then
           (true, x_1212)
         else
           xs_1213 (i_1211 - 1)
       in
       cons_1216 x_1011 (append_1085 (xs'_1012, ys_1010))
     else
       _|_
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1305:int) -> (false, 0)
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> (bool * int))) (i_1294:int) =
       if i_1294 = 0 then
         (true, x_1295)
       else
         xs_1296 (i_1294 - 1)
     in
     cons_1299 (rand_int ()) (make_list_1013 (n_1014 - 1))
 in
 let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
   if fst ((fst xsys_1016) 0) = false && fst ((snd xsys_1016) 0) = false then
     true
   else
     if fst ((fst xsys_1016) 0) <> false && fst ((snd xsys_1016) 0) <> false then
       let x_1017 = snd ((fst xsys_1016) 0) in
       let xs'_1018 (x_1316:int) = (fst xsys_1016) (x_1316 + 1) in
       let y_1019 = snd ((snd xsys_1016) 0) in
       let ys'_1020 (x_1320:int) = (snd xsys_1016) (x_1320 + 1) in
       x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
     else
       false
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1085 (xs_1024, (fun (x_1452:int) -> (false, 0))) in
   if (let x_1502 = ys_1025 i_1022 in
       if fst x_1502 <> false then
         snd x_1502
       else
         _|_)
      = (let x_1492 = xs_1024 i_1022 in
         if fst x_1492 <> false then
           snd x_1492
         else
           _|_) then
     ()
   else
     {fail} ()
 in
 let main_1081 = let arg1_1077 = rand_int () in
                 let arg2_1079 = rand_int () in
                 main_1021 arg1_1077 arg2_1079 in
 ()

ret_fun:
 let List.nth_1082 (x_1083:(int -> (bool * int))) =
   let x_1788 = fun (x_1084:int) -> (let f_1506 = rand_int in
                                     let n_1508 = f_1506 () in
                                     n_1508) in
   (x_1788, x_1083)
 in
 let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1009 = fst x_1014 in
   let ys_1010 = snd x_1014 in
   let p_1514 = xs_1009 0 in
   let b_1516 = fst p_1514 in
   let b_1511 = b_1516 = false in
   if b_1511 then
     let x_1842 = fst x_1014 in
     let x_1843 = snd x_1014 in
     (ys_1010, x_1842, x_1843)
   else
     let p_1521 = xs_1009 0 in
     let b_1523 = fst p_1521 in
     let b_1525 = b_1523 = false in
     let b_1518 = not b_1525 in
     if b_1518 then
       let xs'_1012 (x_1141:int) = let n_1529 = x_1141 + 1 in
                                   let p_1530 = xs_1009 n_1529 in
                                   p_1530 in
       let p_1533 = xs_1009 0 in
       let x_1011 = snd p_1533 in
       let cons_1216 (x_1212:int) (xs_1213:(int -> (bool * int))) =
         let x_1813 (i_1211:int) =
           let b_1535 = i_1211 = 0 in
           if b_1535 then
             (true, x_1212)
           else
             let n_1545 = i_1211 - 1 in
             let p_1546 = xs_1213 n_1545 in
             let xs_1718 (n_1719:int) = if n_1719 = n_1545 then
                                          p_1546
                                        else
                                          xs_1213 n_1719 in
             p_1546
         in
         (x_1813, xs_1213)
       in
       let p_1552 = (xs'_1012, ys_1010) in
       let p_1734 = append_1085 p_1552 in
       let f_1553 = #0 p_1734 in
       let p_1735 = (#1 p_1734, #2 p_1734) in
       let f_1557 = cons_1216 x_1011 in
       let p_1730 = f_1557 f_1553 in
       let f_1558 = fst p_1730 in
       let f_1731 = snd p_1730 in
       let x_1834 = fst x_1014 in
       let x_1835 = snd x_1014 in
       (f_1558, x_1834, x_1835)
     else
       let x_1798 = _|_ in
       let x_1801 = fst x_1014 in
       let x_1802 = snd x_1014 in
       (x_1798, x_1801, x_1802)
 in
 let rec make_list_1013 (n_1014:int) =
   let b_1561 = n_1014 < 0 in
   if b_1561 then
     fun (x_1305:int) -> (false, 0)
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> (bool * int))) =
       let x_1853 =
         fun (i_1294:int) ->
           (let b_1568 = i_1294 = 0 in
            if b_1568 then
              (true, x_1295)
            else
              let n_1578 = i_1294 - 1 in
              let p_1579 = xs_1296 n_1578 in
              let xs_1752 (n_1753:int) = if n_1753 = n_1578 then
                                           p_1579
                                         else
                                           xs_1296 n_1753 in
              p_1579)
       in
       (x_1853, xs_1296)
     in
     let n_1586 = n_1014 - 1 in
     let f_1587 = make_list_1013 n_1586 in
     let f_1580 = rand_int in
     let n_1582 = f_1580 () in
     let f_1591 = cons_1299 n_1582 in
     let p_1763 = f_1591 f_1587 in
     let f_1592 = fst p_1763 in
     let f_1764 = snd p_1763 in
     f_1592
 in
 let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
   let f_1596 = fst xsys_1016 in
   let p_1598 = f_1596 0 in
   let b_1600 = fst p_1598 in
   let b_1609 = b_1600 = false in
   let f_1603 = snd xsys_1016 in
   let p_1605 = f_1603 0 in
   let b_1607 = fst p_1605 in
   let b_1610 = b_1607 = false in
   let b_1594 = b_1609 && b_1610 in
   if b_1594 then
     let x_1910 = fst xsys_1016 in
     let x_1911 = snd xsys_1016 in
     (true, x_1910, x_1911)
   else
     let f_1613 = fst xsys_1016 in
     let p_1615 = f_1613 0 in
     let b_1617 = fst p_1615 in
     let b_1619 = b_1617 = false in
     let b_1628 = not b_1619 in
     let f_1621 = snd xsys_1016 in
     let p_1623 = f_1621 0 in
     let b_1625 = fst p_1623 in
     let b_1627 = b_1625 = false in
     let b_1629 = not b_1627 in
     let b_1611 = b_1628 && b_1629 in
     if b_1611 then
       let f_1631 = fst xsys_1016 in
       let p_1633 = f_1631 0 in
       let x_1017 = snd p_1633 in
       let xs'_1018 (x_1316:int) =
         let n_1639 = x_1316 + 1 in
         let f_1638 = fst xsys_1016 in
         let p_1640 = f_1638 n_1639 in
         p_1640
       in
       let f_1642 = snd xsys_1016 in
       let p_1644 = f_1642 0 in
       let y_1019 = snd p_1644 in
       let ys'_1020 (x_1320:int) =
         let n_1650 = x_1320 + 1 in
         let f_1649 = snd xsys_1016 in
         let p_1651 = f_1649 n_1650 in
         p_1651
       in
       let b_1661 = x_1017 = y_1019 in
       let p_1659 = (xs'_1018, ys'_1020) in
       let p_1769 = list_eq_1015 p_1659 in
       let b_1660 = #0 p_1769 in
       let p_1770 = (#1 p_1769, #2 p_1769) in
       let x_1899 = b_1661 && b_1660 in
       let x_1902 = fst xsys_1016 in
       let x_1903 = snd xsys_1016 in
       (x_1899, x_1902, x_1903)
     else
       let x_1878 = fst xsys_1016 in
       let x_1879 = snd xsys_1016 in
       (false, x_1878, x_1879)
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let f_1665 = make_list_1013 n_1023 in
   let f_1671 (x_1452:int) = (false, 0) in
   let p_1675 = (f_1665, f_1671) in
   let p_1784 = append_1085 p_1675 in
   let f_1676 = #0 p_1784 in
   let p_1785 = (#1 p_1784, #2 p_1784) in
   let p_1680 = f_1676 i_1022 in
   let b_1683 = fst p_1680 in
   let b_1685 = b_1683 = false in
   let b_1681 = not b_1685 in
   let n_1696 = if b_1681 then
                  snd p_1680
                else
                  _|_ in
   let p_1689 = f_1665 i_1022 in
   let b_1692 = fst p_1689 in
   let b_1694 = b_1692 = false in
   let b_1690 = not b_1694 in
   let n_1697 = if b_1690 then
                  snd p_1689
                else
                  _|_ in
   let b_1677 = n_1696 = n_1697 in
   if b_1677 then
     ()
   else
     let f_1698 = {fail} in
     let u_1700 = f_1698 () in
     u_1700
 in
 let f_1701 = rand_int in
 let n_1703 = f_1701 () in
 let f_1704 = rand_int in
 let n_1706 = f_1704 () in
 let f_1710 = main_1021 n_1703 in
 let u_1711 = f_1710 n_1706 in
 ()

ref_trans:
 let List.nth_1082 (x_1083:(int -> (bool * int))) =
   let x_1788 (x_1084:int) = rand_int () in
   let x_1997 (xi_4414:((bool * int) * (bool * int))) =
     ((if fst (fst xi_4414) = false then
         (false, 0)
       else
         (true, x_1788 (snd (fst xi_4414)))),
      (if fst (snd xi_4414) = false then
         (false, (true, 0))
       else
         (true, x_1083 (snd (snd xi_4414)))))
   in
   x_1997
 in
 let rec append_1085 (x_1014:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1998 (i_4297:int) = snd (fst (x_1014 ((true, i_4297), (false, 0)))) in
   let x_1999 (i_4290:int) = snd (snd (x_1014 ((false, 0), (true, i_4290)))) in
   let x_2000 = let x_4289 = x_1014 ((true, 0), (false, 0)) in
                snd (fst x_4289) in
   let x_2002 = snd x_2000 in
   if fst x_2000 = false then
     let x_2063 (iii_4244:((bool * int) * (bool * int) * (bool * int))) =
       ((if fst (#0 iii_4244) = false then
           (false, (true, 0))
         else
           (true, x_1999 (snd (#0 iii_4244)))),
        (if fst (#1 iii_4244) = false then
           (false, (true, 0))
         else
           (true, x_1998 (snd (#1 iii_4244)))),
        (if fst (#2 iii_4244) = false then
           (false, (true, 0))
         else
           (true, x_1999 (snd (#2 iii_4244)))))
     in
     x_2063
   else
     let x_2005 = let x_4167 = x_1014 ((true, 0), (false, 0)) in
                  snd (fst x_4167) in
     let x_2007 = snd x_2005 in
     if fst x_2005 <> false then
       let xs'_1012 (x_1141:int) =
         let x_2016 = let x_4146 = x_1014 ((true, x_1141 + 1), (false, 0)) in
                      snd (fst x_4146) in
         let x_2017 = fst x_2016 in
         let x_2018 = snd x_2016 in
         x_2016
       in
       let x_2019 = let x_4125 = x_1014 ((true, 0), (false, 0)) in
                    snd (fst x_4125) in
       let x_2020 = fst x_2019 in
       let cons_1216 (x_1212:int) (xs_1213:(int -> (bool * int))) =
         let x_1813 (i_1211:int) =
           if i_1211 = 0 then
             (true, x_1212)
           else
             let x_2026 = xs_1213 (i_1211 - 1) in
             let x_2027 = fst x_2026 in
             let x_2028 = snd x_2026 in
             let xs_1718 (n_1719:int) = if n_1719 = i_1211 - 1 then
                                          x_2026
                                        else
                                          xs_1213 n_1719 in
             x_2026
         in
         let x_2037 (ii_4073:((bool * int) * (bool * int))) =
           ((if fst (fst ii_4073) = false then
               (false, (true, 0))
             else
               (true, x_1813 (snd (fst ii_4073)))),
            (if fst (snd ii_4073) = false then
               (false, (true, 0))
             else
               (true, xs_1213 (snd (snd ii_4073)))))
         in
         x_2037
       in
       let x_2040 (ii_3991:((bool * int) * (bool * int))) =
         ((if fst (fst ii_3991) = false then
             (false, (true, 0))
           else
             (true, xs'_1012 (snd (fst ii_3991)))),
          (if fst (snd ii_3991) = false then
             (false, (true, 0))
           else
             (true, x_1999 (snd (snd ii_3991)))))
       in
       let x_2041 (i_3971:int) = snd (fst (x_2040 ((true, i_3971), (false, 0)))) in
       let x_2042 (i_3964:int) = snd (snd (x_2040 ((false, 0), (true, i_3964)))) in
       let x_2043 = append_1085 x_2040 in
       let x_2044 (i_3953:int) = snd (#0 (x_2043 ((true, i_3953), (false, 0), (false, 0)))) in
       let x_2045 (i_3943:int) = snd (#1 (x_2043 ((false, 0), (true, i_3943), (false, 0)))) in
       let x_2046 (i_3933:int) = snd (#2 (x_2043 ((false, 0), (false, 0), (true, i_3933)))) in
       let x_2049 (ii_3916:((bool * int) * (bool * int))) =
         ((if fst (fst ii_3916) = false then
             (false, (true, 0))
           else
             (true, x_2045 (snd (fst ii_3916)))),
          (if fst (snd ii_3916) = false then
             (false, (true, 0))
           else
             (true, x_2046 (snd (snd ii_3916)))))
       in
       let x_2050 (i_3896:int) = snd (fst (x_2049 ((true, i_3896), (false, 0)))) in
       let x_2051 (i_3889:int) = snd (snd (x_2049 ((false, 0), (true, i_3889)))) in
       let x_2052 = cons_1216 (snd x_2019) in
       let x_2053 = x_2052 x_2044 in
       let x_2054 (i_3880:int) = snd (fst (x_2053 ((true, i_3880), (false, 0)))) in
       let x_2055 (i_3873:int) = snd (snd (x_2053 ((false, 0), (true, i_3873)))) in
       let x_2059 (iii_3848:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_3848) = false then
             (false, (true, 0))
           else
             (true, x_2054 (snd (#0 iii_3848)))),
          (if fst (#1 iii_3848) = false then
             (false, (true, 0))
           else
             (true, x_1998 (snd (#1 iii_3848)))),
          (if fst (#2 iii_3848) = false then
             (false, (true, 0))
           else
             (true, x_1999 (snd (#2 iii_3848)))))
       in
       x_2059
     else
       let x_1798 = _|_ in
       let x_2013 (iii_3419:((bool * int) * (bool * int) * (bool * int))) =
         ((if fst (#0 iii_3419) = false then
             (false, (true, 0))
           else
             (true, x_1798 (snd (#0 iii_3419)))),
          (if fst (#1 iii_3419) = false then
             (false, (true, 0))
           else
             (true, x_1998 (snd (#1 iii_3419)))),
          (if fst (#2 iii_3419) = false then
             (false, (true, 0))
           else
             (true, x_1999 (snd (#2 iii_3419)))))
       in
       x_2013
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1305:int) -> (false, 0)
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> (bool * int))) =
       let x_1853 (i_1294:int) =
         if i_1294 = 0 then
           (true, x_1295)
         else
           let x_2070 = xs_1296 (i_1294 - 1) in
           let xs_1752 (n_1753:int) = if n_1753 = i_1294 - 1 then
                                        x_2070
                                      else
                                        xs_1296 n_1753 in
           x_2070
       in
       let x_2079 (ii_3171:((bool * int) * (bool * int))) =
         ((if fst (fst ii_3171) = false then
             (false, (true, 0))
           else
             (true, x_1853 (snd (fst ii_3171)))),
          (if fst (snd ii_3171) = false then
             (false, (true, 0))
           else
             (true, xs_1296 (snd (snd ii_3171)))))
       in
       x_2079
     in
     let x_2082 = make_list_1013 (n_1014 - 1) in
     let x_2083 = rand_int () in
     let x_2084 = cons_1299 x_2083 in
     let x_2085 = x_2084 x_2082 in
     let x_2086 (i_3095:int) = snd (fst (x_2085 ((true, i_3095), (false, 0)))) in
     let x_2087 (i_3088:int) = snd (snd (x_2085 ((false, 0), (true, i_3088)))) in
     x_2086
 in
 let rec
   list_eq_1015 (xsys_1016:(((bool * int) * (bool * int)) -> ((bool * (bool * unit)) * (bool * (bool * unit))))) =
   let x_2093 (i_2977:int) = snd (fst (xsys_1016 ((true, i_2977), (false, 0)))) in
   let x_2094 (i_2970:int) = snd (snd (xsys_1016 ((false, 0), (true, i_2970)))) in
   let x_2095 = let x_2969 = xsys_1016 ((true, 0), (false, 0)) in
                snd (fst x_2969) in
   let x_2097 = snd x_2095 in
   let x_2100 = let x_2948 = xsys_1016 ((true, 0), (true, 0)) in
                snd (snd x_2948) in
   let x_2102 = snd x_2100 in
   if fst x_2095 = false && fst x_2100 = false then
     (true, x_2093, x_2094)
   else
     let x_2106 = let x_2927 = xsys_1016 ((true, 0), (true, 0)) in
                  snd (fst x_2927) in
     let x_2108 = snd x_2106 in
     let x_2111 = let x_2906 = xsys_1016 ((true, 0), (true, 0)) in
                  snd (snd x_2906) in
     let x_2113 = snd x_2111 in
     if fst x_2106 <> false && fst x_2111 <> false then
       let x_2122 = let x_2885 = xsys_1016 ((true, 0), (true, 0)) in
                    snd (fst x_2885) in
       let x_2123 = fst x_2122 in
       let xs'_1018 (x_1316:int) =
         let x_2127 = let x_2864 = xsys_1016 ((true, x_1316 + 1), (true, 0)) in
                      snd (fst x_2864) in
         let x_2128 = fst x_2127 in
         let x_2129 = snd x_2127 in
         x_2127
       in
       let x_2130 = let x_2843 = xsys_1016 ((true, 0), (true, 0)) in
                    snd (snd x_2843) in
       let x_2131 = fst x_2130 in
       let ys'_1020 (x_1320:int) =
         let x_2135 = let x_2822 = xsys_1016 ((true, 0), (true, x_1320 + 1)) in
                      snd (snd x_2822) in
         let x_2136 = fst x_2135 in
         let x_2137 = snd x_2135 in
         x_2135
       in
       let x_2141 (ii_2785:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2785) = false then
             (false, (true, ()))
           else
             (true, xs'_1018 (snd (fst ii_2785)))),
          (if fst (snd ii_2785) = false then
             (false, (true, ()))
           else
             (true, ys'_1020 (snd (snd ii_2785)))))
       in
       let x_2142 (i_2765:int) = snd (fst (x_2141 ((true, i_2765), (false, 0)))) in
       let x_2143 (i_2758:int) = snd (snd (x_2141 ((false, 0), (true, i_2758)))) in
       let x_2144 = list_eq_1015 x_2141 in
       let x_2150 (ii_2740:((bool * int) * (bool * int))) =
         ((if fst (fst ii_2740) = false then
             (false, (true, ()))
           else
             (true, (#1 x_2144) (snd (fst ii_2740)))),
          (if fst (snd ii_2740) = false then
             (false, (true, ()))
           else
             (true, (#2 x_2144) (snd (snd ii_2740)))))
       in
       let x_2151 (i_2720:int) = snd (fst (x_2150 ((true, i_2720), (false, 0)))) in
       let x_2152 (i_2713:int) = snd (snd (x_2150 ((false, 0), (true, i_2713)))) in
       (snd x_2122 = snd x_2130 && #0 x_2144, x_2093, x_2094)
     else
       (false, x_2093, x_2094)
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let x_2163 = make_list_1013 n_1023 in
   let f_1671 (x_1452:int) = (false, 0) in
   let x_2171 (ix_2549:((bool * int) * (bool * int))) =
     ((if fst (fst ix_2549) = false then
         (false, (true, 0))
       else
         (true, x_2163 (snd (fst ix_2549)))),
      (if fst (snd ix_2549) = false then
         (false, (true, 0))
       else
         (true, f_1671 (snd (snd ix_2549)))))
   in
   let x_2172 (i_2529:int) = snd (fst (x_2171 ((true, i_2529), (false, 0)))) in
   let x_2173 (x_2522:int) = snd (snd (x_2171 ((false, 0), (true, x_2522)))) in
   let x_2174 = append_1085 x_2171 in
   let x_2175 (i_2511:int) = snd (#0 (x_2174 ((true, i_2511), (false, 0), (false, 0)))) in
   let x_2176 (i_2501:int) = snd (#1 (x_2174 ((false, 0), (true, i_2501), (false, 0)))) in
   let x_2177 (i_2491:int) = snd (#2 (x_2174 ((false, 0), (false, 0), (true, i_2491)))) in
   let x_2180 (ii_2474:((bool * int) * (bool * int))) =
     ((if fst (fst ii_2474) = false then
         (false, (true, 0))
       else
         (true, x_2176 (snd (fst ii_2474)))),
      (if fst (snd ii_2474) = false then
         (false, (true, 0))
       else
         (true, x_2177 (snd (snd ii_2474)))))
   in
   let x_2181 (i_2454:int) = snd (fst (x_2180 ((true, i_2454), (false, 0)))) in
   let x_2182 (i_2447:int) = snd (snd (x_2180 ((false, 0), (true, i_2447)))) in
   let x_2183 = let x_2446 = x_2174 ((true, i_1022), (false, 0), (false, 0)) in
                snd (#0 x_2446) in
   let n_1696 = if fst x_2183 <> false then
                  snd x_2183
                else
                  _|_ in
   let x_2188 = x_2163 i_1022 in
   let n_1697 = if fst x_2188 <> false then
                  snd x_2188
                else
                  _|_ in
   if n_1696 = n_1697 then
     ()
   else
     {fail} ()
 in
 let x_2195 = rand_int () in
 let x_2196 = rand_int () in
 let x_2197 = main_1021 x_2195 in
 let x_2198 = x_2197 x_2196 in
 ()

#0 (bool * int), int
#1 (bool * int), int
#2 (bool * int), int
#0 (bool * int), int
#1 (bool * int), int
#2 (bool * int), int
#0 (bool * int), int
#1 (bool * int), int
#2 (bool * int), int
tupling:
 let rec append_1085 (x_1014:(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))) =
   let x_1998 (i_4297:int) = let x_4957 = x_1014 ((true, i_4297), (false, 0)) in
                             snd (fst x_4957) in
   let x_1999 (i_4290:int) = let x_4976 = x_1014 ((false, 0), (true, i_4290)) in
                             snd (snd x_4976) in
   let rec x_x_x_4829 (x_4790:int) (x_4791:int) (x_4792:int) =
     let x_4990 = x_1014 ((false, 0), (true, x_4790)) in
     let x_5004 = x_1014 ((true, x_4791), (false, 0)) in
     let x_5018 = x_1014 ((false, 0), (true, x_4792)) in
     (snd (snd x_4990), snd (fst x_5004), snd (snd x_5018))
   in
   let x_5040 = x_1014 ((true, 0), (false, 0)) in
   if fst (snd (fst x_5040)) = false then
     let x_2063 (iii_4244:((bool * int) * (bool * int) * (bool * int))) =
       if fst (#0 iii_4244) = false then
         let x_6256 =
           if fst (#1 iii_4244) = false then
             (false, (true, 0))
           else
             let x_6262 = x_1998 (snd (#1 iii_4244)) in
             (true, x_6262)
         in
         let x_6282 =
           if fst (#2 iii_4244) = false then
             (false, (true, 0))
           else
             let x_6288 = x_1999 (snd (#2 iii_4244)) in
             (true, x_6288)
         in
         ((false, (true, 0)), x_6256, x_6282)
       else
         if fst (#1 iii_4244) = false then
           let x_6194 = x_1999 (snd (#0 iii_4244)) in
           let x_6209 =
             if fst (#2 iii_4244) = false then
               (false, (true, 0))
             else
               let x_6215 = x_1999 (snd (#2 iii_4244)) in
               (true, x_6215)
           in
           ((true, x_6194), (false, (true, 0)), x_6209)
         else
           if fst (#2 iii_4244) = false then
             let x_6153 = x_1999 (snd (#0 iii_4244)) in
             let x_6163 = x_1998 (snd (#1 iii_4244)) in
             ((true, x_6153), (true, x_6163), (false, (true, 0)))
           else
             let x_6121 = x_x_x_4829 (snd (#0 iii_4244)) (snd (#1 iii_4244)) (snd (#2 iii_4244)) in
             ((true, #0 x_6121), (true, #1 x_6121), (true, #2 x_6121))
     in
     x_2063
   else
     if fst (snd (fst x_5040)) <> false then
       let xs'_1012 (x_1141:int) = let x_5269 = x_1014 ((true, x_1141 + 1), (false, 0)) in
                                   snd (fst x_5269) in
       let rec xs'_x_4665 (x_4639:int) (x_4640:int) =
         let x_5284 = x_1014 ((true, x_4639 + 1), (false, 0)) in
         let x_5299 = x_1014 ((false, 0), (true, x_4640)) in
         (snd (fst x_5284), snd (snd x_5299))
       in
       let cons_1216 (x_1212:int) (xs_1213:(int -> (bool * int))) =
         let x_1813 (i_1211:int) =
           if i_1211 = 0 then
             (true, x_1212)
           else
             let x_5310 = xs_1213 (i_1211 - 1) in
             let xs_1718 (n_1719:int) = if n_1719 = i_1211 - 1 then
                                          x_5310
                                        else
                                          xs_1213 n_1719 in
             x_5310
         in
         let x_2037 (ii_4073:((bool * int) * (bool * int))) =
           if fst (fst ii_4073) = false then
             let x_5386 =
               if fst (snd ii_4073) = false then
                 (false, (true, 0))
               else
                 let x_5392 = xs_1213 (snd (snd ii_4073)) in
                 (true, x_5392)
             in
             ((false, (true, 0)), x_5386)
           else
             if fst (snd ii_4073) = false then
               let x_5351 = x_1813 (snd (fst ii_4073)) in
               ((true, x_5351), (false, (true, 0)))
             else
               let x_5328 = x_1813 (snd (fst ii_4073)) in
               let x_5338 = xs_1213 (snd (snd ii_4073)) in
               ((true, x_5328), (true, x_5338))
         in
         x_2037
       in
       let x_2040 (ii_3991:((bool * int) * (bool * int))) =
         if fst (fst ii_3991) = false then
           let x_5486 =
             if fst (snd ii_3991) = false then
               (false, (true, 0))
             else
               let x_5492 = x_1999 (snd (snd ii_3991)) in
               (true, x_5492)
           in
           ((false, (true, 0)), x_5486)
         else
           if fst (snd ii_3991) = false then
             let x_5451 = xs'_1012 (snd (fst ii_3991)) in
             ((true, x_5451), (false, (true, 0)))
           else
             let x_5427 = xs'_x_4665 (snd (fst ii_3991)) (snd (snd ii_3991)) in
             ((true, fst x_5427), (true, snd x_5427))
       in
       let x_2041 (i_3971:int) = let x_5537 = x_2040 ((true, i_3971), (false, 0)) in
                                 snd (fst x_5537) in
       let x_2042 (i_3964:int) = let x_5556 = x_2040 ((false, 0), (true, i_3964)) in
                                 snd (snd x_5556) in
       let x_5559 = append_1085 x_2040 in
       let x_2044 (i_3953:int) = let x_5583 = x_5559 ((true, i_3953), (false, 0), (false, 0)) in
                                 snd (#0 x_5583) in
       let x_2045 (i_3943:int) = let x_5609 = x_5559 ((false, 0), (true, i_3943), (false, 0)) in
                                 snd (#1 x_5609) in
       let x_2046 (i_3933:int) = let x_5635 = x_5559 ((false, 0), (false, 0), (true, i_3933)) in
                                 snd (#2 x_5635) in
       let rec x_x_4717 (x_4679:int) (x_4680:int) =
         let x_5653 = x_5559 ((false, 0), (true, x_4679), (false, 0)) in
         let x_5671 = x_5559 ((false, 0), (false, 0), (true, x_4680)) in
         let x_7440 = x_5559 ((false, 0), (true, x_4679), (true, x_4680)) in
         (snd (#1 x_7440), snd (#2 x_7440))
       in
       let x_2049 (ii_3916:((bool * int) * (bool * int))) =
         if fst (fst ii_3916) = false then
           let x_5742 =
             if fst (snd ii_3916) = false then
               (false, (true, 0))
             else
               let x_5748 = x_2046 (snd (snd ii_3916)) in
               (true, x_5748)
           in
           ((false, (true, 0)), x_5742)
         else
           if fst (snd ii_3916) = false then
             let x_5707 = x_2045 (snd (fst ii_3916)) in
             ((true, x_5707), (false, (true, 0)))
           else
             let x_5683 = x_x_4717 (snd (fst ii_3916)) (snd (snd ii_3916)) in
             ((true, fst x_5683), (true, snd x_5683))
       in
       let x_2050 (i_3896:int) = let x_5793 = x_2049 ((true, i_3896), (false, 0)) in
                                 snd (fst x_5793) in
       let x_2051 (i_3889:int) = let x_5812 = x_2049 ((false, 0), (true, i_3889)) in
                                 snd (snd x_5812) in
       let x_5816 = cons_1216 (snd (snd (fst x_5040))) in
       let x_5817 = x_5816 x_2044 in
       let x_2054 (i_3880:int) = let x_5834 = x_5817 ((true, i_3880), (false, 0)) in
                                 snd (fst x_5834) in
       let rec x_x_x_4770 (x_4731:int) (x_4732:int) (x_4733:int) =
         let x_5848 = x_5817 ((true, x_4731), (false, 0)) in
         let x_5862 = x_1014 ((true, x_4732), (false, 0)) in
         let x_5876 = x_1014 ((false, 0), (true, x_4733)) in
         (snd (fst x_5848), snd (fst x_5862), snd (snd x_5876))
       in
       let x_2055 (i_3873:int) = let x_5899 = x_5817 ((false, 0), (true, i_3873)) in
                                 snd (snd x_5899) in
       let x_2059 (iii_3848:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_3848) = false then
           let x_6046 =
             if fst (#1 iii_3848) = false then
               (false, (true, 0))
             else
               let x_6052 = x_1998 (snd (#1 iii_3848)) in
               (true, x_6052)
           in
           let x_6072 =
             if fst (#2 iii_3848) = false then
               (false, (true, 0))
             else
               let x_6078 = x_1999 (snd (#2 iii_3848)) in
               (true, x_6078)
           in
           ((false, (true, 0)), x_6046, x_6072)
         else
           if fst (#1 iii_3848) = false then
             let x_5984 = x_2054 (snd (#0 iii_3848)) in
             let x_5999 =
               if fst (#2 iii_3848) = false then
                 (false, (true, 0))
               else
                 let x_6005 = x_1999 (snd (#2 iii_3848)) in
                 (true, x_6005)
             in
             ((true, x_5984), (false, (true, 0)), x_5999)
           else
             if fst (#2 iii_3848) = false then
               let x_5943 = x_2054 (snd (#0 iii_3848)) in
               let x_5953 = x_1998 (snd (#1 iii_3848)) in
               ((true, x_5943), (true, x_5953), (false, (true, 0)))
             else
               let x_5911 = x_x_x_4770 (snd (#0 iii_3848)) (snd (#1 iii_3848)) (snd (#2 iii_3848)) in
               ((true, #0 x_5911), (true, #1 x_5911), (true, #2 x_5911))
       in
       x_2059
     else
       let x_1798 = _|_ in
       let x_2013 (iii_3419:((bool * int) * (bool * int) * (bool * int))) =
         if fst (#0 iii_3419) = false then
           let x_5190 =
             if fst (#1 iii_3419) = false then
               (false, (true, 0))
             else
               let x_5196 = x_1998 (snd (#1 iii_3419)) in
               (true, x_5196)
           in
           let x_5216 =
             if fst (#2 iii_3419) = false then
               (false, (true, 0))
             else
               let x_5222 = x_1999 (snd (#2 iii_3419)) in
               (true, x_5222)
           in
           ((false, (true, 0)), x_5190, x_5216)
         else
           if fst (#1 iii_3419) = false then
             let x_5128 = x_1798 (snd (#0 iii_3419)) in
             let x_5143 =
               if fst (#2 iii_3419) = false then
                 (false, (true, 0))
               else
                 let x_5149 = x_1999 (snd (#2 iii_3419)) in
                 (true, x_5149)
             in
             ((true, x_5128), (false, (true, 0)), x_5143)
           else
             if fst (#2 iii_3419) = false then
               let x_5087 = x_1798 (snd (#0 iii_3419)) in
               let x_5097 = x_1998 (snd (#1 iii_3419)) in
               ((true, x_5087), (true, x_5097), (false, (true, 0)))
             else
               let x_5053 = x_1798 (snd (#0 iii_3419)) in
               let x_5063 = x_1998 (snd (#1 iii_3419)) in
               let x_5073 = x_1999 (snd (#2 iii_3419)) in
               ((true, x_5053), (true, x_5063), (true, x_5073))
       in
       x_2013
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1305:int) -> (false, 0)
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> (bool * int))) =
       let x_1853 (i_1294:int) =
         if i_1294 = 0 then
           (true, x_1295)
         else
           let x_6324 = xs_1296 (i_1294 - 1) in
           let xs_1752 (n_1753:int) = if n_1753 = i_1294 - 1 then
                                        x_6324
                                      else
                                        xs_1296 n_1753 in
           x_6324
       in
       let x_2079 (ii_3171:((bool * int) * (bool * int))) =
         if fst (fst ii_3171) = false then
           let x_6400 =
             if fst (snd ii_3171) = false then
               (false, (true, 0))
             else
               let x_6406 = xs_1296 (snd (snd ii_3171)) in
               (true, x_6406)
           in
           ((false, (true, 0)), x_6400)
         else
           if fst (snd ii_3171) = false then
             let x_6365 = x_1853 (snd (fst ii_3171)) in
             ((true, x_6365), (false, (true, 0)))
           else
             let x_6342 = x_1853 (snd (fst ii_3171)) in
             let x_6352 = xs_1296 (snd (snd ii_3171)) in
             ((true, x_6342), (true, x_6352))
       in
       x_2079
     in
     let x_6437 = make_list_1013 (n_1014 - 1) in
     let x_6439 = rand_int () in
     let x_6440 = cons_1299 x_6439 in
     let x_6441 = x_6440 x_6437 in
     let x_2086 (i_3095:int) = let x_6458 = x_6441 ((true, i_3095), (false, 0)) in
                               snd (fst x_6458) in
     let x_2087 (i_3088:int) = let x_6477 = x_6441 ((false, 0), (true, i_3088)) in
                               snd (snd x_6477) in
     x_2086
 in
 let rec
   list_eq_1015 (xsys_1016:(((bool * int) * (bool * int)) -> ((bool * (bool * unit)) * (bool * (bool * unit))))) =
   let x_2093 (i_2977:int) = let x_6503 = xsys_1016 ((true, i_2977), (false, 0)) in
                             snd (fst x_6503) in
   let x_2094 (i_2970:int) = let x_6522 = xsys_1016 ((false, 0), (true, i_2970)) in
                             snd (snd x_6522) in
   let x_6540 = xsys_1016 ((true, 0), (true, 0)) in
   if fst (snd (fst x_6540)) = false && fst (snd (snd x_6540)) = false then
     (true, x_2093, x_2094)
   else
     if fst (snd (fst x_6540)) <> false && fst (snd (snd x_6540)) <> false then
       let xs'_1018 (x_1316:int) = let x_6578 = xsys_1016 ((true, x_1316 + 1), (true, 0)) in
                                   snd (fst x_6578) in
       let ys'_1020 (x_1320:int) = let x_6602 = xsys_1016 ((true, 0), (true, x_1320 + 1)) in
                                   snd (snd x_6602) in
       let rec xs'_ys'_4875 (x_4849:int) (x_4850:int) =
         let x_6617 = xsys_1016 ((true, x_4849 + 1), (true, 0)) in
         let x_6632 = xsys_1016 ((true, 0), (true, x_4850 + 1)) in
         (snd (fst x_6617), snd (snd x_6632))
       in
       let x_2141 (ii_2785:((bool * int) * (bool * int))) =
         if fst (fst ii_2785) = false then
           let x_6704 =
             if fst (snd ii_2785) = false then
               (false, (true, ()))
             else
               let x_6710 = ys'_1020 (snd (snd ii_2785)) in
               (true, x_6710)
           in
           ((false, (true, ())), x_6704)
         else
           if fst (snd ii_2785) = false then
             let x_6669 = xs'_1018 (snd (fst ii_2785)) in
             ((true, x_6669), (false, (true, ())))
           else
             let x_6645 = xs'_ys'_4875 (snd (fst ii_2785)) (snd (snd ii_2785)) in
             ((true, fst x_6645), (true, snd x_6645))
       in
       let x_2142 (i_2765:int) = let x_6755 = x_2141 ((true, i_2765), (false, 0)) in
                                 snd (fst x_6755) in
       let x_2143 (i_2758:int) = let x_6774 = x_2141 ((false, 0), (true, i_2758)) in
                                 snd (snd x_6774) in
       let x_6777 = list_eq_1015 x_2141 in
       let x_2150 (ii_2740:((bool * int) * (bool * int))) =
         if fst (fst ii_2740) = false then
           let x_6848 =
             if fst (snd ii_2740) = false then
               (false, (true, ()))
             else
               let x_6856 = (#2 x_6777) (snd (snd ii_2740)) in
               (true, x_6856)
           in
           ((false, (true, ())), x_6848)
         else
           if fst (snd ii_2740) = false then
             let x_6813 = (#1 x_6777) (snd (fst ii_2740)) in
             ((true, x_6813), (false, (true, ())))
           else
             let x_6786 = (#1 x_6777) (snd (fst ii_2740)) in
             let x_6798 = (#2 x_6777) (snd (snd ii_2740)) in
             ((true, x_6786), (true, x_6798))
       in
       let x_2151 (i_2720:int) = let x_6901 = x_2150 ((true, i_2720), (false, 0)) in
                                 snd (fst x_6901) in
       let x_2152 (i_2713:int) = let x_6920 = x_2150 ((false, 0), (true, i_2713)) in
                                 snd (snd x_6920) in
       (snd (snd (fst x_6540)) = snd (snd (snd x_6540)) && #0 x_6777, x_2093, x_2094)
     else
       (false, x_2093, x_2094)
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let x_6964 = make_list_1013 n_1023 in
   let f_1671 (x_1452:int) = (false, 0) in
   let x_2171 (ix_2549:((bool * int) * (bool * int))) =
     if fst (fst ix_2549) = false then
       let x_7034 =
         if fst (snd ix_2549) = false then
           (false, (true, 0))
         else
           let x_7040 = f_1671 (snd (snd ix_2549)) in
           (true, x_7040)
       in
       ((false, (true, 0)), x_7034)
     else
       if fst (snd ix_2549) = false then
         let x_6999 = x_6964 (snd (fst ix_2549)) in
         ((true, x_6999), (false, (true, 0)))
       else
         let x_6976 = x_6964 (snd (fst ix_2549)) in
         let x_6986 = f_1671 (snd (snd ix_2549)) in
         ((true, x_6976), (true, x_6986))
   in
   let x_2172 (i_2529:int) = let x_7085 = x_2171 ((true, i_2529), (false, 0)) in
                             snd (fst x_7085) in
   let x_2173 (x_2522:int) = let x_7104 = x_2171 ((false, 0), (true, x_2522)) in
                             snd (snd x_7104) in
   let x_7107 = append_1085 x_2171 in
   let x_2175 (i_2511:int) = let x_7131 = x_7107 ((true, i_2511), (false, 0), (false, 0)) in
                             snd (#0 x_7131) in
   let x_2176 (i_2501:int) = let x_7157 = x_7107 ((false, 0), (true, i_2501), (false, 0)) in
                             snd (#1 x_7157) in
   let x_2177 (i_2491:int) = let x_7183 = x_7107 ((false, 0), (false, 0), (true, i_2491)) in
                             snd (#2 x_7183) in
   let rec x_x_4927 (x_4889:int) (x_4890:int) =
     let x_7201 = x_7107 ((false, 0), (true, x_4889), (false, 0)) in
     let x_7219 = x_7107 ((false, 0), (false, 0), (true, x_4890)) in
     let x_7408 = x_7107 ((false, 0), (true, x_4889), (true, x_4890)) in
     (snd (#1 x_7408), snd (#2 x_7408))
   in
   let x_2180 (ii_2474:((bool * int) * (bool * int))) =
     if fst (fst ii_2474) = false then
       let x_7290 =
         if fst (snd ii_2474) = false then
           (false, (true, 0))
         else
           let x_7296 = x_2177 (snd (snd ii_2474)) in
           (true, x_7296)
       in
       ((false, (true, 0)), x_7290)
     else
       if fst (snd ii_2474) = false then
         let x_7255 = x_2176 (snd (fst ii_2474)) in
         ((true, x_7255), (false, (true, 0)))
       else
         let x_7231 = x_x_4927 (snd (fst ii_2474)) (snd (snd ii_2474)) in
         ((true, fst x_7231), (true, snd x_7231))
   in
   let x_2181 (i_2454:int) = let x_7341 = x_2180 ((true, i_2454), (false, 0)) in
                             snd (fst x_7341) in
   let x_2182 (i_2447:int) = let x_7360 = x_2180 ((false, 0), (true, i_2447)) in
                             snd (snd x_7360) in
   let x_7384 = x_7107 ((true, i_1022), (false, 0), (false, 0)) in
   let x_7385 = x_6964 i_1022 in
   let n_1696 = if fst (snd (#0 x_7384)) <> false then
                  snd (snd (#0 x_7384))
                else
                  _|_ in
   let n_1697 = if fst x_7385 <> false then
                  snd x_7385
                else
                  _|_ in
   if n_1696 = n_1697 then
     ()
   else
     {fail} ()
 in
 let x_7403 = rand_int () in
 let x_7405 = rand_int () in
 let x_7406 = main_1021 x_7403 in
 let x_7407 = x_7406 x_7405 in
 let x_2198 = x_7407 in
 ()

CPS:
 let rec
   append_1085 (x_1014:(((bool * int) * (bool * int)) -> (((bool * (bool * int)) * (bool * (bool * int))) -> X) -> X))
              (k_append_7470:((((bool * int) * (bool * int) * (bool * int)) ->
                                 (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)
                                -> X)) =
   let x_1998 (i_4297:int) (k_append_x_7477:((bool * int) -> X)) =
     let x_4957 (k_append_x_x_7502:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1014 ((true, i_4297), (false, 0)) k_append_x_x_7502
     in
     x_4957 (fun (x_7508:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7477 (snd (fst x_7508)))
   in
   let x_1999 (i_4290:int) (k_append_x_7518:((bool * int) -> X)) =
     let x_4976 (k_append_x_x_7543:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1014 ((false, 0), (true, i_4290)) k_append_x_x_7543
     in
     x_4976 (fun (x_7549:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_x_7518 (snd (snd x_7549)))
   in
   let rec
     x_x_x_4829 (x_4790:int) (x_4791:int) (x_4792:int)
               (k_append_x_x_x_7559:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
     let x_4990 (k_append_x_x_x_x_7584:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
       x_1014 ((false, 0), (true, x_4790)) k_append_x_x_x_x_7584
     in
     x_4990
       (fun (x_7660:((bool * (bool * int)) * (bool * (bool * int)))) ->
          (let x_5004 (k_append_x_x_x_x_7614:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
             x_1014 ((true, x_4791), (false, 0)) k_append_x_x_x_x_7614
           in
           x_5004
             (fun (x_7659:((bool * (bool * int)) * (bool * (bool * int)))) ->
                (let x_5018 (k_append_x_x_x_x_7644:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                   x_1014 ((false, 0), (true, x_4792)) k_append_x_x_x_x_7644
                 in
                 x_5018
                   (fun (x_7658:((bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_append_x_x_x_7559 (snd (snd x_7660), snd (fst x_7659), snd (snd x_7658)))))))
   in
   let x_5040 (k_append_x_7693:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
     x_1014 ((true, 0), (false, 0)) k_append_x_7693
   in
   x_5040
     (fun (x_9943:((bool * (bool * int)) * (bool * (bool * int)))) ->
        (if fst (snd (fst x_9943)) = false then
           k_append_7470
             (let
                x_2063 (iii_4244:((bool * int) * (bool * int) * (bool * int)))
                      (k_append_x_7703:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                if fst (#0 iii_4244) = false then
                  let x_6256 (k_append_x_x_7738:((bool * (bool * int)) -> X)) =
                    if fst (#1 iii_4244) = false then
                      k_append_x_x_7738 (false, (true, 0))
                    else
                      let x_6262 (k_append_x_x_x_7724:((bool * int) -> X)) =
                        x_1998 (snd (#1 iii_4244)) k_append_x_x_x_7724
                      in
                      x_6262 (fun (x_7736:(bool * int)) -> k_append_x_x_7738 (true, x_7736))
                  in
                  x_6256
                    (fun (x_7799:(bool * (bool * int))) ->
                       (let x_6282 (k_append_x_x_7775:((bool * (bool * int)) -> X)) =
                          if fst (#2 iii_4244) = false then
                            k_append_x_x_7775 (false, (true, 0))
                          else
                            let x_6288 (k_append_x_x_x_7761:((bool * int) -> X)) =
                              x_1999 (snd (#2 iii_4244)) k_append_x_x_x_7761
                            in
                            x_6288 (fun (x_7773:(bool * int)) -> k_append_x_x_7775 (true, x_7773))
                        in
                        x_6282
                          (fun (x_7798:(bool * (bool * int))) -> k_append_x_7703 ((false, (true, 0)), x_7799, x_7798))))
                else
                  if fst (#1 iii_4244) = false then
                    let x_6194 (k_append_x_x_7806:((bool * int) -> X)) = x_1999 (snd (#0 iii_4244)) k_append_x_x_7806 in
                    x_6194
                      (fun (x_7876:(bool * int)) ->
                         (let x_6209 (k_append_x_x_7846:((bool * (bool * int)) -> X)) =
                            if fst (#2 iii_4244) = false then
                              k_append_x_x_7846 (false, (true, 0))
                            else
                              let x_6215 (k_append_x_x_x_7832:((bool * int) -> X)) =
                                x_1999 (snd (#2 iii_4244)) k_append_x_x_x_7832
                              in
                              x_6215 (fun (x_7844:(bool * int)) -> k_append_x_x_7846 (true, x_7844))
                          in
                          x_6209
                            (fun (x_7875:(bool * (bool * int))) ->
                               k_append_x_7703 ((true, x_7876), (false, (true, 0)), x_7875))))
                  else
                    if fst (#2 iii_4244) = false then
                      let x_6153 (k_append_x_x_7883:((bool * int) -> X)) = x_1999 (snd (#0 iii_4244)) k_append_x_x_7883 in
                      x_6153
                        (fun (x_7934:(bool * int)) ->
                           (let x_6163 (k_append_x_x_7895:((bool * int) -> X)) =
                              x_1998 (snd (#1 iii_4244)) k_append_x_x_7895
                            in
                            x_6163
                              (fun (x_7933:(bool * int)) ->
                                 k_append_x_7703 ((true, x_7934), (true, x_7933), (false, (true, 0))))))
                    else
                      let x_6121 (k_append_x_x_7943:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                        x_x_x_4829 (snd (#0 iii_4244)) (snd (#1 iii_4244)) (snd (#2 iii_4244)) k_append_x_x_7943
                      in
                      x_6121
                        (fun (x_7975:((bool * int) * (bool * int) * (bool * int))) ->
                           k_append_x_7703 ((true, #0 x_7975), (true, #1 x_7975), (true, #2 x_7975)))
              in
              x_2063)
         else
           if fst (snd (fst x_9943)) <> false then
             let xs'_1012 (x_1141:int) (k_append_xs'_7997:((bool * int) -> X)) =
               let x_5269 (k_append_xs'_x_8022:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_1014 ((true, x_1141 + 1), (false, 0)) k_append_xs'_x_8022
               in
               x_5269
                 (fun (x_8028:((bool * (bool * int)) * (bool * (bool * int)))) -> k_append_xs'_7997 (snd (fst x_8028)))
             in
             let rec xs'_x_4665 (x_4639:int) (x_4640:int) (k_append_xs'_x_8038:(((bool * int) * (bool * int)) -> X)) =
               let x_5284 (k_append_xs'_x_x_8063:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_1014 ((true, x_4639 + 1), (false, 0)) k_append_xs'_x_x_8063
               in
               x_5284
                 (fun (x_8106:((bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let x_5299 (k_append_xs'_x_x_8093:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       x_1014 ((false, 0), (true, x_4640)) k_append_xs'_x_x_8093
                     in
                     x_5299
                       (fun (x_8105:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_append_xs'_x_8038 (snd (fst x_8106), snd (snd x_8105)))))
             in
             let cons_1216 (x_1212:int) (xs_1213:(int -> ((bool * int) -> X) -> X)) =
               let x_1813 (i_1211:int) (k_append_cons_x_8123:((bool * int) -> X)) =
                 if i_1211 = 0 then
                   k_append_cons_x_8123 (true, x_1212)
                 else
                   let x_5310 (k_append_cons_x_x_8136:((bool * int) -> X)) =
                     xs_1213 (i_1211 - 1) k_append_cons_x_x_8136
                   in
                   x_5310 (fun (x_8161:(bool * int)) -> k_append_cons_x_8123 x_8161)
               in
               let
                 x_2037 (ii_4073:((bool * int) * (bool * int)))
                       (k_append_cons_x_8170:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_4073) = false then
                   let x_5386 (k_append_cons_x_x_8205:((bool * (bool * int)) -> X)) =
                     if fst (snd ii_4073) = false then
                       k_append_cons_x_x_8205 (false, (true, 0))
                     else
                       let x_5392 (k_append_cons_x_x_x_8191:((bool * int) -> X)) =
                         xs_1213 (snd (snd ii_4073)) k_append_cons_x_x_x_8191
                       in
                       x_5392 (fun (x_8203:(bool * int)) -> k_append_cons_x_x_8205 (true, x_8203))
                   in
                   x_5386 (fun (x_8226:(bool * (bool * int))) -> k_append_cons_x_8170 ((false, (true, 0)), x_8226))
                 else
                   if fst (snd ii_4073) = false then
                     let x_5351 (k_append_cons_x_x_8233:((bool * int) -> X)) =
                       x_1813 (snd (fst ii_4073)) k_append_cons_x_x_8233
                     in
                     x_5351 (fun (x_8263:(bool * int)) -> k_append_cons_x_8170 ((true, x_8263), (false, (true, 0))))
                   else
                     let x_5328 (k_append_cons_x_x_8270:((bool * int) -> X)) =
                       x_1813 (snd (fst ii_4073)) k_append_cons_x_x_8270
                     in
                     x_5328
                       (fun (x_8307:(bool * int)) ->
                          (let x_5338 (k_append_cons_x_x_8282:((bool * int) -> X)) =
                             xs_1213 (snd (snd ii_4073)) k_append_cons_x_x_8282
                           in
                           x_5338 (fun (x_8306:(bool * int)) -> k_append_cons_x_8170 ((true, x_8307), (true, x_8306)))))
               in
               x_2037
             in
             let
               x_2040 (ii_3991:((bool * int) * (bool * int)))
                     (k_append_x_8333:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
               if fst (fst ii_3991) = false then
                 let x_5486 (k_append_x_x_8368:((bool * (bool * int)) -> X)) =
                   if fst (snd ii_3991) = false then
                     k_append_x_x_8368 (false, (true, 0))
                   else
                     let x_5492 (k_append_x_x_x_8354:((bool * int) -> X)) =
                       x_1999 (snd (snd ii_3991)) k_append_x_x_x_8354
                     in
                     x_5492 (fun (x_8366:(bool * int)) -> k_append_x_x_8368 (true, x_8366))
                 in
                 x_5486 (fun (x_8389:(bool * (bool * int))) -> k_append_x_8333 ((false, (true, 0)), x_8389))
               else
                 if fst (snd ii_3991) = false then
                   let x_5451 (k_append_x_x_8396:((bool * int) -> X)) = xs'_1012 (snd (fst ii_3991)) k_append_x_x_8396 in
                   x_5451 (fun (x_8426:(bool * int)) -> k_append_x_8333 ((true, x_8426), (false, (true, 0))))
                 else
                   let x_5427 (k_append_x_x_8434:(((bool * int) * (bool * int)) -> X)) =
                     xs'_x_4665 (snd (fst ii_3991)) (snd (snd ii_3991)) k_append_x_x_8434
                   in
                   x_5427
                     (fun (x_8458:((bool * int) * (bool * int))) ->
                        k_append_x_8333 ((true, fst x_8458), (true, snd x_8458)))
             in
             let
               x_5559
                     (k_append_x_8573:((((bool * int) * (bool * int) * (bool * int)) ->
                                          (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)
                                            -> X) -> X)) = append_1085 x_2040 k_append_x_8573
             in
             x_5559
               (fun (x_9565:(((bool * int) * (bool * int) * (bool * int)) ->
                               (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
                  k_append_7470
                    (let x_2044 (i_3953:int) (k_append_x_8595:((bool * int) -> X)) =
                       let
                         x_5583
                               (k_append_x_x_8628:(((bool * (bool * int)) * (
                                                    bool * (bool * int)) * (
                                                    bool * (bool * int))) -> X)) =
                         x_9565 ((true, i_3953), (false, 0), (false, 0)) k_append_x_x_8628
                       in
                       x_5583
                         (fun (x_8634:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8595 (snd (#0 x_8634)))
                     in
                     let x_2045 (i_3943:int) (k_append_x_8639:((bool * int) -> X)) =
                       let
                         x_5609
                               (k_append_x_x_8672:(((bool * (bool * int)) * (
                                                    bool * (bool * int)) * (
                                                    bool * (bool * int))) -> X)) =
                         x_9565 ((false, 0), (true, i_3943), (false, 0)) k_append_x_x_8672
                       in
                       x_5609
                         (fun (x_8678:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8639 (snd (#1 x_8678)))
                     in
                     let x_2046 (i_3933:int) (k_append_x_8683:((bool * int) -> X)) =
                       let
                         x_5635
                               (k_append_x_x_8716:(((bool * (bool * int)) * (
                                                    bool * (bool * int)) * (
                                                    bool * (bool * int))) -> X)) =
                         x_9565 ((false, 0), (false, 0), (true, i_3933)) k_append_x_x_8716
                       in
                       x_5635
                         (fun (x_8722:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_8683 (snd (#2 x_8722)))
                     in
                     let rec
                       x_x_4717 (x_4679:int) (x_4680:int) (k_append_x_x_8728:(((bool * int) * (bool * int)) -> X)) =
                       let
                         x_5653
                               (k_append_x_x_x_8761:(((bool * (bool * int)) * (
                                                      bool * (bool * int)) * (
                                                      bool * (bool * int))) -> X)) =
                         x_9565 ((false, 0), (true, x_4679), (false, 0)) k_append_x_x_x_8761
                       in
                       x_5653
                         (fun (x_8851:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_5671
                                     (k_append_x_x_x_8799:(((bool * (bool * int)) * (
                                                            bool * (bool * int)) * (
                                                            bool * (bool * int))) -> X)) =
                               x_9565 ((false, 0), (false, 0), (true, x_4680)) k_append_x_x_x_8799
                             in
                             x_5671
                               (fun (x_8850:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                                  (let
                                     x_7440
                                           (k_append_x_x_x_8837:(((bool * (bool * int)) * (
                                                                  bool * (bool * int)) * (
                                                                  bool * (bool * int))) -> X)) =
                                     x_9565 ((false, 0), (true, x_4679), (true, x_4680)) k_append_x_x_x_8837
                                   in
                                   x_7440
                                     (fun (x_8849:((bool * (bool * int)) * (
                                                   bool * (bool * int)) * (
                                                   bool * (bool * int)))) ->
                                        k_append_x_x_8728 (snd (#1 x_8849), snd (#2 x_8849)))))))
                     in
                     let
                       x_2049 (ii_3916:((bool * int) * (bool * int)))
                             (k_append_x_8856:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       if fst (fst ii_3916) = false then
                         let x_5742 (k_append_x_x_8891:((bool * (bool * int)) -> X)) =
                           if fst (snd ii_3916) = false then
                             k_append_x_x_8891 (false, (true, 0))
                           else
                             let x_5748 (k_append_x_x_x_8877:((bool * int) -> X)) =
                               x_2046 (snd (snd ii_3916)) k_append_x_x_x_8877
                             in
                             x_5748 (fun (x_8889:(bool * int)) -> k_append_x_x_8891 (true, x_8889))
                         in
                         x_5742 (fun (x_8912:(bool * (bool * int))) -> k_append_x_8856 ((false, (true, 0)), x_8912))
                       else
                         if fst (snd ii_3916) = false then
                           let x_5707 (k_append_x_x_8919:((bool * int) -> X)) =
                             x_2045 (snd (fst ii_3916)) k_append_x_x_8919
                           in
                           x_5707 (fun (x_8949:(bool * int)) -> k_append_x_8856 ((true, x_8949), (false, (true, 0))))
                         else
                           let x_5683 (k_append_x_x_8957:(((bool * int) * (bool * int)) -> X)) =
                             x_x_4717 (snd (fst ii_3916)) (snd (snd ii_3916)) k_append_x_x_8957
                           in
                           x_5683
                             (fun (x_8981:((bool * int) * (bool * int))) ->
                                k_append_x_8856 ((true, fst x_8981), (true, snd x_8981)))
                     in
                     let x_2054 (i_3880:int) (k_append_x_9094:((bool * int) -> X)) =
                       let x_5834 (k_append_x_x_9119:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1216 (snd (snd (fst x_9943)))) x_2044) ((true, i_3880), (false, 0)) k_append_x_x_9119
                       in
                       x_5834
                         (fun (x_9125:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            k_append_x_9094 (snd (fst x_9125)))
                     in
                     let rec
                       x_x_x_4770 (x_4731:int) (x_4732:int) (x_4733:int)
                                 (k_append_x_x_x_9132:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                       let x_5848 (k_append_x_x_x_x_9157:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                         ((cons_1216 (snd (snd (fst x_9943)))) x_2044)
                           ((true, x_4731), (false, 0)) k_append_x_x_x_x_9157
                       in
                       x_5848
                         (fun (x_9233:((bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_5862 (k_append_x_x_x_x_9187:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                               x_1014 ((true, x_4732), (false, 0)) k_append_x_x_x_x_9187
                             in
                             x_5862
                               (fun (x_9232:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                  (let
                                     x_5876
                                           (k_append_x_x_x_x_9217:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                                     x_1014 ((false, 0), (true, x_4733)) k_append_x_x_x_x_9217
                                   in
                                   x_5876
                                     (fun (x_9231:((bool * (bool * int)) * (bool * (bool * int)))) ->
                                        k_append_x_x_x_9132 (snd (fst x_9233), snd (fst x_9232), snd (snd x_9231)))))))
                     in
                     let
                       x_2059 (iii_3848:((bool * int) * (bool * int) * (bool * int)))
                             (k_append_x_9274:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_3848) = false then
                         let x_6046 (k_append_x_x_9309:((bool * (bool * int)) -> X)) =
                           if fst (#1 iii_3848) = false then
                             k_append_x_x_9309 (false, (true, 0))
                           else
                             let x_6052 (k_append_x_x_x_9295:((bool * int) -> X)) =
                               x_1998 (snd (#1 iii_3848)) k_append_x_x_x_9295
                             in
                             x_6052 (fun (x_9307:(bool * int)) -> k_append_x_x_9309 (true, x_9307))
                         in
                         x_6046
                           (fun (x_9370:(bool * (bool * int))) ->
                              (let x_6072 (k_append_x_x_9346:((bool * (bool * int)) -> X)) =
                                 if fst (#2 iii_3848) = false then
                                   k_append_x_x_9346 (false, (true, 0))
                                 else
                                   let x_6078 (k_append_x_x_x_9332:((bool * int) -> X)) =
                                     x_1999 (snd (#2 iii_3848)) k_append_x_x_x_9332
                                   in
                                   x_6078 (fun (x_9344:(bool * int)) -> k_append_x_x_9346 (true, x_9344))
                               in
                               x_6072
                                 (fun (x_9369:(bool * (bool * int))) ->
                                    k_append_x_9274 ((false, (true, 0)), x_9370, x_9369))))
                       else
                         if fst (#1 iii_3848) = false then
                           let x_5984 (k_append_x_x_9377:((bool * int) -> X)) =
                             x_2054 (snd (#0 iii_3848)) k_append_x_x_9377
                           in
                           x_5984
                             (fun (x_9447:(bool * int)) ->
                                (let x_5999 (k_append_x_x_9417:((bool * (bool * int)) -> X)) =
                                   if fst (#2 iii_3848) = false then
                                     k_append_x_x_9417 (false, (true, 0))
                                   else
                                     let x_6005 (k_append_x_x_x_9403:((bool * int) -> X)) =
                                       x_1999 (snd (#2 iii_3848)) k_append_x_x_x_9403
                                     in
                                     x_6005 (fun (x_9415:(bool * int)) -> k_append_x_x_9417 (true, x_9415))
                                 in
                                 x_5999
                                   (fun (x_9446:(bool * (bool * int))) ->
                                      k_append_x_9274 ((true, x_9447), (false, (true, 0)), x_9446))))
                         else
                           if fst (#2 iii_3848) = false then
                             let x_5943 (k_append_x_x_9454:((bool * int) -> X)) =
                               x_2054 (snd (#0 iii_3848)) k_append_x_x_9454
                             in
                             x_5943
                               (fun (x_9505:(bool * int)) ->
                                  (let x_5953 (k_append_x_x_9466:((bool * int) -> X)) =
                                     x_1998 (snd (#1 iii_3848)) k_append_x_x_9466
                                   in
                                   x_5953
                                     (fun (x_9504:(bool * int)) ->
                                        k_append_x_9274 ((true, x_9505), (true, x_9504), (false, (true, 0))))))
                           else
                             let x_5911 (k_append_x_x_9514:(((bool * int) * (bool * int) * (bool * int)) -> X)) =
                               x_x_x_4770 (snd (#0 iii_3848)) (snd (#1 iii_3848)) (snd (#2 iii_3848)) k_append_x_x_9514
                             in
                             x_5911
                               (fun (x_9546:((bool * int) * (bool * int) * (bool * int))) ->
                                  k_append_x_9274 ((true, #0 x_9546), (true, #1 x_9546), (true, #2 x_9546)))
                     in
                     x_2059))
           else
             let x_1798 (k_append_x_9605:((int -> ((bool * int) -> X) -> X) -> X)) = _|_ in
             x_1798
               (fun (x_9928:(int -> ((bool * int) -> X) -> X)) ->
                  k_append_7470
                    (let
                       x_2013 (iii_3419:((bool * int) * (bool * int) * (bool * int)))
                             (k_append_x_9613:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                 -> X)) =
                       if fst (#0 iii_3419) = false then
                         let x_5190 (k_append_x_x_9648:((bool * (bool * int)) -> X)) =
                           if fst (#1 iii_3419) = false then
                             k_append_x_x_9648 (false, (true, 0))
                           else
                             let x_5196 (k_append_x_x_x_9634:((bool * int) -> X)) =
                               x_1998 (snd (#1 iii_3419)) k_append_x_x_x_9634
                             in
                             x_5196 (fun (x_9646:(bool * int)) -> k_append_x_x_9648 (true, x_9646))
                         in
                         x_5190
                           (fun (x_9709:(bool * (bool * int))) ->
                              (let x_5216 (k_append_x_x_9685:((bool * (bool * int)) -> X)) =
                                 if fst (#2 iii_3419) = false then
                                   k_append_x_x_9685 (false, (true, 0))
                                 else
                                   let x_5222 (k_append_x_x_x_9671:((bool * int) -> X)) =
                                     x_1999 (snd (#2 iii_3419)) k_append_x_x_x_9671
                                   in
                                   x_5222 (fun (x_9683:(bool * int)) -> k_append_x_x_9685 (true, x_9683))
                               in
                               x_5216
                                 (fun (x_9708:(bool * (bool * int))) ->
                                    k_append_x_9613 ((false, (true, 0)), x_9709, x_9708))))
                       else
                         if fst (#1 iii_3419) = false then
                           let x_5128 (k_append_x_x_9716:((bool * int) -> X)) =
                             x_9928 (snd (#0 iii_3419)) k_append_x_x_9716
                           in
                           x_5128
                             (fun (x_9786:(bool * int)) ->
                                (let x_5143 (k_append_x_x_9756:((bool * (bool * int)) -> X)) =
                                   if fst (#2 iii_3419) = false then
                                     k_append_x_x_9756 (false, (true, 0))
                                   else
                                     let x_5149 (k_append_x_x_x_9742:((bool * int) -> X)) =
                                       x_1999 (snd (#2 iii_3419)) k_append_x_x_x_9742
                                     in
                                     x_5149 (fun (x_9754:(bool * int)) -> k_append_x_x_9756 (true, x_9754))
                                 in
                                 x_5143
                                   (fun (x_9785:(bool * (bool * int))) ->
                                      k_append_x_9613 ((true, x_9786), (false, (true, 0)), x_9785))))
                         else
                           if fst (#2 iii_3419) = false then
                             let x_5087 (k_append_x_x_9793:((bool * int) -> X)) =
                               x_9928 (snd (#0 iii_3419)) k_append_x_x_9793
                             in
                             x_5087
                               (fun (x_9844:(bool * int)) ->
                                  (let x_5097 (k_append_x_x_9805:((bool * int) -> X)) =
                                     x_1998 (snd (#1 iii_3419)) k_append_x_x_9805
                                   in
                                   x_5097
                                     (fun (x_9843:(bool * int)) ->
                                        k_append_x_9613 ((true, x_9844), (true, x_9843), (false, (true, 0))))))
                           else
                             let x_5053 (k_append_x_x_9851:((bool * int) -> X)) =
                               x_9928 (snd (#0 iii_3419)) k_append_x_x_9851
                             in
                             x_5053
                               (fun (x_9909:(bool * int)) ->
                                  (let x_5063 (k_append_x_x_9863:((bool * int) -> X)) =
                                     x_1998 (snd (#1 iii_3419)) k_append_x_x_9863
                                   in
                                   x_5063
                                     (fun (x_9908:(bool * int)) ->
                                        (let x_5073 (k_append_x_x_9875:((bool * int) -> X)) =
                                           x_1999 (snd (#2 iii_3419)) k_append_x_x_9875
                                         in
                                         x_5073
                                           (fun (x_9907:(bool * int)) ->
                                              k_append_x_9613 ((true, x_9909), (true, x_9908), (true, x_9907)))))))
                     in
                     x_2013))))
 in
 let rec make_list_1013 (n_1014:int) (k_make_list_9973:((int -> ((bool * int) -> X) -> X) -> X)) =
   if n_1014 < 0 then
     k_make_list_9973 (fun (x_1305:int) -> fun (k_make_list_9975:((bool * int) -> X)) -> k_make_list_9975 (false, 0))
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> ((bool * int) -> X) -> X)) =
       let x_1853 (i_1294:int) (k_make_list_cons_x_9994:((bool * int) -> X)) =
         if i_1294 = 0 then
           k_make_list_cons_x_9994 (true, x_1295)
         else
           let x_6324 (k_make_list_cons_x_x_10007:((bool * int) -> X)) =
             xs_1296 (i_1294 - 1) k_make_list_cons_x_x_10007
           in
           x_6324 (fun (x_10032:(bool * int)) -> k_make_list_cons_x_9994 x_10032)
       in
       let
         x_2079 (ii_3171:((bool * int) * (bool * int)))
               (k_make_list_cons_x_10041:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
         if fst (fst ii_3171) = false then
           let x_6400 (k_make_list_cons_x_x_10076:((bool * (bool * int)) -> X)) =
             if fst (snd ii_3171) = false then
               k_make_list_cons_x_x_10076 (false, (true, 0))
             else
               let x_6406 (k_make_list_cons_x_x_x_10062:((bool * int) -> X)) =
                 xs_1296 (snd (snd ii_3171)) k_make_list_cons_x_x_x_10062
               in
               x_6406 (fun (x_10074:(bool * int)) -> k_make_list_cons_x_x_10076 (true, x_10074))
           in
           x_6400 (fun (x_10097:(bool * (bool * int))) -> k_make_list_cons_x_10041 ((false, (true, 0)), x_10097))
         else
           if fst (snd ii_3171) = false then
             let x_6365 (k_make_list_cons_x_x_10104:((bool * int) -> X)) =
               x_1853 (snd (fst ii_3171)) k_make_list_cons_x_x_10104
             in
             x_6365 (fun (x_10134:(bool * int)) -> k_make_list_cons_x_10041 ((true, x_10134), (false, (true, 0))))
           else
             let x_6342 (k_make_list_cons_x_x_10141:((bool * int) -> X)) =
               x_1853 (snd (fst ii_3171)) k_make_list_cons_x_x_10141
             in
             x_6342
               (fun (x_10178:(bool * int)) ->
                  (let x_6352 (k_make_list_cons_x_x_10153:((bool * int) -> X)) =
                     xs_1296 (snd (snd ii_3171)) k_make_list_cons_x_x_10153
                   in
                   x_6352 (fun (x_10177:(bool * int)) -> k_make_list_cons_x_10041 ((true, x_10178), (true, x_10177)))))
       in
       x_2079
     in
     let x_6437 (k_make_list_x_10213:((int -> ((bool * int) -> X) -> X) -> X)) =
       make_list_1013 (n_1014 - 1) k_make_list_x_10213
     in
     x_6437
       (fun (x_10347:(int -> ((bool * int) -> X) -> X)) ->
          (let x_6439 (k_make_list_x_10234:(int -> X)) = rand_int_cps () k_make_list_x_10234 in
           x_6439
             (fun (x_10343:int) ->
                k_make_list_9973
                  (let x_2086 (i_3095:int) (k_make_list_x_10272:((bool * int) -> X)) =
                     let x_6458 (k_make_list_x_x_10297:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                       ((cons_1299 x_10343) x_10347) ((true, i_3095), (false, 0)) k_make_list_x_x_10297
                     in
                     x_6458
                       (fun (x_10303:((bool * (bool * int)) * (bool * (bool * int)))) ->
                          k_make_list_x_10272 (snd (fst x_10303)))
                   in
                   x_2086))))
 in
 let rec
   list_eq_1015
               (xsys_1016:(((bool * int) * (bool * int)) ->
                             (((bool * (bool * unit)) * (bool * (bool * unit))) -> X) -> X))
               (k_list_eq_10382:((bool * (int -> ((bool * unit) -> X) -> X) * (int -> ((bool * unit) -> X) -> X)) -> X)) =
   let x_2093 (i_2977:int) (k_list_eq_x_10392:((bool * unit) -> X)) =
     let x_6503 (k_list_eq_x_x_10417:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
       xsys_1016 ((true, i_2977), (false, 0)) k_list_eq_x_x_10417
     in
     x_6503 (fun (x_10423:((bool * (bool * unit)) * (bool * (bool * unit)))) -> k_list_eq_x_10392 (snd (fst x_10423)))
   in
   let x_2094 (i_2970:int) (k_list_eq_x_10436:((bool * unit) -> X)) =
     let x_6522 (k_list_eq_x_x_10461:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
       xsys_1016 ((false, 0), (true, i_2970)) k_list_eq_x_x_10461
     in
     x_6522 (fun (x_10467:((bool * (bool * unit)) * (bool * (bool * unit)))) -> k_list_eq_x_10436 (snd (snd x_10467)))
   in
   let x_6540 (k_list_eq_x_10501:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
     xsys_1016 ((true, 0), (true, 0)) k_list_eq_x_10501
   in
   x_6540
     (fun (x_11383:((bool * (bool * unit)) * (bool * (bool * unit)))) ->
        (let k_list_eq_10509 (b_12930:bool) =
           if b_12930 then
             k_list_eq_10382 (true, x_2093, x_2094)
           else
             let k_list_eq_13265 (b_13271:bool) =
               if b_13271 then
                 let xs'_13277 (x_13278:int) (k_list_eq_xs'_13279:((bool * unit) -> X)) =
                   let x_13281 (k_list_eq_xs'_x_13282:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                     xsys_1016 ((true, x_13278 + 1), (true, 0)) k_list_eq_xs'_x_13282
                   in
                   x_13281
                     (fun (x_13294:((bool * (bool * unit)) * (bool * (bool * unit)))) ->
                        k_list_eq_xs'_13279 (snd (fst x_13294)))
                 in
                 let ys'_13297 (x_13298:int) (k_list_eq_ys'_13299:((bool * unit) -> X)) =
                   let x_13301 (k_list_eq_ys'_x_13302:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                     xsys_1016 ((true, 0), (true, x_13298 + 1)) k_list_eq_ys'_x_13302
                   in
                   x_13301
                     (fun (x_13314:((bool * (bool * unit)) * (bool * (bool * unit)))) ->
                        k_list_eq_ys'_13299 (snd (snd x_13314)))
                 in
                 let rec
                   xs'_ys'_13317 (x_13318:int) (x_13319:int)
                                (k_list_eq_xs'_ys'_13320:(((bool * unit) * (bool * unit)) -> X)) =
                   let x_13322 (k_list_eq_xs'_ys'_x_13323:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                     xsys_1016 ((true, x_13318 + 1), (true, 0)) k_list_eq_xs'_ys'_x_13323
                   in
                   x_13322
                     (fun (x_13335:((bool * (bool * unit)) * (bool * (bool * unit)))) ->
                        (let
                           x_13337 (k_list_eq_xs'_ys'_x_13338:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                           xsys_1016 ((true, 0), (true, x_13319 + 1)) k_list_eq_xs'_ys'_x_13338
                         in
                         x_13337
                           (fun (x_13350:((bool * (bool * unit)) * (bool * (bool * unit)))) ->
                              k_list_eq_xs'_ys'_13320 (snd (fst x_13335), snd (snd x_13350)))))
                 in
                 let
                   x_13356 (ii_13357:((bool * int) * (bool * int)))
                          (k_list_eq_x_13358:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                   if fst (fst ii_13357) = false then
                     let x_13399 (k_list_eq_x_x_13400:((bool * (bool * unit)) -> X)) =
                       if fst (snd ii_13357) = false then
                         k_list_eq_x_x_13400 (false, (true, ()))
                       else
                         let x_13404 (k_list_eq_x_x_x_13405:((bool * unit) -> X)) =
                           ys'_13297 (snd (snd ii_13357)) k_list_eq_x_x_x_13405
                         in
                         x_13404 (fun (x_13408:(bool * unit)) -> k_list_eq_x_x_13400 (true, x_13408))
                     in
                     x_13399 (fun (x_13418:(bool * (bool * unit))) -> k_list_eq_x_13358 ((false, (true, ())), x_13418))
                   else
                     if fst (snd ii_13357) = false then
                       let x_13381 (k_list_eq_x_x_13382:((bool * unit) -> X)) =
                         xs'_13277 (snd (fst ii_13357)) k_list_eq_x_x_13382
                       in
                       x_13381
                         (fun (x_13385:(bool * unit)) -> k_list_eq_x_13358 ((true, x_13385), (false, (true, ()))))
                     else
                       let x_13366 (k_list_eq_x_x_13367:(((bool * unit) * (bool * unit)) -> X)) =
                         xs'_ys'_13317 (snd (fst ii_13357)) (snd (snd ii_13357)) k_list_eq_x_x_13367
                       in
                       x_13366
                         (fun (x_13370:((bool * unit) * (bool * unit))) ->
                            k_list_eq_x_13358 ((true, fst x_13370), (true, snd x_13370)))
                 in
                 let
                   x_13470
                          (k_list_eq_x_13471:((bool * (int -> ((bool * unit) -> X) -> X) *
                                               (int -> ((bool * unit) -> X) -> X)) -> X)) =
                   list_eq_1015 x_13356 k_list_eq_x_13471
                 in
                 x_13470
                   (fun (x_13474:(bool * (int -> ((bool * unit) -> X) -> X) * (int -> ((bool * unit) -> X) -> X))) ->
                      (let
                         x_13476 (ii_13477:((bool * int) * (bool * int)))
                                (k_list_eq_x_13478:(((bool * (bool * unit)) * (bool * (bool * unit))) -> X)) =
                         if fst (fst ii_13477) = false then
                           let x_13525 (k_list_eq_x_x_13526:((bool * (bool * unit)) -> X)) =
                             if fst (snd ii_13477) = false then
                               k_list_eq_x_x_13526 (false, (true, ()))
                             else
                               let x_13530 (k_list_eq_x_x_x_13531:((bool * unit) -> X)) =
                                 (#2 x_13474) (snd (snd ii_13477)) k_list_eq_x_x_x_13531
                               in
                               x_13530 (fun (x_13534:(bool * unit)) -> k_list_eq_x_x_13526 (true, x_13534))
                           in
                           x_13525
                             (fun (x_13544:(bool * (bool * unit))) -> k_list_eq_x_13478 ((false, (true, ())), x_13544))
                         else
                           if fst (snd ii_13477) = false then
                             let x_13507 (k_list_eq_x_x_13508:((bool * unit) -> X)) =
                               (#1 x_13474) (snd (fst ii_13477)) k_list_eq_x_x_13508
                             in
                             x_13507
                               (fun (x_13511:(bool * unit)) ->
                                  k_list_eq_x_13478 ((true, x_13511), (false, (true, ()))))
                           else
                             let x_13486 (k_list_eq_x_x_13487:((bool * unit) -> X)) =
                               (#1 x_13474) (snd (fst ii_13477)) k_list_eq_x_x_13487
                             in
                             x_13486
                               (fun (x_13490:(bool * unit)) ->
                                  (let x_13492 (k_list_eq_x_x_13493:((bool * unit) -> X)) =
                                     (#2 x_13474) (snd (snd ii_13477)) k_list_eq_x_x_13493
                                   in
                                   x_13492
                                     (fun (x_13496:(bool * unit)) ->
                                        k_list_eq_x_13478 ((true, x_13490), (true, x_13496)))))
                       in
                       let k_list_eq_13600 (x_13704:bool) = k_list_eq_10382 (x_13704, x_2093, x_2094) in
                       if snd (snd (fst x_11383)) = snd (snd (snd x_11383)) then
                         k_list_eq_13600 (#0 x_13474)
                       else
                         k_list_eq_13600 false))
               else
                 k_list_eq_10382 (false, x_2093, x_2094)
             in
             if fst (snd (fst x_11383)) <> false then
               k_list_eq_13265 (fst (snd (snd x_11383)) <> false)
             else
               k_list_eq_13265 false
         in
         if fst (snd (fst x_11383)) = false then
           k_list_eq_10509 (fst (snd (snd x_11383)) = false)
         else
           k_list_eq_10509 false))
 in
 let main_1021 (i_1022:int) (n_1023:int) (k_main_11407:(unit -> X)) =
   let x_6964 (k_main_x_11420:((int -> ((bool * int) -> X) -> X) -> X)) = make_list_1013 n_1023 k_main_x_11420 in
   x_6964
     (fun (x_12327:(int -> ((bool * int) -> X) -> X)) ->
        (let f_1671 (x_1452:int) (k_main_f_11435:((bool * int) -> X)) = k_main_f_11435 (false, 0) in
         let
           x_2171 (ix_2549:((bool * int) * (bool * int)))
                 (k_main_x_11448:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
           if fst (fst ix_2549) = false then
             let x_7034 (k_main_x_x_11483:((bool * (bool * int)) -> X)) =
               if fst (snd ix_2549) = false then
                 k_main_x_x_11483 (false, (true, 0))
               else
                 let x_7040 (k_main_x_x_x_11469:((bool * int) -> X)) = f_1671 (snd (snd ix_2549)) k_main_x_x_x_11469 in
                 x_7040 (fun (x_11481:(bool * int)) -> k_main_x_x_11483 (true, x_11481))
             in
             x_7034 (fun (x_11504:(bool * (bool * int))) -> k_main_x_11448 ((false, (true, 0)), x_11504))
           else
             if fst (snd ix_2549) = false then
               let x_6999 (k_main_x_x_11511:((bool * int) -> X)) = x_12327 (snd (fst ix_2549)) k_main_x_x_11511 in
               x_6999 (fun (x_11541:(bool * int)) -> k_main_x_11448 ((true, x_11541), (false, (true, 0))))
             else
               let x_6976 (k_main_x_x_11548:((bool * int) -> X)) = x_12327 (snd (fst ix_2549)) k_main_x_x_11548 in
               x_6976
                 (fun (x_11585:(bool * int)) ->
                    (let x_6986 (k_main_x_x_11560:((bool * int) -> X)) = f_1671 (snd (snd ix_2549)) k_main_x_x_11560 in
                     x_6986 (fun (x_11584:(bool * int)) -> k_main_x_11448 ((true, x_11585), (true, x_11584)))))
         in
         let
           x_7107
                 (k_main_x_11691:((((bool * int) * (bool * int) * (bool * int)) ->
                                     (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) ->
                                       X) -> X)) = append_1085 x_2171 k_main_x_11691
         in
         x_7107
           (fun (x_12307:(((bool * int) * (bool * int) * (bool * int)) ->
                            (((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X) -> X)) ->
              (let x_2176 (i_2501:int) (k_main_x_11758:((bool * int) -> X)) =
                 let
                   x_7157
                         (k_main_x_x_11791:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) ->
                                              X)) = x_12307 ((false, 0), (true, i_2501), (false, 0)) k_main_x_x_11791
                 in
                 x_7157
                   (fun (x_11797:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_11758 (snd (#1 x_11797)))
               in
               let x_2177 (i_2491:int) (k_main_x_11804:((bool * int) -> X)) =
                 let
                   x_7183
                         (k_main_x_x_11837:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) ->
                                              X)) = x_12307 ((false, 0), (false, 0), (true, i_2491)) k_main_x_x_11837
                 in
                 x_7183
                   (fun (x_11843:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      k_main_x_11804 (snd (#2 x_11843)))
               in
               let rec x_x_4927 (x_4889:int) (x_4890:int) (k_main_x_x_11850:(((bool * int) * (bool * int)) -> X)) =
                 let
                   x_7201
                         (k_main_x_x_x_11883:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))
                                                -> X)) =
                   x_12307 ((false, 0), (true, x_4889), (false, 0)) k_main_x_x_x_11883
                 in
                 x_7201
                   (fun (x_11973:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                      (let
                         x_7219
                               (k_main_x_x_x_11921:(((bool * (bool * int)) * (
                                                     bool * (bool * int)) * (
                                                     bool * (bool * int))) -> X)) =
                         x_12307 ((false, 0), (false, 0), (true, x_4890)) k_main_x_x_x_11921
                       in
                       x_7219
                         (fun (x_11972:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                            (let
                               x_7408
                                     (k_main_x_x_x_11959:(((bool * (bool * int)) * (
                                                           bool * (bool * int)) * (
                                                           bool * (bool * int))) -> X)) =
                               x_12307 ((false, 0), (true, x_4889), (true, x_4890)) k_main_x_x_x_11959
                             in
                             x_7408
                               (fun (x_11971:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                                  k_main_x_x_11850 (snd (#1 x_11971), snd (#2 x_11971)))))))
               in
               let
                 x_2180 (ii_2474:((bool * int) * (bool * int)))
                       (k_main_x_11981:(((bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 if fst (fst ii_2474) = false then
                   let x_7290 (k_main_x_x_12016:((bool * (bool * int)) -> X)) =
                     if fst (snd ii_2474) = false then
                       k_main_x_x_12016 (false, (true, 0))
                     else
                       let x_7296 (k_main_x_x_x_12002:((bool * int) -> X)) =
                         x_2177 (snd (snd ii_2474)) k_main_x_x_x_12002
                       in
                       x_7296 (fun (x_12014:(bool * int)) -> k_main_x_x_12016 (true, x_12014))
                   in
                   x_7290 (fun (x_12037:(bool * (bool * int))) -> k_main_x_11981 ((false, (true, 0)), x_12037))
                 else
                   if fst (snd ii_2474) = false then
                     let x_7255 (k_main_x_x_12044:((bool * int) -> X)) = x_2176 (snd (fst ii_2474)) k_main_x_x_12044 in
                     x_7255 (fun (x_12074:(bool * int)) -> k_main_x_11981 ((true, x_12074), (false, (true, 0))))
                   else
                     let x_7231 (k_main_x_x_12082:(((bool * int) * (bool * int)) -> X)) =
                       x_x_4927 (snd (fst ii_2474)) (snd (snd ii_2474)) k_main_x_x_12082
                     in
                     x_7231
                       (fun (x_12106:((bool * int) * (bool * int))) ->
                          k_main_x_11981 ((true, fst x_12106), (true, snd x_12106)))
               in
               let
                 x_7384 (k_main_x_12226:(((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int))) -> X)) =
                 x_12307 ((true, i_1022), (false, 0), (false, 0)) k_main_x_12226
               in
               x_7384
                 (fun (x_12274:((bool * (bool * int)) * (bool * (bool * int)) * (bool * (bool * int)))) ->
                    (let x_7385 (k_main_x_12238:((bool * int) -> X)) = x_12327 i_1022 k_main_x_12238 in
                     x_7385
                       (fun (x_12273:(bool * int)) ->
                          (let n_1696 (k_main_n_12249:(int -> X)) =
                             if fst (snd (#0 x_12274)) <> false then
                               k_main_n_12249 (snd (snd (#0 x_12274)))
                             else
                               _|_
                           in
                           n_1696
                             (fun (n_12272:int) ->
                                (let n_1697 (k_main_n_12257:(int -> X)) =
                                   if fst x_12273 <> false then
                                     k_main_n_12257 (snd x_12273)
                                   else
                                     _|_
                                 in
                                 n_1697
                                   (fun (n_12271:int) ->
                                      (if n_12272 = n_12271 then
                                         k_main_11407 ()
                                       else
                                         {|fail|} () k_main_11407))))))))))))
 in
 let x_7403 (k_x_12338:(int -> X)) = rand_int_cps () k_x_12338 in
 x_7403
   (fun (x_12383:int) ->
      (let x_7405 (k_x_12350:(int -> X)) = rand_int_cps () k_x_12350 in
       x_7405
         (fun (x_12382:int) ->
            (let x_7407 (k_x_12371:(unit -> X)) = (main_1021 x_12383) x_12382 k_x_12371 in
             x_7407 (fun (x_12377:unit) -> {end})))))

remove_pair:
 let rec
   append_1085
              (x_1014:(bool ->
                         int ->
                           bool ->
                             int ->
                               (bool ->
                                  bool ->
                                    r011_7464:int ->
                                      bool -> bool -> r111_7464:int[\r111_7464. r011_7464 = r111_7464] -> X) -> X))
              (k_append_7470:((bool ->
                                 int ->
                                   bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           (bool ->
                                              bool ->
                                                r011_7467:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_7467:int[\r111_7467. r011_7467 = r111_7467] ->
                                                        bool -> bool -> int -> X) -> X) -> X)) =
   let x_1998 (i_4297:int) (k_append_x_7477:(bool -> int -> X)) =
     let
       x_4957
             (k_append_x_x_7502:(bool ->
                                   bool ->
                                     r011_7501:int ->
                                       bool -> bool -> r111_7501:int[\r111_7501. r011_7501 = r111_7501] -> X)) =
       x_1014 true i_4297 false 0 k_append_x_x_7502
     in
     x_4957
       (fun (x00_7508:bool) ->
          fun (x010_7508:bool) ->
            fun (x011_7508:int) ->
              fun (x10_7508:bool) -> fun (x110_7508:bool) -> fun (x111_7508:int) -> k_append_x_7477 x010_7508 x011_7508)
   in
   let x_1999 (i_4290:int) (k_append_x_7518:(bool -> int -> X)) =
     let
       x_4976
             (k_append_x_x_7543:(bool ->
                                   bool ->
                                     r011_7542:int ->
                                       bool -> bool -> r111_7542:int[\r111_7542. r011_7542 = r111_7542] -> X)) =
       x_1014 false 0 true i_4290 k_append_x_x_7543
     in
     x_4976
       (fun (x00_7549:bool) ->
          fun (x010_7549:bool) ->
            fun (x011_7549:int) ->
              fun (x10_7549:bool) -> fun (x110_7549:bool) -> fun (x111_7549:int) -> k_append_x_7518 x110_7549 x111_7549)
   in
   let rec
     x_x_x_4829 (x_4790:int) (x_4791:int) (x_4792:int)
               (k_append_x_x_x_7559:(bool -> int -> bool -> int -> bool -> int -> X)) =
     let
       x_4990
             (k_append_x_x_x_x_7584:(bool ->
                                       bool ->
                                         r011_7583:int ->
                                           bool -> bool -> r111_7583:int[\r111_7583. r011_7583 = r111_7583] -> X)) =
       x_1014 false 0 true x_4790 k_append_x_x_x_x_7584
     in
     x_4990
       (fun (x00_7660:bool) ->
          fun (x010_7660:bool) ->
            fun (x011_7660:int) ->
              fun (x10_7660:bool) ->
                fun (x110_7660:bool) ->
                  fun (x111_7660:int) ->
                    (let
                       x_5004
                             (k_append_x_x_x_x_7614:(bool ->
                                                       bool ->
                                                         r011_7613:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_7613:int[\r111_7613. r011_7613 = r111_7613] -> X)) =
                       x_1014 true x_4791 false 0 k_append_x_x_x_x_7614
                     in
                     x_5004
                       (fun (x00_7659:bool) ->
                          fun (x010_7659:bool) ->
                            fun (x011_7659:int) ->
                              fun (x10_7659:bool) ->
                                fun (x110_7659:bool) ->
                                  fun (x111_7659:int) ->
                                    (let
                                       x_5018
                                             (k_append_x_x_x_x_7644:(
                                             bool ->
                                               bool ->
                                                 r011_7643:int ->
                                                   bool ->
                                                     bool -> r111_7643:int[\r111_7643. r011_7643 = r111_7643] -> X)) =
                                       x_1014 false 0 true x_4792 k_append_x_x_x_x_7644
                                     in
                                     x_5018
                                       (fun (x00_7658:bool) ->
                                          fun (x010_7658:bool) ->
                                            fun (x011_7658:int) ->
                                              fun (x10_7658:bool) ->
                                                fun (x110_7658:bool) ->
                                                  fun (x111_7658:int) ->
                                                    k_append_x_x_x_7559 x110_7660 x111_7660 x010_7659 x011_7659
                                                      x110_7658 x111_7658)))))
   in
   let
     x_5040
           (k_append_x_7693:(bool ->
                               bool ->
                                 r011_7692:int -> bool -> bool -> r111_7692:int[\r111_7692. r011_7692 = r111_7692] -> X)) =
     x_1014 true 0 false 0 k_append_x_7693
   in
   x_5040
     (fun (x00_9943:bool) ->
        fun (x010_9943:bool) ->
          fun (x011_9943:int) ->
            fun (x10_9943:bool) ->
              fun (x110_9943:bool) ->
                fun (x111_9943:int) ->
                  (if x010_9943 = false then
                     k_append_7470
                       (let
                          x_2063 (iii00_4244:bool) (iii01_4244:int) (iii10_4244:bool) (iii11_4244:int)
                                (iii20_4244:bool) (iii21_4244:int)
                                (k_append_x_7703:(bool ->
                                                    bool ->
                                                      r011_7700:int ->
                                                        bool ->
                                                          bool ->
                                                            r111_7700:
                                                              int[\r111_7700. r011_7700 = r111_7700] ->
                                                              bool -> bool -> int -> X)) =
                          if iii00_4244 = false then
                            let x_6256 (k_append_x_x_7738:(bool -> bool -> int -> X)) =
                              if iii10_4244 = false then
                                k_append_x_x_7738 false true 0
                              else
                                let x_6262 (k_append_x_x_x_7724:(bool -> int -> X)) =
                                  x_1998 iii11_4244 k_append_x_x_x_7724
                                in
                                x_6262
                                  (fun (x0_7736:bool) -> fun (x1_7736:int) -> k_append_x_x_7738 true x0_7736 x1_7736)
                            in
                            x_6256
                              (fun (x0_7799:bool) ->
                                 fun (x10_7799:bool) ->
                                   fun (x11_7799:int) ->
                                     (let x_6282 (k_append_x_x_7775:(bool -> bool -> int -> X)) =
                                        if iii20_4244 = false then
                                          k_append_x_x_7775 false true 0
                                        else
                                          let x_6288 (k_append_x_x_x_7761:(bool -> int -> X)) =
                                            x_1999 iii21_4244 k_append_x_x_x_7761
                                          in
                                          x_6288
                                            (fun (x0_7773:bool) ->
                                               fun (x1_7773:int) -> k_append_x_x_7775 true x0_7773 x1_7773)
                                      in
                                      x_6282
                                        (fun (x0_7798:bool) ->
                                           fun (x10_7798:bool) ->
                                             fun (x11_7798:int) ->
                                               k_append_x_7703 false true 0 x0_7799 x10_7799 x11_7799 x0_7798 x10_7798
                                                 x11_7798)))
                          else
                            if iii10_4244 = false then
                              let x_6194 (k_append_x_x_7806:(bool -> int -> X)) = x_1999 iii01_4244 k_append_x_x_7806 in
                              x_6194
                                (fun (x0_7876:bool) ->
                                   fun (x1_7876:int) ->
                                     (let x_6209 (k_append_x_x_7846:(bool -> bool -> int -> X)) =
                                        if iii20_4244 = false then
                                          k_append_x_x_7846 false true 0
                                        else
                                          let x_6215 (k_append_x_x_x_7832:(bool -> int -> X)) =
                                            x_1999 iii21_4244 k_append_x_x_x_7832
                                          in
                                          x_6215
                                            (fun (x0_7844:bool) ->
                                               fun (x1_7844:int) -> k_append_x_x_7846 true x0_7844 x1_7844)
                                      in
                                      x_6209
                                        (fun (x0_7875:bool) ->
                                           fun (x10_7875:bool) ->
                                             fun (x11_7875:int) ->
                                               k_append_x_7703 true x0_7876 x1_7876 false true 0 x0_7875 x10_7875
                                                 x11_7875)))
                            else
                              if iii20_4244 = false then
                                let x_6153 (k_append_x_x_7883:(bool -> int -> X)) = x_1999 iii01_4244 k_append_x_x_7883 in
                                x_6153
                                  (fun (x0_7934:bool) ->
                                     fun (x1_7934:int) ->
                                       (let x_6163 (k_append_x_x_7895:(bool -> int -> X)) =
                                          x_1998 iii11_4244 k_append_x_x_7895
                                        in
                                        x_6163
                                          (fun (x0_7933:bool) ->
                                             fun (x1_7933:int) ->
                                               k_append_x_7703 true x0_7934 x1_7934 true x0_7933 x1_7933 false true 0)))
                              else
                                let x_6121 (k_append_x_x_7943:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                  x_x_x_4829 iii01_4244 iii11_4244 iii21_4244 k_append_x_x_7943
                                in
                                x_6121
                                  (fun (x00_7975:bool) ->
                                     fun (x01_7975:int) ->
                                       fun (x10_7975:bool) ->
                                         fun (x11_7975:int) ->
                                           fun (x20_7975:bool) ->
                                             fun (x21_7975:int) ->
                                               k_append_x_7703 true x00_7975 x01_7975 true x10_7975 x11_7975 true
                                                 x20_7975 x21_7975)
                        in
                        x_2063)
                   else
                     if x010_9943 <> false then
                       let xs'_1012 (x_1141:int) (k_append_xs'_7997:(bool -> int -> X)) =
                         let
                           x_5269
                                 (k_append_xs'_x_8022:(bool ->
                                                         bool ->
                                                           r011_8021:int ->
                                                             bool ->
                                                               bool ->
                                                                 r111_8021:int[\r111_8021. r011_8021 = r111_8021] -> X)) =
                           x_1014 true (x_1141 + 1) false 0 k_append_xs'_x_8022
                         in
                         x_5269
                           (fun (x00_8028:bool) ->
                              fun (x010_8028:bool) ->
                                fun (x011_8028:int) ->
                                  fun (x10_8028:bool) ->
                                    fun (x110_8028:bool) ->
                                      fun (x111_8028:int) -> k_append_xs'_7997 x010_8028 x011_8028)
                       in
                       let rec
                         xs'_x_4665 (x_4639:int) (x_4640:int) (k_append_xs'_x_8038:(bool -> int -> bool -> int -> X)) =
                         let
                           x_5284
                                 (k_append_xs'_x_x_8063:(bool ->
                                                           bool ->
                                                             r011_8062:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8062:
                                                                    int[\r111_8062. r011_8062 = r111_8062] -> X)) =
                           x_1014 true (x_4639 + 1) false 0 k_append_xs'_x_x_8063
                         in
                         x_5284
                           (fun (x00_8106:bool) ->
                              fun (x010_8106:bool) ->
                                fun (x011_8106:int) ->
                                  fun (x10_8106:bool) ->
                                    fun (x110_8106:bool) ->
                                      fun (x111_8106:int) ->
                                        (let
                                           x_5299
                                                 (k_append_xs'_x_x_8093:(
                                                 bool ->
                                                   bool ->
                                                     r011_8092:int ->
                                                       bool ->
                                                         bool -> r111_8092:int[\r111_8092. r011_8092 = r111_8092] -> X)) =
                                           x_1014 false 0 true x_4640 k_append_xs'_x_x_8093
                                         in
                                         x_5299
                                           (fun (x00_8105:bool) ->
                                              fun (x010_8105:bool) ->
                                                fun (x011_8105:int) ->
                                                  fun (x10_8105:bool) ->
                                                    fun (x110_8105:bool) ->
                                                      fun (x111_8105:int) ->
                                                        k_append_xs'_x_8038 x010_8106 x011_8106 x110_8105 x111_8105)))
                       in
                       let cons_1216 (x_1212:int) (xs_1213:(int -> (bool -> int -> X) -> X)) =
                         let x_1813 (i_1211:int) (k_append_cons_x_8123:(bool -> int -> X)) =
                           if i_1211 = 0 then
                             k_append_cons_x_8123 true x_1212
                           else
                             let x_5310 (k_append_cons_x_x_8136:(bool -> int -> X)) =
                               xs_1213 (i_1211 - 1) k_append_cons_x_x_8136
                             in
                             x_5310 (fun (x0_8161:bool) -> fun (x1_8161:int) -> k_append_cons_x_8123 x0_8161 x1_8161)
                         in
                         let
                           x_2037 (ii00_4073:bool) (ii01_4073:int) (ii10_4073:bool) (ii11_4073:int)
                                 (k_append_cons_x_8170:(bool ->
                                                          bool ->
                                                            r011_8167:int ->
                                                              bool ->
                                                                bool ->
                                                                  r111_8167:int[\r111_8167. r011_8167 = r111_8167] -> X)) =
                           if ii00_4073 = false then
                             let x_5386 (k_append_cons_x_x_8205:(bool -> bool -> int -> X)) =
                               if ii10_4073 = false then
                                 k_append_cons_x_x_8205 false true 0
                               else
                                 let x_5392 (k_append_cons_x_x_x_8191:(bool -> int -> X)) =
                                   xs_1213 ii11_4073 k_append_cons_x_x_x_8191
                                 in
                                 x_5392
                                   (fun (x0_8203:bool) ->
                                      fun (x1_8203:int) -> k_append_cons_x_x_8205 true x0_8203 x1_8203)
                             in
                             x_5386
                               (fun (x0_8226:bool) ->
                                  fun (x10_8226:bool) ->
                                    fun (x11_8226:int) -> k_append_cons_x_8170 false true 0 x0_8226 x10_8226 x11_8226)
                           else
                             if ii10_4073 = false then
                               let x_5351 (k_append_cons_x_x_8233:(bool -> int -> X)) =
                                 x_1813 ii01_4073 k_append_cons_x_x_8233
                               in
                               x_5351
                                 (fun (x0_8263:bool) ->
                                    fun (x1_8263:int) -> k_append_cons_x_8170 true x0_8263 x1_8263 false true 0)
                             else
                               let x_5328 (k_append_cons_x_x_8270:(bool -> int -> X)) =
                                 x_1813 ii01_4073 k_append_cons_x_x_8270
                               in
                               x_5328
                                 (fun (x0_8307:bool) ->
                                    fun (x1_8307:int) ->
                                      (let x_5338 (k_append_cons_x_x_8282:(bool -> int -> X)) =
                                         xs_1213 ii11_4073 k_append_cons_x_x_8282
                                       in
                                       x_5338
                                         (fun (x0_8306:bool) ->
                                            fun (x1_8306:int) ->
                                              k_append_cons_x_8170 true x0_8307 x1_8307 true x0_8306 x1_8306)))
                         in
                         x_2037
                       in
                       let
                         x_2040 (ii00_3991:bool) (ii01_3991:int) (ii10_3991:bool) (ii11_3991:int)
                               (k_append_x_8333:(bool ->
                                                   bool ->
                                                     r011_8332:int ->
                                                       bool ->
                                                         bool -> r111_8332:int[\r111_8332. r011_8332 = r111_8332] -> X)) =
                         if ii00_3991 = false then
                           let x_5486 (k_append_x_x_8368:(bool -> bool -> int -> X)) =
                             if ii10_3991 = false then
                               k_append_x_x_8368 false true 0
                             else
                               let x_5492 (k_append_x_x_x_8354:(bool -> int -> X)) =
                                 x_1999 ii11_3991 k_append_x_x_x_8354
                               in
                               x_5492
                                 (fun (x0_8366:bool) -> fun (x1_8366:int) -> k_append_x_x_8368 true x0_8366 x1_8366)
                           in
                           x_5486
                             (fun (x0_8389:bool) ->
                                fun (x10_8389:bool) ->
                                  fun (x11_8389:int) -> k_append_x_8333 false true 0 x0_8389 x10_8389 x11_8389)
                         else
                           if ii10_3991 = false then
                             let x_5451 (k_append_x_x_8396:(bool -> int -> X)) = xs'_1012 ii01_3991 k_append_x_x_8396 in
                             x_5451
                               (fun (x0_8426:bool) ->
                                  fun (x1_8426:int) -> k_append_x_8333 true x0_8426 x1_8426 false true 0)
                           else
                             let x_5427 (k_append_x_x_8434:(bool -> int -> bool -> int -> X)) =
                               xs'_x_4665 ii01_3991 ii11_3991 k_append_x_x_8434
                             in
                             x_5427
                               (fun (x00_8458:bool) ->
                                  fun (x01_8458:int) ->
                                    fun (x10_8458:bool) ->
                                      fun (x11_8458:int) ->
                                        k_append_x_8333 true x00_8458 x01_8458 true x10_8458 x11_8458)
                       in
                       let
                         x_5559
                               (k_append_x_8573:((bool ->
                                                    int ->
                                                      bool ->
                                                        int ->
                                                          bool ->
                                                            int ->
                                                              (bool ->
                                                                 bool ->
                                                                   r011_8570:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8570:
                                                                    int[\r111_8570. r011_8570 = r111_8570] ->
                                                                    bool -> bool -> int -> X) -> X) -> X)) =
                         append_1085 x_2040 k_append_x_8573
                       in
                       x_5559
                         (fun (x_9565:(bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               bool ->
                                                 int ->
                                                   (bool ->
                                                      bool ->
                                                        r011_9563:int ->
                                                          bool ->
                                                            bool ->
                                                              r111_9563:
                                                                int[\r111_9563. r011_9563 = r111_9563] ->
                                                                bool -> bool -> int -> X) -> X)) ->
                            k_append_7470
                              (let x_2044 (i_3953:int) (k_append_x_8595:(bool -> int -> X)) =
                                 let
                                   x_5583
                                         (k_append_x_x_8628:(bool ->
                                                               bool ->
                                                                 r011_8627:int ->
                                                                   bool ->
                                                                    bool ->
                                                                    r111_8627:
                                                                    int[\r111_8627. r011_8627 = r111_8627] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_9565 true i_3953 false 0 false 0 k_append_x_x_8628
                                 in
                                 x_5583
                                   (fun (x00_8634:bool) ->
                                      fun (x010_8634:bool) ->
                                        fun (x011_8634:int) ->
                                          fun (x10_8634:bool) ->
                                            fun (x110_8634:bool) ->
                                              fun (x111_8634:int) ->
                                                fun (x20_8634:bool) ->
                                                  fun (x210_8634:bool) ->
                                                    fun (x211_8634:int) -> k_append_x_8595 x010_8634 x011_8634)
                               in
                               let x_2045 (i_3943:int) (k_append_x_8639:(bool -> int -> X)) =
                                 let
                                   x_5609
                                         (k_append_x_x_8672:(bool ->
                                                               bool ->
                                                                 r011_8671:int ->
                                                                   bool ->
                                                                    bool ->
                                                                    r111_8671:
                                                                    int[\r111_8671. r011_8671 = r111_8671] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_9565 false 0 true i_3943 false 0 k_append_x_x_8672
                                 in
                                 x_5609
                                   (fun (x00_8678:bool) ->
                                      fun (x010_8678:bool) ->
                                        fun (x011_8678:int) ->
                                          fun (x10_8678:bool) ->
                                            fun (x110_8678:bool) ->
                                              fun (x111_8678:int) ->
                                                fun (x20_8678:bool) ->
                                                  fun (x210_8678:bool) ->
                                                    fun (x211_8678:int) -> k_append_x_8639 x110_8678 x111_8678)
                               in
                               let x_2046 (i_3933:int) (k_append_x_8683:(bool -> int -> X)) =
                                 let
                                   x_5635
                                         (k_append_x_x_8716:(bool ->
                                                               bool ->
                                                                 r011_8715:int ->
                                                                   bool ->
                                                                    bool ->
                                                                    r111_8715:
                                                                    int[\r111_8715. r011_8715 = r111_8715] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_9565 false 0 false 0 true i_3933 k_append_x_x_8716
                                 in
                                 x_5635
                                   (fun (x00_8722:bool) ->
                                      fun (x010_8722:bool) ->
                                        fun (x011_8722:int) ->
                                          fun (x10_8722:bool) ->
                                            fun (x110_8722:bool) ->
                                              fun (x111_8722:int) ->
                                                fun (x20_8722:bool) ->
                                                  fun (x210_8722:bool) ->
                                                    fun (x211_8722:int) -> k_append_x_8683 x210_8722 x211_8722)
                               in
                               let rec
                                 x_x_4717 (x_4679:int) (x_4680:int)
                                         (k_append_x_x_8728:(bool -> int -> bool -> int -> X)) =
                                 let
                                   x_5653
                                         (k_append_x_x_x_8761:(bool ->
                                                                 bool ->
                                                                   r011_8760:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8760:
                                                                    int[\r111_8760. r011_8760 = r111_8760] ->
                                                                    bool -> bool -> int -> X)) =
                                   x_9565 false 0 true x_4679 false 0 k_append_x_x_x_8761
                                 in
                                 x_5653
                                   (fun (x00_8851:bool) ->
                                      fun (x010_8851:bool) ->
                                        fun (x011_8851:int) ->
                                          fun (x10_8851:bool) ->
                                            fun (x110_8851:bool) ->
                                              fun (x111_8851:int) ->
                                                fun (x20_8851:bool) ->
                                                  fun (x210_8851:bool) ->
                                                    fun (x211_8851:int) ->
                                                      (let
                                                         x_5671
                                                               (k_append_x_x_x_8799:(
                                                               bool ->
                                                                 bool ->
                                                                   r011_8798:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8798:
                                                                    int[\r111_8798. r011_8798 = r111_8798] ->
                                                                    bool -> bool -> int -> X)) =
                                                         x_9565 false 0 false 0 true x_4680 k_append_x_x_x_8799
                                                       in
                                                       x_5671
                                                         (fun (x00_8850:bool) ->
                                                            fun (x010_8850:bool) ->
                                                              fun (x011_8850:int) ->
                                                                fun (x10_8850:bool) ->
                                                                  fun (x110_8850:bool) ->
                                                                    fun (x111_8850:int) ->
                                                                    fun (x20_8850:bool) ->
                                                                    fun (x210_8850:bool) ->
                                                                    fun (x211_8850:int) ->
                                                                    (let
                                                                    x_7440
                                                                     (k_append_x_x_x_8837:(
                                                                    bool ->
                                                                    bool ->
                                                                    r011_8836:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_8836:
                                                                    int[\r111_8836. r011_8836 = r111_8836] ->
                                                                    bool -> bool -> int -> X)) =
                                                                    x_9565 false 0 true x_4679 true x_4680
                                                                    k_append_x_x_x_8837
                                                                    in
                                                                    x_7440
                                                                    (fun (x00_8849:bool) ->
                                                                    fun (x010_8849:bool) ->
                                                                    fun (x011_8849:int) ->
                                                                    fun (x10_8849:bool) ->
                                                                    fun (x110_8849:bool) ->
                                                                    fun (x111_8849:int) ->
                                                                    fun (x20_8849:bool) ->
                                                                    fun (x210_8849:bool) ->
                                                                    fun (x211_8849:int) ->
                                                                    k_append_x_x_8728 x110_8849 x111_8849 x210_8849
                                                                    x211_8849)))))
                               in
                               let
                                 x_2049 (ii00_3916:bool) (ii01_3916:int) (ii10_3916:bool) (ii11_3916:int)
                                       (k_append_x_8856:(bool ->
                                                           bool ->
                                                             r011_8853:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_8853:
                                                                    int[\r111_8853. r011_8853 = r111_8853] -> X)) =
                                 if ii00_3916 = false then
                                   let x_5742 (k_append_x_x_8891:(bool -> bool -> int -> X)) =
                                     if ii10_3916 = false then
                                       k_append_x_x_8891 false true 0
                                     else
                                       let x_5748 (k_append_x_x_x_8877:(bool -> int -> X)) =
                                         x_2046 ii11_3916 k_append_x_x_x_8877
                                       in
                                       x_5748
                                         (fun (x0_8889:bool) ->
                                            fun (x1_8889:int) -> k_append_x_x_8891 true x0_8889 x1_8889)
                                   in
                                   x_5742
                                     (fun (x0_8912:bool) ->
                                        fun (x10_8912:bool) ->
                                          fun (x11_8912:int) -> k_append_x_8856 false true 0 x0_8912 x10_8912 x11_8912)
                                 else
                                   if ii10_3916 = false then
                                     let x_5707 (k_append_x_x_8919:(bool -> int -> X)) =
                                       x_2045 ii01_3916 k_append_x_x_8919
                                     in
                                     x_5707
                                       (fun (x0_8949:bool) ->
                                          fun (x1_8949:int) -> k_append_x_8856 true x0_8949 x1_8949 false true 0)
                                   else
                                     let x_5683 (k_append_x_x_8957:(bool -> int -> bool -> int -> X)) =
                                       x_x_4717 ii01_3916 ii11_3916 k_append_x_x_8957
                                     in
                                     x_5683
                                       (fun (x00_8981:bool) ->
                                          fun (x01_8981:int) ->
                                            fun (x10_8981:bool) ->
                                              fun (x11_8981:int) ->
                                                k_append_x_8856 true x00_8981 x01_8981 true x10_8981 x11_8981)
                               in
                               let x_2054 (i_3880:int) (k_append_x_9094:(bool -> int -> X)) =
                                 let
                                   x_5834
                                         (k_append_x_x_9119:(bool ->
                                                               bool ->
                                                                 r011_9118:int ->
                                                                   bool ->
                                                                    bool ->
                                                                    r111_9118:
                                                                    int[\r111_9118. r011_9118 = r111_9118] -> X)) =
                                   cons_1216 x011_9943 x_2044 true i_3880 false 0 k_append_x_x_9119
                                 in
                                 x_5834
                                   (fun (x00_9125:bool) ->
                                      fun (x010_9125:bool) ->
                                        fun (x011_9125:int) ->
                                          fun (x10_9125:bool) ->
                                            fun (x110_9125:bool) ->
                                              fun (x111_9125:int) -> k_append_x_9094 x010_9125 x011_9125)
                               in
                               let rec
                                 x_x_x_4770 (x_4731:int) (x_4732:int) (x_4733:int)
                                           (k_append_x_x_x_9132:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                 let
                                   x_5848
                                         (k_append_x_x_x_x_9157:(bool ->
                                                                   bool ->
                                                                    r011_9156:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9156:
                                                                    int[\r111_9156. r011_9156 = r111_9156] -> X)) =
                                   cons_1216 x011_9943 x_2044 true x_4731 false 0 k_append_x_x_x_x_9157
                                 in
                                 x_5848
                                   (fun (x00_9233:bool) ->
                                      fun (x010_9233:bool) ->
                                        fun (x011_9233:int) ->
                                          fun (x10_9233:bool) ->
                                            fun (x110_9233:bool) ->
                                              fun (x111_9233:int) ->
                                                (let
                                                   x_5862
                                                         (k_append_x_x_x_x_9187:(
                                                         bool ->
                                                           bool ->
                                                             r011_9186:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9186:
                                                                    int[\r111_9186. r011_9186 = r111_9186] -> X)) =
                                                   x_1014 true x_4732 false 0 k_append_x_x_x_x_9187
                                                 in
                                                 x_5862
                                                   (fun (x00_9232:bool) ->
                                                      fun (x010_9232:bool) ->
                                                        fun (x011_9232:int) ->
                                                          fun (x10_9232:bool) ->
                                                            fun (x110_9232:bool) ->
                                                              fun (x111_9232:int) ->
                                                                (let
                                                                   x_5876
                                                                    (k_append_x_x_x_x_9217:(
                                                                   bool ->
                                                                    bool ->
                                                                    r011_9216:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_9216:
                                                                    int[\r111_9216. r011_9216 = r111_9216] -> X)) =
                                                                   x_1014 false 0 true x_4733 k_append_x_x_x_x_9217
                                                                 in
                                                                 x_5876
                                                                   (fun (x00_9231:bool) ->
                                                                    fun (x010_9231:bool) ->
                                                                    fun (x011_9231:int) ->
                                                                    fun (x10_9231:bool) ->
                                                                    fun (x110_9231:bool) ->
                                                                    fun (x111_9231:int) ->
                                                                    k_append_x_x_x_9132 x010_9233 x011_9233 x010_9232
                                                                    x011_9232 x110_9231 x111_9231)))))
                               in
                               let
                                 x_2059 (iii00_3848:bool) (iii01_3848:int) (iii10_3848:bool) (iii11_3848:int)
                                       (iii20_3848:bool) (iii21_3848:int)
                                       (k_append_x_9274:(bool ->
                                                           bool ->
                                                             r011_9271:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9271:
                                                                    int[\r111_9271. r011_9271 = r111_9271] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_3848 = false then
                                   let x_6046 (k_append_x_x_9309:(bool -> bool -> int -> X)) =
                                     if iii10_3848 = false then
                                       k_append_x_x_9309 false true 0
                                     else
                                       let x_6052 (k_append_x_x_x_9295:(bool -> int -> X)) =
                                         x_1998 iii11_3848 k_append_x_x_x_9295
                                       in
                                       x_6052
                                         (fun (x0_9307:bool) ->
                                            fun (x1_9307:int) -> k_append_x_x_9309 true x0_9307 x1_9307)
                                   in
                                   x_6046
                                     (fun (x0_9370:bool) ->
                                        fun (x10_9370:bool) ->
                                          fun (x11_9370:int) ->
                                            (let x_6072 (k_append_x_x_9346:(bool -> bool -> int -> X)) =
                                               if iii20_3848 = false then
                                                 k_append_x_x_9346 false true 0
                                               else
                                                 let x_6078 (k_append_x_x_x_9332:(bool -> int -> X)) =
                                                   x_1999 iii21_3848 k_append_x_x_x_9332
                                                 in
                                                 x_6078
                                                   (fun (x0_9344:bool) ->
                                                      fun (x1_9344:int) -> k_append_x_x_9346 true x0_9344 x1_9344)
                                             in
                                             x_6072
                                               (fun (x0_9369:bool) ->
                                                  fun (x10_9369:bool) ->
                                                    fun (x11_9369:int) ->
                                                      k_append_x_9274 false true 0 x0_9370 x10_9370 x11_9370 x0_9369
                                                        x10_9369 x11_9369)))
                                 else
                                   if iii10_3848 = false then
                                     let x_5984 (k_append_x_x_9377:(bool -> int -> X)) =
                                       x_2054 iii01_3848 k_append_x_x_9377
                                     in
                                     x_5984
                                       (fun (x0_9447:bool) ->
                                          fun (x1_9447:int) ->
                                            (let x_5999 (k_append_x_x_9417:(bool -> bool -> int -> X)) =
                                               if iii20_3848 = false then
                                                 k_append_x_x_9417 false true 0
                                               else
                                                 let x_6005 (k_append_x_x_x_9403:(bool -> int -> X)) =
                                                   x_1999 iii21_3848 k_append_x_x_x_9403
                                                 in
                                                 x_6005
                                                   (fun (x0_9415:bool) ->
                                                      fun (x1_9415:int) -> k_append_x_x_9417 true x0_9415 x1_9415)
                                             in
                                             x_5999
                                               (fun (x0_9446:bool) ->
                                                  fun (x10_9446:bool) ->
                                                    fun (x11_9446:int) ->
                                                      k_append_x_9274 true x0_9447 x1_9447 false true 0 x0_9446
                                                        x10_9446 x11_9446)))
                                   else
                                     if iii20_3848 = false then
                                       let x_5943 (k_append_x_x_9454:(bool -> int -> X)) =
                                         x_2054 iii01_3848 k_append_x_x_9454
                                       in
                                       x_5943
                                         (fun (x0_9505:bool) ->
                                            fun (x1_9505:int) ->
                                              (let x_5953 (k_append_x_x_9466:(bool -> int -> X)) =
                                                 x_1998 iii11_3848 k_append_x_x_9466
                                               in
                                               x_5953
                                                 (fun (x0_9504:bool) ->
                                                    fun (x1_9504:int) ->
                                                      k_append_x_9274 true x0_9505 x1_9505 true x0_9504 x1_9504 false
                                                        true 0)))
                                     else
                                       let
                                         x_5911 (k_append_x_x_9514:(bool -> int -> bool -> int -> bool -> int -> X)) =
                                         x_x_x_4770 iii01_3848 iii11_3848 iii21_3848 k_append_x_x_9514
                                       in
                                       x_5911
                                         (fun (x00_9546:bool) ->
                                            fun (x01_9546:int) ->
                                              fun (x10_9546:bool) ->
                                                fun (x11_9546:int) ->
                                                  fun (x20_9546:bool) ->
                                                    fun (x21_9546:int) ->
                                                      k_append_x_9274 true x00_9546 x01_9546 true x10_9546 x11_9546
                                                        true x20_9546 x21_9546)
                               in
                               x_2059))
                     else
                       let x_1798 (k_append_x_9605:((int -> (bool -> int -> X) -> X) -> X)) = _|_ in
                       x_1798
                         (fun (x_9928:(int -> (bool -> int -> X) -> X)) ->
                            k_append_7470
                              (let
                                 x_2013 (iii00_3419:bool) (iii01_3419:int) (iii10_3419:bool) (iii11_3419:int)
                                       (iii20_3419:bool) (iii21_3419:int)
                                       (k_append_x_9613:(bool ->
                                                           bool ->
                                                             r011_9610:int ->
                                                               bool ->
                                                                 bool ->
                                                                   r111_9610:
                                                                    int[\r111_9610. r011_9610 = r111_9610] ->
                                                                    bool -> bool -> int -> X)) =
                                 if iii00_3419 = false then
                                   let x_5190 (k_append_x_x_9648:(bool -> bool -> int -> X)) =
                                     if iii10_3419 = false then
                                       k_append_x_x_9648 false true 0
                                     else
                                       let x_5196 (k_append_x_x_x_9634:(bool -> int -> X)) =
                                         x_1998 iii11_3419 k_append_x_x_x_9634
                                       in
                                       x_5196
                                         (fun (x0_9646:bool) ->
                                            fun (x1_9646:int) -> k_append_x_x_9648 true x0_9646 x1_9646)
                                   in
                                   x_5190
                                     (fun (x0_9709:bool) ->
                                        fun (x10_9709:bool) ->
                                          fun (x11_9709:int) ->
                                            (let x_5216 (k_append_x_x_9685:(bool -> bool -> int -> X)) =
                                               if iii20_3419 = false then
                                                 k_append_x_x_9685 false true 0
                                               else
                                                 let x_5222 (k_append_x_x_x_9671:(bool -> int -> X)) =
                                                   x_1999 iii21_3419 k_append_x_x_x_9671
                                                 in
                                                 x_5222
                                                   (fun (x0_9683:bool) ->
                                                      fun (x1_9683:int) -> k_append_x_x_9685 true x0_9683 x1_9683)
                                             in
                                             x_5216
                                               (fun (x0_9708:bool) ->
                                                  fun (x10_9708:bool) ->
                                                    fun (x11_9708:int) ->
                                                      k_append_x_9613 false true 0 x0_9709 x10_9709 x11_9709 x0_9708
                                                        x10_9708 x11_9708)))
                                 else
                                   if iii10_3419 = false then
                                     let x_5128 (k_append_x_x_9716:(bool -> int -> X)) =
                                       x_9928 iii01_3419 k_append_x_x_9716
                                     in
                                     x_5128
                                       (fun (x0_9786:bool) ->
                                          fun (x1_9786:int) ->
                                            (let x_5143 (k_append_x_x_9756:(bool -> bool -> int -> X)) =
                                               if iii20_3419 = false then
                                                 k_append_x_x_9756 false true 0
                                               else
                                                 let x_5149 (k_append_x_x_x_9742:(bool -> int -> X)) =
                                                   x_1999 iii21_3419 k_append_x_x_x_9742
                                                 in
                                                 x_5149
                                                   (fun (x0_9754:bool) ->
                                                      fun (x1_9754:int) -> k_append_x_x_9756 true x0_9754 x1_9754)
                                             in
                                             x_5143
                                               (fun (x0_9785:bool) ->
                                                  fun (x10_9785:bool) ->
                                                    fun (x11_9785:int) ->
                                                      k_append_x_9613 true x0_9786 x1_9786 false true 0 x0_9785
                                                        x10_9785 x11_9785)))
                                   else
                                     if iii20_3419 = false then
                                       let x_5087 (k_append_x_x_9793:(bool -> int -> X)) =
                                         x_9928 iii01_3419 k_append_x_x_9793
                                       in
                                       x_5087
                                         (fun (x0_9844:bool) ->
                                            fun (x1_9844:int) ->
                                              (let x_5097 (k_append_x_x_9805:(bool -> int -> X)) =
                                                 x_1998 iii11_3419 k_append_x_x_9805
                                               in
                                               x_5097
                                                 (fun (x0_9843:bool) ->
                                                    fun (x1_9843:int) ->
                                                      k_append_x_9613 true x0_9844 x1_9844 true x0_9843 x1_9843 false
                                                        true 0)))
                                     else
                                       let x_5053 (k_append_x_x_9851:(bool -> int -> X)) =
                                         x_9928 iii01_3419 k_append_x_x_9851
                                       in
                                       x_5053
                                         (fun (x0_9909:bool) ->
                                            fun (x1_9909:int) ->
                                              (let x_5063 (k_append_x_x_9863:(bool -> int -> X)) =
                                                 x_1998 iii11_3419 k_append_x_x_9863
                                               in
                                               x_5063
                                                 (fun (x0_9908:bool) ->
                                                    fun (x1_9908:int) ->
                                                      (let x_5073 (k_append_x_x_9875:(bool -> int -> X)) =
                                                         x_1999 iii21_3419 k_append_x_x_9875
                                                       in
                                                       x_5073
                                                         (fun (x0_9907:bool) ->
                                                            fun (x1_9907:int) ->
                                                              k_append_x_9613 true x0_9909 x1_9909 true x0_9908 x1_9908
                                                                true x0_9907 x1_9907)))))
                               in
                               x_2013))))
 in
 let rec make_list_1013 (n_1014:int) (k_make_list_9973:((int -> (bool -> int -> X) -> X) -> X)) =
   if n_1014 < 0 then
     k_make_list_9973 (fun (x_1305:int) -> fun (k_make_list_9975:(bool -> int -> X)) -> k_make_list_9975 false 0)
   else
     let cons_1299 (x_1295:int) (xs_1296:(int -> (bool -> int -> X) -> X)) =
       let x_1853 (i_1294:int) (k_make_list_cons_x_9994:(bool -> int -> X)) =
         if i_1294 = 0 then
           k_make_list_cons_x_9994 true x_1295
         else
           let x_6324 (k_make_list_cons_x_x_10007:(bool -> int -> X)) = xs_1296 (i_1294 - 1) k_make_list_cons_x_x_10007 in
           x_6324 (fun (x0_10032:bool) -> fun (x1_10032:int) -> k_make_list_cons_x_9994 x0_10032 x1_10032)
       in
       let
         x_2079 (ii00_3171:bool) (ii01_3171:int) (ii10_3171:bool) (ii11_3171:int)
               (k_make_list_cons_x_10041:(bool ->
                                            bool ->
                                              r011_10038:int ->
                                                bool ->
                                                  bool -> r111_10038:int[\r111_10038. r011_10038 = r111_10038] -> X)) =
         if ii00_3171 = false then
           let x_6400 (k_make_list_cons_x_x_10076:(bool -> bool -> int -> X)) =
             if ii10_3171 = false then
               k_make_list_cons_x_x_10076 false true 0
             else
               let x_6406 (k_make_list_cons_x_x_x_10062:(bool -> int -> X)) =
                 xs_1296 ii11_3171 k_make_list_cons_x_x_x_10062
               in
               x_6406 (fun (x0_10074:bool) -> fun (x1_10074:int) -> k_make_list_cons_x_x_10076 true x0_10074 x1_10074)
           in
           x_6400
             (fun (x0_10097:bool) ->
                fun (x10_10097:bool) ->
                  fun (x11_10097:int) -> k_make_list_cons_x_10041 false true 0 x0_10097 x10_10097 x11_10097)
         else
           if ii10_3171 = false then
             let x_6365 (k_make_list_cons_x_x_10104:(bool -> int -> X)) = x_1853 ii01_3171 k_make_list_cons_x_x_10104 in
             x_6365
               (fun (x0_10134:bool) ->
                  fun (x1_10134:int) -> k_make_list_cons_x_10041 true x0_10134 x1_10134 false true 0)
           else
             let x_6342 (k_make_list_cons_x_x_10141:(bool -> int -> X)) = x_1853 ii01_3171 k_make_list_cons_x_x_10141 in
             x_6342
               (fun (x0_10178:bool) ->
                  fun (x1_10178:int) ->
                    (let x_6352 (k_make_list_cons_x_x_10153:(bool -> int -> X)) =
                       xs_1296 ii11_3171 k_make_list_cons_x_x_10153
                     in
                     x_6352
                       (fun (x0_10177:bool) ->
                          fun (x1_10177:int) -> k_make_list_cons_x_10041 true x0_10178 x1_10178 true x0_10177 x1_10177)))
       in
       x_2079
     in
     let x_6437 (k_make_list_x_10213:((int -> (bool -> int -> X) -> X) -> X)) =
       make_list_1013 (n_1014 - 1) k_make_list_x_10213
     in
     x_6437
       (fun (x_10347:(int -> (bool -> int -> X) -> X)) ->
          (let x_6439 (k_make_list_x_10234:(int -> X)) = rand_int_cps () k_make_list_x_10234 in
           x_6439
             (fun (x_10343:int) ->
                k_make_list_9973
                  (let x_2086 (i_3095:int) (k_make_list_x_10272:(bool -> int -> X)) =
                     let
                       x_6458
                             (k_make_list_x_x_10297:(bool ->
                                                       bool ->
                                                         r011_10296:int ->
                                                           bool ->
                                                             bool ->
                                                               r111_10296:
                                                                 int[\r111_10296. r011_10296 = r111_10296] -> X)) =
                       cons_1299 x_10343 x_10347 true i_3095 false 0 k_make_list_x_x_10297
                     in
                     x_6458
                       (fun (x00_10303:bool) ->
                          fun (x010_10303:bool) ->
                            fun (x011_10303:int) ->
                              fun (x10_10303:bool) ->
                                fun (x110_10303:bool) ->
                                  fun (x111_10303:int) -> k_make_list_x_10272 x010_10303 x011_10303)
                   in
                   x_2086))))
 in
 let rec
   list_eq_1015 (xsys_1016:(bool -> int -> bool -> int -> (bool -> bool -> unit -> bool -> bool -> unit -> X) -> X))
               (k_list_eq_10382:(bool -> (int -> (bool -> unit -> X) -> X) -> (int -> (bool -> unit -> X) -> X) -> X)) =
   let x_2093 (i_2977:int) (k_list_eq_x_10392:(bool -> unit -> X)) =
     let x_6503 (k_list_eq_x_x_10417:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
       xsys_1016 true i_2977 false 0 k_list_eq_x_x_10417
     in
     x_6503
       (fun (x00_10423:bool) ->
          fun (x010_10423:bool) ->
            fun (x011_10423:unit) ->
              fun (x10_10423:bool) ->
                fun (x110_10423:bool) -> fun (x111_10423:unit) -> k_list_eq_x_10392 x010_10423 x011_10423)
   in
   let x_2094 (i_2970:int) (k_list_eq_x_10436:(bool -> unit -> X)) =
     let x_6522 (k_list_eq_x_x_10461:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
       xsys_1016 false 0 true i_2970 k_list_eq_x_x_10461
     in
     x_6522
       (fun (x00_10467:bool) ->
          fun (x010_10467:bool) ->
            fun (x011_10467:unit) ->
              fun (x10_10467:bool) ->
                fun (x110_10467:bool) -> fun (x111_10467:unit) -> k_list_eq_x_10436 x110_10467 x111_10467)
   in
   let x_6540 (k_list_eq_x_10501:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
     xsys_1016 true 0 true 0 k_list_eq_x_10501
   in
   x_6540
     (fun (x00_11383:bool) ->
        fun (x010_11383:bool) ->
          fun (x011_11383:unit) ->
            fun (x10_11383:bool) ->
              fun (x110_11383:bool) ->
                fun (x111_11383:unit) ->
                  (let k_list_eq_10509 (b_12930:bool) =
                     if b_12930 then
                       k_list_eq_10382 true x_2093 x_2094
                     else
                       let k_list_eq_13265 (b_13271:bool) =
                         if b_13271 then
                           let xs'_13277 (x_13278:int) (k_list_eq_xs'_13279:(bool -> unit -> X)) =
                             let x_13281 (k_list_eq_xs'_x_13282:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                               xsys_1016 true (x_13278 + 1) true 0 k_list_eq_xs'_x_13282
                             in
                             x_13281
                               (fun (x00_13294:bool) ->
                                  fun (x010_13294:bool) ->
                                    fun (x011_13294:unit) ->
                                      fun (x10_13294:bool) ->
                                        fun (x110_13294:bool) ->
                                          fun (x111_13294:unit) -> k_list_eq_xs'_13279 x010_13294 x011_13294)
                           in
                           let ys'_13297 (x_13298:int) (k_list_eq_ys'_13299:(bool -> unit -> X)) =
                             let x_13301 (k_list_eq_ys'_x_13302:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                               xsys_1016 true 0 true (x_13298 + 1) k_list_eq_ys'_x_13302
                             in
                             x_13301
                               (fun (x00_13314:bool) ->
                                  fun (x010_13314:bool) ->
                                    fun (x011_13314:unit) ->
                                      fun (x10_13314:bool) ->
                                        fun (x110_13314:bool) ->
                                          fun (x111_13314:unit) -> k_list_eq_ys'_13299 x110_13314 x111_13314)
                           in
                           let rec
                             xs'_ys'_13317 (x_13318:int) (x_13319:int)
                                          (k_list_eq_xs'_ys'_13320:(bool -> unit -> bool -> unit -> X)) =
                             let
                               x_13322 (k_list_eq_xs'_ys'_x_13323:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                               xsys_1016 true (x_13318 + 1) true 0 k_list_eq_xs'_ys'_x_13323
                             in
                             x_13322
                               (fun (x00_13335:bool) ->
                                  fun (x010_13335:bool) ->
                                    fun (x011_13335:unit) ->
                                      fun (x10_13335:bool) ->
                                        fun (x110_13335:bool) ->
                                          fun (x111_13335:unit) ->
                                            (let
                                               x_13337
                                                      (k_list_eq_xs'_ys'_x_13338:(
                                                      bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                                               xsys_1016 true 0 true (x_13319 + 1) k_list_eq_xs'_ys'_x_13338
                                             in
                                             x_13337
                                               (fun (x00_13350:bool) ->
                                                  fun (x010_13350:bool) ->
                                                    fun (x011_13350:unit) ->
                                                      fun (x10_13350:bool) ->
                                                        fun (x110_13350:bool) ->
                                                          fun (x111_13350:unit) ->
                                                            k_list_eq_xs'_ys'_13320 x010_13335 x011_13335 x110_13350
                                                              x111_13350)))
                           in
                           let
                             x_13356 (ii00_13357:bool) (ii01_13357:int) (ii10_13357:bool) (ii11_13357:int)
                                    (k_list_eq_x_13358:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                             if ii00_13357 = false then
                               let x_13399 (k_list_eq_x_x_13400:(bool -> bool -> unit -> X)) =
                                 if ii10_13357 = false then
                                   k_list_eq_x_x_13400 false true ()
                                 else
                                   let x_13404 (k_list_eq_x_x_x_13405:(bool -> unit -> X)) =
                                     ys'_13297 ii11_13357 k_list_eq_x_x_x_13405
                                   in
                                   x_13404
                                     (fun (x0_13408:bool) ->
                                        fun (x1_13408:unit) -> k_list_eq_x_x_13400 true x0_13408 x1_13408)
                               in
                               x_13399
                                 (fun (x0_13418:bool) ->
                                    fun (x10_13418:bool) ->
                                      fun (x11_13418:unit) ->
                                        k_list_eq_x_13358 false true () x0_13418 x10_13418 x11_13418)
                             else
                               if ii10_13357 = false then
                                 let x_13381 (k_list_eq_x_x_13382:(bool -> unit -> X)) =
                                   xs'_13277 ii01_13357 k_list_eq_x_x_13382
                                 in
                                 x_13381
                                   (fun (x0_13385:bool) ->
                                      fun (x1_13385:unit) -> k_list_eq_x_13358 true x0_13385 x1_13385 false true ())
                               else
                                 let x_13366 (k_list_eq_x_x_13367:(bool -> unit -> bool -> unit -> X)) =
                                   xs'_ys'_13317 ii01_13357 ii11_13357 k_list_eq_x_x_13367
                                 in
                                 x_13366
                                   (fun (x00_13370:bool) ->
                                      fun (x01_13370:unit) ->
                                        fun (x10_13370:bool) ->
                                          fun (x11_13370:unit) ->
                                            k_list_eq_x_13358 true x00_13370 x01_13370 true x10_13370 x11_13370)
                           in
                           let
                             x_13470
                                    (k_list_eq_x_13471:(bool ->
                                                          (int -> (bool -> unit -> X) -> X) ->
                                                            (int -> (bool -> unit -> X) -> X) -> X)) =
                             list_eq_1015 x_13356 k_list_eq_x_13471
                           in
                           x_13470
                             (fun (x0_13474:bool) ->
                                fun (x1_13474:(int -> (bool -> unit -> X) -> X)) ->
                                  fun (x2_13474:(int -> (bool -> unit -> X) -> X)) ->
                                    (let
                                       x_13476 (ii00_13477:bool) (ii01_13477:int) (ii10_13477:bool) (ii11_13477:int)
                                              (k_list_eq_x_13478:(bool -> bool -> unit -> bool -> bool -> unit -> X)) =
                                       if ii00_13477 = false then
                                         let x_13525 (k_list_eq_x_x_13526:(bool -> bool -> unit -> X)) =
                                           if ii10_13477 = false then
                                             k_list_eq_x_x_13526 false true ()
                                           else
                                             let x_13530 (k_list_eq_x_x_x_13531:(bool -> unit -> X)) =
                                               x2_13474 ii11_13477 k_list_eq_x_x_x_13531
                                             in
                                             x_13530
                                               (fun (x0_13534:bool) ->
                                                  fun (x1_13534:unit) -> k_list_eq_x_x_13526 true x0_13534 x1_13534)
                                         in
                                         x_13525
                                           (fun (x0_13544:bool) ->
                                              fun (x10_13544:bool) ->
                                                fun (x11_13544:unit) ->
                                                  k_list_eq_x_13478 false true () x0_13544 x10_13544 x11_13544)
                                       else
                                         if ii10_13477 = false then
                                           let x_13507 (k_list_eq_x_x_13508:(bool -> unit -> X)) =
                                             x1_13474 ii01_13477 k_list_eq_x_x_13508
                                           in
                                           x_13507
                                             (fun (x0_13511:bool) ->
                                                fun (x1_13511:unit) ->
                                                  k_list_eq_x_13478 true x0_13511 x1_13511 false true ())
                                         else
                                           let x_13486 (k_list_eq_x_x_13487:(bool -> unit -> X)) =
                                             x1_13474 ii01_13477 k_list_eq_x_x_13487
                                           in
                                           x_13486
                                             (fun (x0_13490:bool) ->
                                                fun (x1_13490:unit) ->
                                                  (let x_13492 (k_list_eq_x_x_13493:(bool -> unit -> X)) =
                                                     x2_13474 ii11_13477 k_list_eq_x_x_13493
                                                   in
                                                   x_13492
                                                     (fun (x0_13496:bool) ->
                                                        fun (x1_13496:unit) ->
                                                          k_list_eq_x_13478 true x0_13490 x1_13490 true x0_13496
                                                            x1_13496)))
                                     in
                                     let k_list_eq_13600 (x_13704:bool) = k_list_eq_10382 x_13704 x_2093 x_2094 in
                                     if x011_11383 = x111_11383 then
                                       k_list_eq_13600 x0_13474
                                     else
                                       k_list_eq_13600 false))
                         else
                           k_list_eq_10382 false x_2093 x_2094
                       in
                       if x010_11383 <> false then
                         k_list_eq_13265 (x110_11383 <> false)
                       else
                         k_list_eq_13265 false
                   in
                   if x010_11383 = false then
                     k_list_eq_10509 (x110_11383 = false)
                   else
                     k_list_eq_10509 false))
 in
 let main_1021 (i_1022:int) (n_1023:int) (k_main_11407:(unit -> X)) =
   let x_6964 (k_main_x_11420:((int -> (bool -> int -> X) -> X) -> X)) = make_list_1013 n_1023 k_main_x_11420 in
   x_6964
     (fun (x_12327:(int -> (bool -> int -> X) -> X)) ->
        (let f_1671 (x_1452:int) (k_main_f_11435:(bool -> int -> X)) = k_main_f_11435 false 0 in
         let
           x_2171 (ix00_2549:bool) (ix01_2549:int) (ix10_2549:bool) (ix11_2549:int)
                 (k_main_x_11448:(bool ->
                                    bool ->
                                      r011_11447:int ->
                                        bool -> bool -> r111_11447:int[\r111_11447. r011_11447 = r111_11447] -> X)) =
           if ix00_2549 = false then
             let x_7034 (k_main_x_x_11483:(bool -> bool -> int -> X)) =
               if ix10_2549 = false then
                 k_main_x_x_11483 false true 0
               else
                 let x_7040 (k_main_x_x_x_11469:(bool -> int -> X)) = f_1671 ix11_2549 k_main_x_x_x_11469 in
                 x_7040 (fun (x0_11481:bool) -> fun (x1_11481:int) -> k_main_x_x_11483 true x0_11481 x1_11481)
             in
             x_7034
               (fun (x0_11504:bool) ->
                  fun (x10_11504:bool) ->
                    fun (x11_11504:int) -> k_main_x_11448 false true 0 x0_11504 x10_11504 x11_11504)
           else
             if ix10_2549 = false then
               let x_6999 (k_main_x_x_11511:(bool -> int -> X)) = x_12327 ix01_2549 k_main_x_x_11511 in
               x_6999 (fun (x0_11541:bool) -> fun (x1_11541:int) -> k_main_x_11448 true x0_11541 x1_11541 false true 0)
             else
               let x_6976 (k_main_x_x_11548:(bool -> int -> X)) = x_12327 ix01_2549 k_main_x_x_11548 in
               x_6976
                 (fun (x0_11585:bool) ->
                    fun (x1_11585:int) ->
                      (let x_6986 (k_main_x_x_11560:(bool -> int -> X)) = f_1671 ix11_2549 k_main_x_x_11560 in
                       x_6986
                         (fun (x0_11584:bool) ->
                            fun (x1_11584:int) -> k_main_x_11448 true x0_11585 x1_11585 true x0_11584 x1_11584)))
         in
         let
           x_7107
                 (k_main_x_11691:((bool ->
                                     int ->
                                       bool ->
                                         int ->
                                           bool ->
                                             int ->
                                               (bool ->
                                                  bool ->
                                                    r011_11688:int ->
                                                      bool ->
                                                        bool ->
                                                          r111_11688:
                                                            int[\r111_11688. r011_11688 = r111_11688] ->
                                                            bool -> bool -> int -> X) -> X) -> X)) =
           append_1085 x_2171 k_main_x_11691
         in
         x_7107
           (fun (x_12307:(bool ->
                            int ->
                              bool ->
                                int ->
                                  bool ->
                                    int ->
                                      (bool ->
                                         bool ->
                                           r011_12305:int ->
                                             bool ->
                                               bool ->
                                                 r111_12305:int[\r111_12305. r011_12305 = r111_12305] ->
                                                   bool -> bool -> int -> X) -> X)) ->
              (let x_2176 (i_2501:int) (k_main_x_11758:(bool -> int -> X)) =
                 let
                   x_7157
                         (k_main_x_x_11791:(bool ->
                                              bool ->
                                                r011_11790:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_11790:int[\r111_11790. r011_11790 = r111_11790] ->
                                                        bool -> bool -> int -> X)) =
                   x_12307 false 0 true i_2501 false 0 k_main_x_x_11791
                 in
                 x_7157
                   (fun (x00_11797:bool) ->
                      fun (x010_11797:bool) ->
                        fun (x011_11797:int) ->
                          fun (x10_11797:bool) ->
                            fun (x110_11797:bool) ->
                              fun (x111_11797:int) ->
                                fun (x20_11797:bool) ->
                                  fun (x210_11797:bool) -> fun (x211_11797:int) -> k_main_x_11758 x110_11797 x111_11797)
               in
               let x_2177 (i_2491:int) (k_main_x_11804:(bool -> int -> X)) =
                 let
                   x_7183
                         (k_main_x_x_11837:(bool ->
                                              bool ->
                                                r011_11836:int ->
                                                  bool ->
                                                    bool ->
                                                      r111_11836:int[\r111_11836. r011_11836 = r111_11836] ->
                                                        bool -> bool -> int -> X)) =
                   x_12307 false 0 false 0 true i_2491 k_main_x_x_11837
                 in
                 x_7183
                   (fun (x00_11843:bool) ->
                      fun (x010_11843:bool) ->
                        fun (x011_11843:int) ->
                          fun (x10_11843:bool) ->
                            fun (x110_11843:bool) ->
                              fun (x111_11843:int) ->
                                fun (x20_11843:bool) ->
                                  fun (x210_11843:bool) -> fun (x211_11843:int) -> k_main_x_11804 x210_11843 x211_11843)
               in
               let rec x_x_4927 (x_4889:int) (x_4890:int) (k_main_x_x_11850:(bool -> int -> bool -> int -> X)) =
                 let
                   x_7201
                         (k_main_x_x_x_11883:(bool ->
                                                bool ->
                                                  r011_11882:int ->
                                                    bool ->
                                                      bool ->
                                                        r111_11882:int[\r111_11882. r011_11882 = r111_11882] ->
                                                          bool -> bool -> int -> X)) =
                   x_12307 false 0 true x_4889 false 0 k_main_x_x_x_11883
                 in
                 x_7201
                   (fun (x00_11973:bool) ->
                      fun (x010_11973:bool) ->
                        fun (x011_11973:int) ->
                          fun (x10_11973:bool) ->
                            fun (x110_11973:bool) ->
                              fun (x111_11973:int) ->
                                fun (x20_11973:bool) ->
                                  fun (x210_11973:bool) ->
                                    fun (x211_11973:int) ->
                                      (let
                                         x_7219
                                               (k_main_x_x_x_11921:(bool ->
                                                                    bool ->
                                                                    r011_11920:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_11920:
                                                                    int[\r111_11920.
                                                                    r011_11920 = r111_11920] ->
                                                                    bool -> bool -> int -> X)) =
                                         x_12307 false 0 false 0 true x_4890 k_main_x_x_x_11921
                                       in
                                       x_7219
                                         (fun (x00_11972:bool) ->
                                            fun (x010_11972:bool) ->
                                              fun (x011_11972:int) ->
                                                fun (x10_11972:bool) ->
                                                  fun (x110_11972:bool) ->
                                                    fun (x111_11972:int) ->
                                                      fun (x20_11972:bool) ->
                                                        fun (x210_11972:bool) ->
                                                          fun (x211_11972:int) ->
                                                            (let
                                                               x_7408
                                                                (k_main_x_x_x_11959:(
                                                               bool ->
                                                                 bool ->
                                                                   r011_11958:int ->
                                                                    bool ->
                                                                    bool ->
                                                                    r111_11958:
                                                                    int[\r111_11958.
                                                                    r011_11958 = r111_11958] ->
                                                                    bool -> bool -> int -> X)) =
                                                               x_12307 false 0 true x_4889 true x_4890
                                                                 k_main_x_x_x_11959
                                                             in
                                                             x_7408
                                                               (fun (x00_11971:bool) ->
                                                                  fun (x010_11971:bool) ->
                                                                    fun (x011_11971:int) ->
                                                                    fun (x10_11971:bool) ->
                                                                    fun (x110_11971:bool) ->
                                                                    fun (x111_11971:int) ->
                                                                    fun (x20_11971:bool) ->
                                                                    fun (x210_11971:bool) ->
                                                                    fun (x211_11971:int) ->
                                                                    k_main_x_x_11850 x110_11971 x111_11971 x210_11971
                                                                    x211_11971)))))
               in
               let
                 x_2180 (ii00_2474:bool) (ii01_2474:int) (ii10_2474:bool) (ii11_2474:int)
                       (k_main_x_11981:(bool ->
                                          bool ->
                                            r011_11980:int ->
                                              bool -> bool -> r111_11980:int[\r111_11980. r011_11980 = r111_11980] -> X)) =
                 if ii00_2474 = false then
                   let x_7290 (k_main_x_x_12016:(bool -> bool -> int -> X)) =
                     if ii10_2474 = false then
                       k_main_x_x_12016 false true 0
                     else
                       let x_7296 (k_main_x_x_x_12002:(bool -> int -> X)) = x_2177 ii11_2474 k_main_x_x_x_12002 in
                       x_7296 (fun (x0_12014:bool) -> fun (x1_12014:int) -> k_main_x_x_12016 true x0_12014 x1_12014)
                   in
                   x_7290
                     (fun (x0_12037:bool) ->
                        fun (x10_12037:bool) ->
                          fun (x11_12037:int) -> k_main_x_11981 false true 0 x0_12037 x10_12037 x11_12037)
                 else
                   if ii10_2474 = false then
                     let x_7255 (k_main_x_x_12044:(bool -> int -> X)) = x_2176 ii01_2474 k_main_x_x_12044 in
                     x_7255
                       (fun (x0_12074:bool) ->
                          fun (x1_12074:int) -> k_main_x_11981 true x0_12074 x1_12074 false true 0)
                   else
                     let x_7231 (k_main_x_x_12082:(bool -> int -> bool -> int -> X)) =
                       x_x_4927 ii01_2474 ii11_2474 k_main_x_x_12082
                     in
                     x_7231
                       (fun (x00_12106:bool) ->
                          fun (x01_12106:int) ->
                            fun (x10_12106:bool) ->
                              fun (x11_12106:int) -> k_main_x_11981 true x00_12106 x01_12106 true x10_12106 x11_12106)
               in
               let
                 x_7384
                       (k_main_x_12226:(bool ->
                                          bool ->
                                            r011_12225:int ->
                                              bool ->
                                                bool ->
                                                  r111_12225:int[\r111_12225. r011_12225 = r111_12225] ->
                                                    bool -> bool -> int -> X)) =
                 x_12307 true i_1022 false 0 false 0 k_main_x_12226
               in
               x_7384
                 (fun (x00_12274:bool) ->
                    fun (x010_12274:bool) ->
                      fun (x011_12274:int) ->
                        fun (x10_12274:bool) ->
                          fun (x110_12274:bool) ->
                            fun (x111_12274:int) ->
                              fun (x20_12274:bool) ->
                                fun (x210_12274:bool) ->
                                  fun (x211_12274:int) ->
                                    (let x_7385 (k_main_x_12238:(bool -> int -> X)) = x_12327 i_1022 k_main_x_12238 in
                                     x_7385
                                       (fun (x0_12273:bool) ->
                                          fun (x1_12273:int) ->
                                            (let n_1696 (k_main_n_12249:(int -> X)) =
                                               if x010_12274 <> false then
                                                 k_main_n_12249 x011_12274
                                               else
                                                 _|_
                                             in
                                             n_1696
                                               (fun (n_12272:int) ->
                                                  (let n_1697 (k_main_n_12257:(int -> X)) =
                                                     if x0_12273 <> false then
                                                       k_main_n_12257 x1_12273
                                                     else
                                                       _|_
                                                   in
                                                   n_1697
                                                     (fun (n_12271:int) ->
                                                        (if n_12272 = n_12271 then
                                                           k_main_11407 ()
                                                         else
                                                           {|fail|} () k_main_11407))))))))))))
 in
 let x_7403 (k_x_12338:(int -> X)) = rand_int_cps () k_x_12338 in
 x_7403
   (fun (x_12383:int) ->
      (let x_7405 (k_x_12350:(int -> X)) = rand_int_cps () k_x_12350 in
       x_7405
         (fun (x_12382:int) ->
            (let x_7407 (k_x_12371:(unit -> X)) = main_1021 x_12383 x_12382 k_x_12371 in
             x_7407 (fun (x_12377:unit) -> {end})))))
