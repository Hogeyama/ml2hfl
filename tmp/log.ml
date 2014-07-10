MoCHi: Model Checker for Higher-Order Programs
  Build: _00810e4 (after 2014-07-10 17:01:42 +0900)
  FPAT version: b00026d
  TRecS version: 1.33
  OCaml version: 4.01.0
  Command: ./mochi.opt test_relabs/append_nil2_nth.ml -debug-module Ref_trans -disable-rc -color -tupling -gchi 
           -list-option -abs-remove-false

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

ASSERT: List.nth (append_1008 (make_list_1013 n_1023, [])) i_1022 = List.nth (make_list_1013 n_1023) i_1022
RAND: make_list_1013
RAND: main_1081
FUN: List.nth
FUN: List.nth
FUN': List.nth
FUN': List.nth
ALL_FUN_ARG: List.nth, List.nth
FUN_ARG: List.nth, make_list_1013
FUN_ARG: List.nth, append_1008
FUN_ARG': List.nth, List.nth


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
       let xs'_1012 (x_1135:int) = xs_1009 (x_1135 + 1) in
       let x_1011 = snd (xs_1009 0) in
       let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) (i_1205:int) =
         if i_1205 = 0 then
           (true, x_1206)
         else
           xs_1207 (i_1205 - 1)
       in
       cons_1210 x_1011 (append_1085 (xs'_1012, ys_1010))
     else
       _|_
 in
 let rec make_list_1013 (n_1014:int) =
   if n_1014 < 0 then
     fun (x_1299:int) -> (false, 0)
   else
     let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) (i_1288:int) =
       if i_1288 = 0 then
         (true, x_1289)
       else
         xs_1290 (i_1288 - 1)
     in
     cons_1293 (rand_int ()) (make_list_1013 (n_1014 - 1))
 in
 let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
   if fst ((fst xsys_1016) 0) = false && fst ((snd xsys_1016) 0) = false then
     true
   else
     if fst ((fst xsys_1016) 0) <> false && fst ((snd xsys_1016) 0) <> false then
       let x_1017 = snd ((fst xsys_1016) 0) in
       let xs'_1018 (x_1310:int) = (fst xsys_1016) (x_1310 + 1) in
       let y_1019 = snd ((snd xsys_1016) 0) in
       let ys'_1020 (x_1314:int) = (snd xsys_1016) (x_1314 + 1) in
       x_1017 = y_1019 && list_eq_1015 (xs'_1018, ys'_1020)
     else
       false
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let xs_1024 = make_list_1013 n_1023 in
   let ys_1025 = append_1085 (xs_1024, (fun (x_1446:int) -> (false, 0))) in
   if (let x_1496 = ys_1025 i_1022 in
       if fst x_1496 <> false then
         snd x_1496
       else
         _|_)
      = (let x_1486 = xs_1024 i_1022 in
         if fst x_1486 <> false then
           snd x_1486
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
   ((fun (x_1084:int) -> (let f_1500 = rand_int in
                          let n_1502 = f_1500 () in
                          n_1502)), x_1083)
 in
 let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
   let xs_1009 = fst x_1014 in
   let ys_1010 = snd x_1014 in
   let p_1508 = xs_1009 0 in
   let b_1510 = fst p_1508 in
   let b_1505 = b_1510 = false in
   if b_1505 then
     (ys_1010, x_1014)
   else
     let p_1515 = xs_1009 0 in
     let b_1517 = fst p_1515 in
     let b_1519 = b_1517 = false in
     let b_1512 = not b_1519 in
     if b_1512 then
       let xs'_1012 (x_1135:int) = let n_1523 = x_1135 + 1 in
                                   let p_1524 = xs_1009 n_1523 in
                                   p_1524 in
       let p_1527 = xs_1009 0 in
       let x_1011 = snd p_1527 in
       let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
         ((fun (i_1205:int) ->
             (let b_1529 = i_1205 = 0 in
              if b_1529 then
                (true, x_1206)
              else
                let n_1539 = i_1205 - 1 in
                let p_1540 = xs_1207 n_1539 in
                let xs_1712 (n_1713:int) = if n_1713 = n_1539 then
                                             p_1540
                                           else
                                             xs_1207 n_1713 in
                p_1540)),
          xs_1207)
       in
       let p_1546 = (xs'_1012, ys_1010) in
       let p_1728 = append_1085 p_1546 in
       let f_1547 = fst p_1728 in
       let p_1729 = snd p_1728 in
       let f_1551 = cons_1210 x_1011 in
       let p_1724 = f_1551 f_1547 in
       let f_1552 = fst p_1724 in
       let f_1725 = snd p_1724 in
       (f_1552, x_1014)
     else
       (_|_, x_1014)
 in
 let rec make_list_1013 (n_1014:int) =
   let b_1555 = n_1014 < 0 in
   if b_1555 then
     fun (x_1299:int) -> (false, 0)
   else
     let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
       ((fun (i_1288:int) ->
           (let b_1562 = i_1288 = 0 in
            if b_1562 then
              (true, x_1289)
            else
              let n_1572 = i_1288 - 1 in
              let p_1573 = xs_1290 n_1572 in
              let xs_1746 (n_1747:int) = if n_1747 = n_1572 then
                                           p_1573
                                         else
                                           xs_1290 n_1747 in
              p_1573)),
        xs_1290)
     in
     let n_1580 = n_1014 - 1 in
     let f_1581 = make_list_1013 n_1580 in
     let f_1574 = rand_int in
     let n_1576 = f_1574 () in
     let f_1585 = cons_1293 n_1576 in
     let p_1757 = f_1585 f_1581 in
     let f_1586 = fst p_1757 in
     let f_1758 = snd p_1757 in
     f_1586
 in
 let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
   let f_1590 = fst xsys_1016 in
   let p_1592 = f_1590 0 in
   let b_1594 = fst p_1592 in
   let b_1603 = b_1594 = false in
   let f_1597 = snd xsys_1016 in
   let p_1599 = f_1597 0 in
   let b_1601 = fst p_1599 in
   let b_1604 = b_1601 = false in
   let b_1588 = b_1603 && b_1604 in
   if b_1588 then
     (true, xsys_1016)
   else
     let f_1607 = fst xsys_1016 in
     let p_1609 = f_1607 0 in
     let b_1611 = fst p_1609 in
     let b_1613 = b_1611 = false in
     let b_1622 = not b_1613 in
     let f_1615 = snd xsys_1016 in
     let p_1617 = f_1615 0 in
     let b_1619 = fst p_1617 in
     let b_1621 = b_1619 = false in
     let b_1623 = not b_1621 in
     let b_1605 = b_1622 && b_1623 in
     if b_1605 then
       let f_1625 = fst xsys_1016 in
       let p_1627 = f_1625 0 in
       let x_1017 = snd p_1627 in
       let xs'_1018 (x_1310:int) =
         let n_1633 = x_1310 + 1 in
         let f_1632 = fst xsys_1016 in
         let p_1634 = f_1632 n_1633 in
         p_1634
       in
       let f_1636 = snd xsys_1016 in
       let p_1638 = f_1636 0 in
       let y_1019 = snd p_1638 in
       let ys'_1020 (x_1314:int) =
         let n_1644 = x_1314 + 1 in
         let f_1643 = snd xsys_1016 in
         let p_1645 = f_1643 n_1644 in
         p_1645
       in
       let b_1655 = x_1017 = y_1019 in
       let p_1653 = (xs'_1018, ys'_1020) in
       let p_1763 = list_eq_1015 p_1653 in
       let b_1654 = fst p_1763 in
       let p_1764 = snd p_1763 in
       (b_1655 && b_1654, xsys_1016)
     else
       (false, xsys_1016)
 in
 let main_1021 (i_1022:int) (n_1023:int) =
   let f_1659 = make_list_1013 n_1023 in
   let f_1665 (x_1446:int) = (false, 0) in
   let p_1669 = (f_1659, f_1665) in
   let p_1778 = append_1085 p_1669 in
   let f_1670 = fst p_1778 in
   let p_1779 = snd p_1778 in
   let p_1674 = f_1670 i_1022 in
   let b_1677 = fst p_1674 in
   let b_1679 = b_1677 = false in
   let b_1675 = not b_1679 in
   let n_1690 = if b_1675 then
                  snd p_1674
                else
                  _|_ in
   let p_1683 = f_1659 i_1022 in
   let b_1686 = fst p_1683 in
   let b_1688 = b_1686 = false in
   let b_1684 = not b_1688 in
   let n_1691 = if b_1684 then
                  snd p_1683
                else
                  _|_ in
   let b_1671 = n_1690 = n_1691 in
   if b_1671 then
     ()
   else
     let f_1692 = {fail} in
     let u_1694 = f_1692 () in
     u_1694
 in
 let f_1695 = rand_int in
 let n_1697 = f_1695 () in
 let f_1698 = rand_int in
 let n_1700 = f_1698 () in
 let f_1704 = main_1021 n_1697 in
 let u_1705 = f_1704 n_1700 in
 ()

INPUT: let List.nth_1082 x_1083 = ((fun x_1084 -> (let f_1500 = rand_int in
                                                   let n_1502 = f_1500 () in
                                                   n_1502)), x_1083) in
       let rec append_1085 x_1014 =
         let xs_1009 = fst x_1014 in
         let ys_1010 = snd x_1014 in
         let p_1508 = xs_1009 0 in
         let b_1510 = fst p_1508 in
         let b_1505 = b_1510 = false in
         if b_1505 then
           (ys_1010, x_1014)
         else
           let p_1515 = xs_1009 0 in
           let b_1517 = fst p_1515 in
           let b_1519 = b_1517 = false in
           let b_1512 = not b_1519 in
           if b_1512 then
             let xs'_1012 x_1135 = let n_1523 = x_1135 + 1 in
                                   let p_1524 = xs_1009 n_1523 in
                                   p_1524 in
             let p_1527 = xs_1009 0 in
             let x_1011 = snd p_1527 in
             let cons_1210 x_1206 xs_1207 =
               ((fun i_1205 ->
                   (let b_1529 = i_1205 = 0 in
                    if b_1529 then
                      (true, x_1206)
                    else
                      let n_1539 = i_1205 - 1 in
                      let p_1540 = xs_1207 n_1539 in
                      let xs_1712 n_1713 = if n_1713 = n_1539 then
                                             p_1540
                                           else
                                             xs_1207 n_1713 in
                      p_1540)),
                xs_1207)
             in
             let p_1546 = (xs'_1012, ys_1010) in
             let p_1728 = append_1085 p_1546 in
             let f_1547 = fst p_1728 in
             let p_1729 = snd p_1728 in
             let f_1551 = cons_1210 x_1011 in
             let p_1724 = f_1551 f_1547 in
             let f_1552 = fst p_1724 in
             let f_1725 = snd p_1724 in
             (f_1552, x_1014)
           else
             (_|_, x_1014)
       in
       let rec make_list_1013 n_1014 =
         let b_1555 = n_1014 < 0 in
         if b_1555 then
           fun x_1299 -> (false, 0)
         else
           let cons_1293 x_1289 xs_1290 =
             ((fun i_1288 ->
                 (let b_1562 = i_1288 = 0 in
                  if b_1562 then
                    (true, x_1289)
                  else
                    let n_1572 = i_1288 - 1 in
                    let p_1573 = xs_1290 n_1572 in
                    let xs_1746 n_1747 = if n_1747 = n_1572 then
                                           p_1573
                                         else
                                           xs_1290 n_1747 in
                    p_1573)),
              xs_1290)
           in
           let n_1580 = n_1014 - 1 in
           let f_1581 = make_list_1013 n_1580 in
           let f_1574 = rand_int in
           let n_1576 = f_1574 () in
           let f_1585 = cons_1293 n_1576 in
           let p_1757 = f_1585 f_1581 in
           let f_1586 = fst p_1757 in
           let f_1758 = snd p_1757 in
           f_1586
       in
       let rec list_eq_1015 xsys_1016 =
         let f_1590 = fst xsys_1016 in
         let p_1592 = f_1590 0 in
         let b_1594 = fst p_1592 in
         let b_1603 = b_1594 = false in
         let f_1597 = snd xsys_1016 in
         let p_1599 = f_1597 0 in
         let b_1601 = fst p_1599 in
         let b_1604 = b_1601 = false in
         let b_1588 = b_1603 && b_1604 in
         if b_1588 then
           (true, xsys_1016)
         else
           let f_1607 = fst xsys_1016 in
           let p_1609 = f_1607 0 in
           let b_1611 = fst p_1609 in
           let b_1613 = b_1611 = false in
           let b_1622 = not b_1613 in
           let f_1615 = snd xsys_1016 in
           let p_1617 = f_1615 0 in
           let b_1619 = fst p_1617 in
           let b_1621 = b_1619 = false in
           let b_1623 = not b_1621 in
           let b_1605 = b_1622 && b_1623 in
           if b_1605 then
             let f_1625 = fst xsys_1016 in
             let p_1627 = f_1625 0 in
             let x_1017 = snd p_1627 in
             let xs'_1018 x_1310 =
               let n_1633 = x_1310 + 1 in
               let f_1632 = fst xsys_1016 in
               let p_1634 = f_1632 n_1633 in
               p_1634
             in
             let f_1636 = snd xsys_1016 in
             let p_1638 = f_1636 0 in
             let y_1019 = snd p_1638 in
             let ys'_1020 x_1314 =
               let n_1644 = x_1314 + 1 in
               let f_1643 = snd xsys_1016 in
               let p_1645 = f_1643 n_1644 in
               p_1645
             in
             let b_1655 = x_1017 = y_1019 in
             let p_1653 = (xs'_1018, ys'_1020) in
             let p_1763 = list_eq_1015 p_1653 in
             let b_1654 = fst p_1763 in
             let p_1764 = snd p_1763 in
             (b_1655 && b_1654, xsys_1016)
           else
             (false, xsys_1016)
       in
       let main_1021 i_1022 n_1023 =
         let f_1659 = make_list_1013 n_1023 in
         let f_1665 x_1446 = (false, 0) in
         let p_1669 = (f_1659, f_1665) in
         let p_1778 = append_1085 p_1669 in
         let f_1670 = fst p_1778 in
         let p_1779 = snd p_1778 in
         let p_1674 = f_1670 i_1022 in
         let b_1677 = fst p_1674 in
         let b_1679 = b_1677 = false in
         let b_1675 = not b_1679 in
         let n_1690 = if b_1675 then
                        snd p_1674
                      else
                        _|_ in
         let p_1683 = f_1659 i_1022 in
         let b_1686 = fst p_1683 in
         let b_1688 = b_1686 = false in
         let b_1684 = not b_1688 in
         let n_1691 = if b_1684 then
                        snd p_1683
                      else
                        _|_ in
         let b_1671 = n_1690 = n_1691 in
         if b_1671 then
           ()
         else
           let f_1692 = {fail} in
           let u_1694 = f_1692 () in
           u_1694
       in
       let f_1695 = rand_int in
       let n_1697 = f_1695 () in
       let f_1698 = rand_int in
       let n_1700 = f_1698 () in
       let f_1704 = main_1021 n_1697 in
       let u_1705 = f_1704 n_1700 in
       ()
move_proj: let List.nth_1082 (x_1083:(int -> (bool * int))) =
             ((fun (x_1084:int) -> (let f_1500 = rand_int in
                                    let n_1502 = f_1500 () in
                                    n_1502)), x_1083)
           in
           let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
             let x1_1800 = fst x_1014 in
             let x2_1801 = snd x_1014 in
             let xs_1009 = x1_1800 in
             let ys_1010 = x2_1801 in
             let p_1508 = xs_1009 0 in
             let x1_1798 = fst p_1508 in
             let x2_1799 = snd p_1508 in
             let b_1505 = x1_1798 = false in
             if b_1505 then
               (ys_1010, x_1014)
             else
               let p_1515 = xs_1009 0 in
               let x1_1796 = fst p_1515 in
               let x2_1797 = snd p_1515 in
               let b_1519 = x1_1796 = false in
               let b_1512 = not b_1519 in
               if b_1512 then
                 let xs'_1012 (x_1135:int) =
                   let n_1523 = x_1135 + 1 in
                   let p_1524 = xs_1009 n_1523 in
                   let x1_1782 = fst p_1524 in
                   let x2_1783 = snd p_1524 in
                   p_1524
                 in
                 let p_1527 = xs_1009 0 in
                 let x1_1794 = fst p_1527 in
                 let x2_1795 = snd p_1527 in
                 let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
                   ((fun (i_1205:int) ->
                       (let b_1529 = i_1205 = 0 in
                        if b_1529 then
                          (true, x_1206)
                        else
                          let n_1539 = i_1205 - 1 in
                          let p_1540 = xs_1207 n_1539 in
                          let xs_1712 (n_1713:int) = if n_1713 = n_1539 then
                                                       p_1540
                                                     else
                                                       xs_1207 n_1713 in
                          p_1540)),
                    xs_1207)
                 in
                 let p_1546 = (xs'_1012, ys_1010) in
                 let x1_1792 = fst p_1546 in
                 let x2_1793 = snd p_1546 in
                 let p_1728 = append_1085 p_1546 in
                 let r1_1788 = fst p_1728 in
                 let x2_1789 = snd p_1728 in
                 let x1_1790 = fst x2_1789 in
                 let x2_1791 = snd x2_1789 in
                 let f_1551 = cons_1210 x2_1795 in
                 let p_1724 = f_1551 r1_1788 in
                 let r1_1784 = fst p_1724 in
                 let xs2_1785 = snd p_1724 in
                 (r1_1784, x_1014)
               else
                 (_|_, x_1014)
           in
           let rec make_list_1013 (n_1014:int) =
             let b_1555 = n_1014 < 0 in
             if b_1555 then
               fun (x_1299:int) -> (false, 0)
             else
               let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
                 ((fun (i_1288:int) ->
                     (let b_1562 = i_1288 = 0 in
                      if b_1562 then
                        (true, x_1289)
                      else
                        let n_1572 = i_1288 - 1 in
                        let p_1573 = xs_1290 n_1572 in
                        let xs_1746 (n_1747:int) = if n_1747 = n_1572 then
                                                     p_1573
                                                   else
                                                     xs_1290 n_1747 in
                        p_1573)),
                  xs_1290)
               in
               let n_1580 = n_1014 - 1 in
               let f_1581 = make_list_1013 n_1580 in
               let f_1574 = rand_int in
               let n_1576 = f_1574 () in
               let f_1585 = cons_1293 n_1576 in
               let p_1757 = f_1585 f_1581 in
               let r1_1802 = fst p_1757 in
               let xs2_1803 = snd p_1757 in
               let f_1586 = r1_1802 in
               let f_1758 = xs2_1803 in
               f_1586
           in
           let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
             let x1_1828 = fst xsys_1016 in
             let x2_1829 = snd xsys_1016 in
             let f_1590 = x1_1828 in
             let p_1592 = f_1590 0 in
             let x1_1826 = fst p_1592 in
             let x2_1827 = snd p_1592 in
             let b_1603 = x1_1826 = false in
             let f_1597 = x2_1829 in
             let p_1599 = f_1597 0 in
             let x1_1824 = fst p_1599 in
             let x2_1825 = snd p_1599 in
             let b_1604 = x1_1824 = false in
             let b_1588 = b_1603 && b_1604 in
             if b_1588 then
               (true, xsys_1016)
             else
               let f_1607 = x1_1828 in
               let p_1609 = f_1607 0 in
               let x1_1822 = fst p_1609 in
               let x2_1823 = snd p_1609 in
               let b_1613 = x1_1822 = false in
               let b_1622 = not b_1613 in
               let f_1615 = x2_1829 in
               let p_1617 = f_1615 0 in
               let x1_1820 = fst p_1617 in
               let x2_1821 = snd p_1617 in
               let b_1621 = x1_1820 = false in
               let b_1623 = not b_1621 in
               let b_1605 = b_1622 && b_1623 in
               if b_1605 then
                 let f_1625 = x1_1828 in
                 let p_1627 = f_1625 0 in
                 let x1_1818 = fst p_1627 in
                 let x2_1819 = snd p_1627 in
                 let xs'_1018 (x_1310:int) =
                   let n_1633 = x_1310 + 1 in
                   let f_1632 = x1_1828 in
                   let p_1634 = f_1632 n_1633 in
                   let x1_1804 = fst p_1634 in
                   let x2_1805 = snd p_1634 in
                   p_1634
                 in
                 let f_1636 = x2_1829 in
                 let p_1638 = f_1636 0 in
                 let x1_1816 = fst p_1638 in
                 let x2_1817 = snd p_1638 in
                 let ys'_1020 (x_1314:int) =
                   let n_1644 = x_1314 + 1 in
                   let f_1643 = x2_1829 in
                   let p_1645 = f_1643 n_1644 in
                   let x1_1806 = fst p_1645 in
                   let x2_1807 = snd p_1645 in
                   p_1645
                 in
                 let b_1655 = x2_1819 = x2_1817 in
                 let p_1653 = (xs'_1018, ys'_1020) in
                 let x1_1814 = fst p_1653 in
                 let x2_1815 = snd p_1653 in
                 let p_1763 = list_eq_1015 p_1653 in
                 let r1_1810 = fst p_1763 in
                 let x2_1811 = snd p_1763 in
                 let x1_1812 = fst x2_1811 in
                 let x2_1813 = snd x2_1811 in
                 (b_1655 && r1_1810, xsys_1016)
               else
                 (false, xsys_1016)
           in
           let main_1021 (i_1022:int) (n_1023:int) =
             let f_1659 = make_list_1013 n_1023 in
             let f_1665 (x_1446:int) = (false, 0) in
             let p_1669 = (f_1659, f_1665) in
             let x1_1840 = fst p_1669 in
             let x2_1841 = snd p_1669 in
             let p_1778 = append_1085 p_1669 in
             let r1_1836 = fst p_1778 in
             let x2_1837 = snd p_1778 in
             let x1_1838 = fst x2_1837 in
             let x2_1839 = snd x2_1837 in
             let p_1674 = r1_1836 i_1022 in
             let x1_1832 = fst p_1674 in
             let x2_1833 = snd p_1674 in
             let b_1679 = x1_1832 = false in
             let b_1675 = not b_1679 in
             let n_1690 = if b_1675 then
                            x2_1833
                          else
                            _|_ in
             let p_1683 = f_1659 i_1022 in
             let x1_1830 = fst p_1683 in
             let x2_1831 = snd p_1683 in
             let b_1688 = x1_1830 = false in
             let b_1684 = not b_1688 in
             let n_1691 = if b_1684 then
                            x2_1831
                          else
                            _|_ in
             let b_1671 = n_1690 = n_1691 in
             if b_1671 then
               ()
             else
               let f_1692 = {fail} in
               let u_1694 = f_1692 () in
               u_1694
           in
           let f_1695 = rand_int in
           let n_1697 = f_1695 () in
           let f_1698 = rand_int in
           let n_1700 = f_1698 () in
           let f_1704 = main_1021 n_1697 in
           let u_1705 = f_1704 n_1700 in
           ()
inline_no_effect: let List.nth_1082 (x_1083:(int -> (bool * int))) =
                    ((fun (x_1084:int) -> (let f_1500 = rand_int in
                                           let n_1502 = f_1500 () in
                                           n_1502)), x_1083)
                  in
                  let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
                    let x1_1800 = fst x_1014 in
                    let x2_1801 = snd x_1014 in
                    let xs_1009 = x1_1800 in
                    let ys_1010 = x2_1801 in
                    let p_1508 = xs_1009 0 in
                    let x1_1798 = fst p_1508 in
                    let x2_1799 = snd p_1508 in
                    let b_1505 = x1_1798 = false in
                    if b_1505 then
                      (ys_1010, x_1014)
                    else
                      let p_1515 = xs_1009 0 in
                      let x1_1796 = fst p_1515 in
                      let x2_1797 = snd p_1515 in
                      let b_1519 = x1_1796 = false in
                      let b_1512 = not b_1519 in
                      if b_1512 then
                        let xs'_1012 (x_1135:int) =
                          let n_1523 = x_1135 + 1 in
                          let p_1524 = xs_1009 n_1523 in
                          let x1_1782 = fst p_1524 in
                          let x2_1783 = snd p_1524 in
                          p_1524
                        in
                        let p_1527 = xs_1009 0 in
                        let x1_1794 = fst p_1527 in
                        let x2_1795 = snd p_1527 in
                        let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
                          ((fun (i_1205:int) ->
                              (let b_1529 = i_1205 = 0 in
                               if b_1529 then
                                 (true, x_1206)
                               else
                                 let n_1539 = i_1205 - 1 in
                                 let p_1540 = xs_1207 n_1539 in
                                 let xs_1712 (n_1713:int) = if n_1713 = n_1539 then
                                                              p_1540
                                                            else
                                                              xs_1207 n_1713 in
                                 p_1540)),
                           xs_1207)
                        in
                        let p_1546 = (xs'_1012, ys_1010) in
                        let x1_1792 = fst p_1546 in
                        let x2_1793 = snd p_1546 in
                        let p_1728 = append_1085 p_1546 in
                        let r1_1788 = fst p_1728 in
                        let x2_1789 = snd p_1728 in
                        let x1_1790 = fst x2_1789 in
                        let x2_1791 = snd x2_1789 in
                        let f_1551 = cons_1210 x2_1795 in
                        let p_1724 = f_1551 r1_1788 in
                        let r1_1784 = fst p_1724 in
                        let xs2_1785 = snd p_1724 in
                        (r1_1784, x_1014)
                      else
                        (_|_, x_1014)
                  in
                  let rec make_list_1013 (n_1014:int) =
                    let b_1555 = n_1014 < 0 in
                    if b_1555 then
                      fun (x_1299:int) -> (false, 0)
                    else
                      let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
                        ((fun (i_1288:int) ->
                            (let b_1562 = i_1288 = 0 in
                             if b_1562 then
                               (true, x_1289)
                             else
                               let n_1572 = i_1288 - 1 in
                               let p_1573 = xs_1290 n_1572 in
                               let xs_1746 (n_1747:int) = if n_1747 = n_1572 then
                                                            p_1573
                                                          else
                                                            xs_1290 n_1747 in
                               p_1573)),
                         xs_1290)
                      in
                      let n_1580 = n_1014 - 1 in
                      let f_1581 = make_list_1013 n_1580 in
                      let f_1574 = rand_int in
                      let n_1576 = f_1574 () in
                      let f_1585 = cons_1293 n_1576 in
                      let p_1757 = f_1585 f_1581 in
                      let r1_1802 = fst p_1757 in
                      let xs2_1803 = snd p_1757 in
                      let f_1586 = r1_1802 in
                      let f_1758 = xs2_1803 in
                      f_1586
                  in
                  let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
                    let x1_1828 = fst xsys_1016 in
                    let x2_1829 = snd xsys_1016 in
                    let f_1590 = x1_1828 in
                    let p_1592 = f_1590 0 in
                    let x1_1826 = fst p_1592 in
                    let x2_1827 = snd p_1592 in
                    let b_1603 = x1_1826 = false in
                    let f_1597 = x2_1829 in
                    let p_1599 = f_1597 0 in
                    let x1_1824 = fst p_1599 in
                    let x2_1825 = snd p_1599 in
                    let b_1604 = x1_1824 = false in
                    let b_1588 = b_1603 && b_1604 in
                    if b_1588 then
                      (true, xsys_1016)
                    else
                      let f_1607 = x1_1828 in
                      let p_1609 = f_1607 0 in
                      let x1_1822 = fst p_1609 in
                      let x2_1823 = snd p_1609 in
                      let b_1613 = x1_1822 = false in
                      let b_1622 = not b_1613 in
                      let f_1615 = x2_1829 in
                      let p_1617 = f_1615 0 in
                      let x1_1820 = fst p_1617 in
                      let x2_1821 = snd p_1617 in
                      let b_1621 = x1_1820 = false in
                      let b_1623 = not b_1621 in
                      let b_1605 = b_1622 && b_1623 in
                      if b_1605 then
                        let f_1625 = x1_1828 in
                        let p_1627 = f_1625 0 in
                        let x1_1818 = fst p_1627 in
                        let x2_1819 = snd p_1627 in
                        let xs'_1018 (x_1310:int) =
                          let n_1633 = x_1310 + 1 in
                          let f_1632 = x1_1828 in
                          let p_1634 = f_1632 n_1633 in
                          let x1_1804 = fst p_1634 in
                          let x2_1805 = snd p_1634 in
                          p_1634
                        in
                        let f_1636 = x2_1829 in
                        let p_1638 = f_1636 0 in
                        let x1_1816 = fst p_1638 in
                        let x2_1817 = snd p_1638 in
                        let ys'_1020 (x_1314:int) =
                          let n_1644 = x_1314 + 1 in
                          let f_1643 = x2_1829 in
                          let p_1645 = f_1643 n_1644 in
                          let x1_1806 = fst p_1645 in
                          let x2_1807 = snd p_1645 in
                          p_1645
                        in
                        let b_1655 = x2_1819 = x2_1817 in
                        let p_1653 = (xs'_1018, ys'_1020) in
                        let x1_1814 = fst p_1653 in
                        let x2_1815 = snd p_1653 in
                        let p_1763 = list_eq_1015 p_1653 in
                        let r1_1810 = fst p_1763 in
                        let x2_1811 = snd p_1763 in
                        let x1_1812 = fst x2_1811 in
                        let x2_1813 = snd x2_1811 in
                        (b_1655 && r1_1810, xsys_1016)
                      else
                        (false, xsys_1016)
                  in
                  let main_1021 (i_1022:int) (n_1023:int) =
                    let f_1659 = make_list_1013 n_1023 in
                    let f_1665 (x_1446:int) = (false, 0) in
                    let p_1669 = (f_1659, f_1665) in
                    let x1_1840 = fst p_1669 in
                    let x2_1841 = snd p_1669 in
                    let p_1778 = append_1085 p_1669 in
                    let r1_1836 = fst p_1778 in
                    let x2_1837 = snd p_1778 in
                    let x1_1838 = fst x2_1837 in
                    let x2_1839 = snd x2_1837 in
                    let p_1674 = r1_1836 i_1022 in
                    let x1_1832 = fst p_1674 in
                    let x2_1833 = snd p_1674 in
                    let b_1679 = x1_1832 = false in
                    let b_1675 = not b_1679 in
                    let n_1690 = if b_1675 then
                                   x2_1833
                                 else
                                   _|_ in
                    let p_1683 = f_1659 i_1022 in
                    let x1_1830 = fst p_1683 in
                    let x2_1831 = snd p_1683 in
                    let b_1688 = x1_1830 = false in
                    let b_1684 = not b_1688 in
                    let n_1691 = if b_1684 then
                                   x2_1831
                                 else
                                   _|_ in
                    let b_1671 = n_1690 = n_1691 in
                    if b_1671 then
                      ()
                    else
                      let f_1692 = {fail} in
                      let u_1694 = f_1692 () in
                      u_1694
                  in
                  let f_1695 = rand_int in
                  let n_1697 = f_1695 () in
                  let f_1698 = rand_int in
                  let n_1700 = f_1698 () in
                  let f_1704 = main_1021 n_1697 in
                  let u_1705 = f_1704 n_1700 in
                  ()
normalize_let: let List.nth_1082 (x_1083:(int -> (bool * int))) =
                 let x_1842 (x_1084:int) =
                   let f_1500 = rand_int in
                   let n_1502 = let x_1843 = f_1500 () in
                                x_1843 in
                   n_1502
                 in
                 let x_1846 = (x_1842, x_1083) in
                 x_1846
               in
               let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
                 let x1_1800 = let x_1847 = fst x_1014 in
                               x_1847 in
                 let x2_1801 = let x_1848 = snd x_1014 in
                               x_1848 in
                 let xs_1009 = x1_1800 in
                 let ys_1010 = x2_1801 in
                 let p_1508 = let x_1849 = xs_1009 0 in
                              x_1849 in
                 let x1_1798 = let x_1850 = fst p_1508 in
                               x_1850 in
                 let x2_1799 = let x_1851 = snd p_1508 in
                               x_1851 in
                 let b_1505 = let x_1852 = false in
                              let x_1853 = x1_1798 = x_1852 in
                              x_1853 in
                 if b_1505 then
                   let x_1905 = (ys_1010, x_1014) in
                   x_1905
                 else
                   let p_1515 = let x_1854 = xs_1009 0 in
                                x_1854 in
                   let x1_1796 = let x_1855 = fst p_1515 in
                                 x_1855 in
                   let x2_1797 = let x_1856 = snd p_1515 in
                                 x_1856 in
                   let b_1519 = let x_1857 = false in
                                let x_1858 = x1_1796 = x_1857 in
                                x_1858 in
                   let b_1512 = not b_1519 in
                   if b_1512 then
                     let xs'_1012 (x_1135:int) =
                       let n_1523 = x_1135 + 1 in
                       let p_1524 = let x_1865 = xs_1009 n_1523 in
                                    x_1865 in
                       let x1_1782 = let x_1866 = fst p_1524 in
                                     x_1866 in
                       let x2_1783 = let x_1867 = snd p_1524 in
                                     x_1867 in
                       p_1524
                     in
                     let p_1527 = let x_1868 = xs_1009 0 in
                                  x_1868 in
                     let x1_1794 = let x_1869 = fst p_1527 in
                                   x_1869 in
                     let x2_1795 = let x_1870 = snd p_1527 in
                                   x_1870 in
                     let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
                       let x_1871 (i_1205:int) =
                         let b_1529 = let x_1873 = i_1205 = 0 in
                                      x_1873 in
                         if b_1529 then
                           let x_1879 = true in
                           let x_1882 = (x_1879, x_1206) in
                           x_1882
                         else
                           let n_1539 = i_1205 - 1 in
                           let p_1540 = let x_1876 = xs_1207 n_1539 in
                                        x_1876 in
                           let xs_1712 (n_1713:int) =
                             if let x_1878 = n_1713 = n_1539 in
                                x_1878 then
                               p_1540
                             else
                               let x_1877 = xs_1207 n_1713 in
                               x_1877
                           in
                           p_1540
                       in
                       let x_1885 = (x_1871, xs_1207) in
                       x_1885
                     in
                     let p_1546 = let x_1888 = (xs'_1012, ys_1010) in
                                  x_1888 in
                     let x1_1792 = let x_1889 = fst p_1546 in
                                   x_1889 in
                     let x2_1793 = let x_1890 = snd p_1546 in
                                   x_1890 in
                     let p_1728 = let x_1891 = append_1085 p_1546 in
                                  x_1891 in
                     let r1_1788 = let x_1892 = fst p_1728 in
                                   x_1892 in
                     let x2_1789 = let x_1893 = snd p_1728 in
                                   x_1893 in
                     let x1_1790 = let x_1894 = fst x2_1789 in
                                   x_1894 in
                     let x2_1791 = let x_1895 = snd x2_1789 in
                                   x_1895 in
                     let f_1551 = let x_1896 = cons_1210 x2_1795 in
                                  x_1896 in
                     let p_1724 = let x_1897 = f_1551 r1_1788 in
                                  x_1897 in
                     let r1_1784 = let x_1898 = fst p_1724 in
                                   x_1898 in
                     let xs2_1785 = let x_1899 = snd p_1724 in
                                    x_1899 in
                     let x_1902 = (r1_1784, x_1014) in
                     x_1902
                   else
                     let x_1859 = _|_ in
                     let x_1862 = (x_1859, x_1014) in
                     x_1862
               in
               let rec make_list_1013 (n_1014:int) =
                 let b_1555 = let x_1907 = n_1014 < 0 in
                              x_1907 in
                 if b_1555 then
                   fun (x_1299:int) -> (let x_1931 = false in
                                        let x_1935 = (x_1931, 0) in
                                        x_1935)
                 else
                   let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
                     let x_1908 (i_1288:int) =
                       let b_1562 = let x_1910 = i_1288 = 0 in
                                    x_1910 in
                       if b_1562 then
                         let x_1916 = true in
                         let x_1919 = (x_1916, x_1289) in
                         x_1919
                       else
                         let n_1572 = i_1288 - 1 in
                         let p_1573 = let x_1913 = xs_1290 n_1572 in
                                      x_1913 in
                         let xs_1746 (n_1747:int) =
                           if let x_1915 = n_1747 = n_1572 in
                              x_1915 then
                             p_1573
                           else
                             let x_1914 = xs_1290 n_1747 in
                             x_1914
                         in
                         p_1573
                     in
                     let x_1922 = (x_1908, xs_1290) in
                     x_1922
                   in
                   let n_1580 = n_1014 - 1 in
                   let f_1581 = let x_1925 = make_list_1013 n_1580 in
                                x_1925 in
                   let f_1574 = rand_int in
                   let n_1576 = let x_1926 = f_1574 () in
                                x_1926 in
                   let f_1585 = let x_1927 = cons_1293 n_1576 in
                                x_1927 in
                   let p_1757 = let x_1928 = f_1585 f_1581 in
                                x_1928 in
                   let r1_1802 = let x_1929 = fst p_1757 in
                                 x_1929 in
                   let xs2_1803 = let x_1930 = snd p_1757 in
                                  x_1930 in
                   let f_1586 = r1_1802 in
                   let f_1758 = xs2_1803 in
                   f_1586
               in
               let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
                 let x1_1828 = let x_1936 = fst xsys_1016 in
                               x_1936 in
                 let x2_1829 = let x_1937 = snd xsys_1016 in
                               x_1937 in
                 let f_1590 = x1_1828 in
                 let p_1592 = let x_1938 = f_1590 0 in
                              x_1938 in
                 let x1_1826 = let x_1939 = fst p_1592 in
                               x_1939 in
                 let x2_1827 = let x_1940 = snd p_1592 in
                               x_1940 in
                 let b_1603 = let x_1941 = false in
                              let x_1942 = x1_1826 = x_1941 in
                              x_1942 in
                 let f_1597 = x2_1829 in
                 let p_1599 = let x_1943 = f_1597 0 in
                              x_1943 in
                 let x1_1824 = let x_1944 = fst p_1599 in
                               x_1944 in
                 let x2_1825 = let x_1945 = snd p_1599 in
                               x_1945 in
                 let b_1604 = let x_1946 = false in
                              let x_1947 = x1_1824 = x_1946 in
                              x_1947 in
                 let b_1588 = let x_1948 = b_1603 && b_1604 in
                              x_1948 in
                 if b_1588 then
                   let x_1996 = true in
                   let x_1999 = (x_1996, xsys_1016) in
                   x_1999
                 else
                   let f_1607 = x1_1828 in
                   let p_1609 = let x_1949 = f_1607 0 in
                                x_1949 in
                   let x1_1822 = let x_1950 = fst p_1609 in
                                 x_1950 in
                   let x2_1823 = let x_1951 = snd p_1609 in
                                 x_1951 in
                   let b_1613 = let x_1952 = false in
                                let x_1953 = x1_1822 = x_1952 in
                                x_1953 in
                   let b_1622 = not b_1613 in
                   let f_1615 = x2_1829 in
                   let p_1617 = let x_1954 = f_1615 0 in
                                x_1954 in
                   let x1_1820 = let x_1955 = fst p_1617 in
                                 x_1955 in
                   let x2_1821 = let x_1956 = snd p_1617 in
                                 x_1956 in
                   let b_1621 = let x_1957 = false in
                                let x_1958 = x1_1820 = x_1957 in
                                x_1958 in
                   let b_1623 = not b_1621 in
                   let b_1605 = let x_1959 = b_1622 && b_1623 in
                                x_1959 in
                   if b_1605 then
                     let f_1625 = x1_1828 in
                     let p_1627 = let x_1964 = f_1625 0 in
                                  x_1964 in
                     let x1_1818 = let x_1965 = fst p_1627 in
                                   x_1965 in
                     let x2_1819 = let x_1966 = snd p_1627 in
                                   x_1966 in
                     let xs'_1018 (x_1310:int) =
                       let n_1633 = x_1310 + 1 in
                       let f_1632 = x1_1828 in
                       let p_1634 = let x_1969 = f_1632 n_1633 in
                                    x_1969 in
                       let x1_1804 = let x_1970 = fst p_1634 in
                                     x_1970 in
                       let x2_1805 = let x_1971 = snd p_1634 in
                                     x_1971 in
                       p_1634
                     in
                     let f_1636 = x2_1829 in
                     let p_1638 = let x_1972 = f_1636 0 in
                                  x_1972 in
                     let x1_1816 = let x_1973 = fst p_1638 in
                                   x_1973 in
                     let x2_1817 = let x_1974 = snd p_1638 in
                                   x_1974 in
                     let ys'_1020 (x_1314:int) =
                       let n_1644 = x_1314 + 1 in
                       let f_1643 = x2_1829 in
                       let p_1645 = let x_1977 = f_1643 n_1644 in
                                    x_1977 in
                       let x1_1806 = let x_1978 = fst p_1645 in
                                     x_1978 in
                       let x2_1807 = let x_1979 = snd p_1645 in
                                     x_1979 in
                       p_1645
                     in
                     let b_1655 = let x_1980 = x2_1819 = x2_1817 in
                                  x_1980 in
                     let p_1653 = let x_1983 = (xs'_1018, ys'_1020) in
                                  x_1983 in
                     let x1_1814 = let x_1984 = fst p_1653 in
                                   x_1984 in
                     let x2_1815 = let x_1985 = snd p_1653 in
                                   x_1985 in
                     let p_1763 = let x_1986 = list_eq_1015 p_1653 in
                                  x_1986 in
                     let r1_1810 = let x_1987 = fst p_1763 in
                                   x_1987 in
                     let x2_1811 = let x_1988 = snd p_1763 in
                                   x_1988 in
                     let x1_1812 = let x_1989 = fst x2_1811 in
                                   x_1989 in
                     let x2_1813 = let x_1990 = snd x2_1811 in
                                   x_1990 in
                     let x_1991 = let x_1992 = b_1655 && r1_1810 in
                                  x_1992 in
                     let x_1995 = (x_1991, xsys_1016) in
                     x_1995
                   else
                     let x_1960 = false in
                     let x_1963 = (x_1960, xsys_1016) in
                     x_1963
               in
               let main_1021 (i_1022:int) (n_1023:int) =
                 let f_1659 = let x_2000 = make_list_1013 n_1023 in
                              x_2000 in
                 let f_1665 (x_1446:int) = let x_2001 = false in
                                           let x_2005 = (x_2001, 0) in
                                           x_2005 in
                 let p_1669 = let x_2008 = (f_1659, f_1665) in
                              x_2008 in
                 let x1_1840 = let x_2009 = fst p_1669 in
                               x_2009 in
                 let x2_1841 = let x_2010 = snd p_1669 in
                               x_2010 in
                 let p_1778 = let x_2011 = append_1085 p_1669 in
                              x_2011 in
                 let r1_1836 = let x_2012 = fst p_1778 in
                               x_2012 in
                 let x2_1837 = let x_2013 = snd p_1778 in
                               x_2013 in
                 let x1_1838 = let x_2014 = fst x2_1837 in
                               x_2014 in
                 let x2_1839 = let x_2015 = snd x2_1837 in
                               x_2015 in
                 let p_1674 = let x_2016 = r1_1836 i_1022 in
                              x_2016 in
                 let x1_1832 = let x_2017 = fst p_1674 in
                               x_2017 in
                 let x2_1833 = let x_2018 = snd p_1674 in
                               x_2018 in
                 let b_1679 = let x_2019 = false in
                              let x_2020 = x1_1832 = x_2019 in
                              x_2020 in
                 let b_1675 = not b_1679 in
                 let n_1690 = if b_1675 then
                                x2_1833
                              else
                                _|_ in
                 let p_1683 = let x_2021 = f_1659 i_1022 in
                              x_2021 in
                 let x1_1830 = let x_2022 = fst p_1683 in
                               x_2022 in
                 let x2_1831 = let x_2023 = snd p_1683 in
                               x_2023 in
                 let b_1688 = let x_2024 = false in
                              let x_2025 = x1_1830 = x_2024 in
                              x_2025 in
                 let b_1684 = not b_1688 in
                 let n_1691 = if b_1684 then
                                x2_1831
                              else
                                _|_ in
                 let b_1671 = let x_2026 = n_1690 = n_1691 in
                              x_2026 in
                 if b_1671 then
                   ()
                 else
                   let f_1692 = {fail} in
                   let u_1694 = let x_2027 = f_1692 () in
                                x_2027 in
                   u_1694
               in
               let f_1695 = rand_int in
               let n_1697 = let x_2028 = f_1695 () in
                            x_2028 in
               let f_1698 = rand_int in
               let n_1700 = let x_2029 = f_1698 () in
                            x_2029 in
               let f_1704 = let x_2030 = main_1021 n_1697 in
                            x_2030 in
               let u_1705 = let x_2031 = f_1704 n_1700 in
                            x_2031 in
               ()
flatten_let: let List.nth_1082 (x_1083:(int -> (bool * int))) =
               let x_1842 (x_1084:int) = let f_1500 = rand_int in
                                         let x_1843 = f_1500 () in
                                         x_1843 in
               let x_1846 = (x_1842, x_1083) in
               x_1846
             in
             let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
               let x_1847 = fst x_1014 in
               let x_1848 = snd x_1014 in
               let x_1849 = x_1847 0 in
               let x_1850 = fst x_1849 in
               let x_1851 = snd x_1849 in
               let x_1853 = x_1850 = false in
               if x_1853 then
                 let x_1905 = (x_1848, x_1014) in
                 x_1905
               else
                 let x_1854 = x_1847 0 in
                 let x_1855 = fst x_1854 in
                 let x_1856 = snd x_1854 in
                 let x_1858 = x_1855 = false in
                 let b_1512 = not x_1858 in
                 if b_1512 then
                   let xs'_1012 (x_1135:int) =
                     let n_1523 = x_1135 + 1 in
                     let x_1865 = x_1847 n_1523 in
                     let x_1866 = fst x_1865 in
                     let x_1867 = snd x_1865 in
                     x_1865
                   in
                   let x_1868 = x_1847 0 in
                   let x_1869 = fst x_1868 in
                   let x_1870 = snd x_1868 in
                   let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
                     let x_1871 (i_1205:int) =
                       let x_1873 = i_1205 = 0 in
                       if x_1873 then
                         let x_1882 = (true, x_1206) in
                         x_1882
                       else
                         let n_1539 = i_1205 - 1 in
                         let x_1876 = xs_1207 n_1539 in
                         let xs_1712 (n_1713:int) =
                           if let x_1878 = n_1713 = n_1539 in
                              x_1878 then
                             x_1876
                           else
                             let x_1877 = xs_1207 n_1713 in
                             x_1877
                         in
                         x_1876
                     in
                     let x_1885 = (x_1871, xs_1207) in
                     x_1885
                   in
                   let x_1888 = (xs'_1012, x_1848) in
                   let x_1889 = fst x_1888 in
                   let x_1890 = snd x_1888 in
                   let x_1891 = append_1085 x_1888 in
                   let x_1892 = fst x_1891 in
                   let x_1893 = snd x_1891 in
                   let x_1894 = fst x_1893 in
                   let x_1895 = snd x_1893 in
                   let x_1896 = cons_1210 x_1870 in
                   let x_1897 = x_1896 x_1892 in
                   let x_1898 = fst x_1897 in
                   let x_1899 = snd x_1897 in
                   let x_1902 = (x_1898, x_1014) in
                   x_1902
                 else
                   let x_1859 = _|_ in
                   let x_1862 = (x_1859, x_1014) in
                   x_1862
             in
             let rec make_list_1013 (n_1014:int) =
               let x_1907 = n_1014 < 0 in
               if x_1907 then
                 fun (x_1299:int) -> (let x_1935 = (false, 0) in
                                      x_1935)
               else
                 let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
                   let x_1908 (i_1288:int) =
                     let x_1910 = i_1288 = 0 in
                     if x_1910 then
                       let x_1919 = (true, x_1289) in
                       x_1919
                     else
                       let n_1572 = i_1288 - 1 in
                       let x_1913 = xs_1290 n_1572 in
                       let xs_1746 (n_1747:int) =
                         if let x_1915 = n_1747 = n_1572 in
                            x_1915 then
                           x_1913
                         else
                           let x_1914 = xs_1290 n_1747 in
                           x_1914
                       in
                       x_1913
                   in
                   let x_1922 = (x_1908, xs_1290) in
                   x_1922
                 in
                 let n_1580 = n_1014 - 1 in
                 let x_1925 = make_list_1013 n_1580 in
                 let f_1574 = rand_int in
                 let x_1926 = f_1574 () in
                 let x_1927 = cons_1293 x_1926 in
                 let x_1928 = x_1927 x_1925 in
                 let x_1929 = fst x_1928 in
                 let x_1930 = snd x_1928 in
                 x_1929
             in
             let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
               let x_1936 = fst xsys_1016 in
               let x_1937 = snd xsys_1016 in
               let x_1938 = x_1936 0 in
               let x_1939 = fst x_1938 in
               let x_1940 = snd x_1938 in
               let x_1942 = x_1939 = false in
               let x_1943 = x_1937 0 in
               let x_1944 = fst x_1943 in
               let x_1945 = snd x_1943 in
               let x_1947 = x_1944 = false in
               let x_1948 = x_1942 && x_1947 in
               if x_1948 then
                 let x_1999 = (true, xsys_1016) in
                 x_1999
               else
                 let x_1949 = x_1936 0 in
                 let x_1950 = fst x_1949 in
                 let x_1951 = snd x_1949 in
                 let x_1953 = x_1950 = false in
                 let b_1622 = not x_1953 in
                 let x_1954 = x_1937 0 in
                 let x_1955 = fst x_1954 in
                 let x_1956 = snd x_1954 in
                 let x_1958 = x_1955 = false in
                 let b_1623 = not x_1958 in
                 let x_1959 = b_1622 && b_1623 in
                 if x_1959 then
                   let x_1964 = x_1936 0 in
                   let x_1965 = fst x_1964 in
                   let x_1966 = snd x_1964 in
                   let xs'_1018 (x_1310:int) =
                     let n_1633 = x_1310 + 1 in
                     let x_1969 = x_1936 n_1633 in
                     let x_1970 = fst x_1969 in
                     let x_1971 = snd x_1969 in
                     x_1969
                   in
                   let x_1972 = x_1937 0 in
                   let x_1973 = fst x_1972 in
                   let x_1974 = snd x_1972 in
                   let ys'_1020 (x_1314:int) =
                     let n_1644 = x_1314 + 1 in
                     let x_1977 = x_1937 n_1644 in
                     let x_1978 = fst x_1977 in
                     let x_1979 = snd x_1977 in
                     x_1977
                   in
                   let x_1980 = x_1966 = x_1974 in
                   let x_1983 = (xs'_1018, ys'_1020) in
                   let x_1984 = fst x_1983 in
                   let x_1985 = snd x_1983 in
                   let x_1986 = list_eq_1015 x_1983 in
                   let x_1987 = fst x_1986 in
                   let x_1988 = snd x_1986 in
                   let x_1989 = fst x_1988 in
                   let x_1990 = snd x_1988 in
                   let x_1992 = x_1980 && x_1987 in
                   let x_1995 = (x_1992, xsys_1016) in
                   x_1995
                 else
                   let x_1963 = (false, xsys_1016) in
                   x_1963
             in
             let main_1021 (i_1022:int) (n_1023:int) =
               let x_2000 = make_list_1013 n_1023 in
               let f_1665 (x_1446:int) = let x_2005 = (false, 0) in
                                         x_2005 in
               let x_2008 = (x_2000, f_1665) in
               let x_2009 = fst x_2008 in
               let x_2010 = snd x_2008 in
               let x_2011 = append_1085 x_2008 in
               let x_2012 = fst x_2011 in
               let x_2013 = snd x_2011 in
               let x_2014 = fst x_2013 in
               let x_2015 = snd x_2013 in
               let x_2016 = x_2012 i_1022 in
               let x_2017 = fst x_2016 in
               let x_2018 = snd x_2016 in
               let x_2020 = x_2017 = false in
               let b_1675 = not x_2020 in
               let n_1690 = if b_1675 then
                              x_2018
                            else
                              _|_ in
               let x_2021 = x_2000 i_1022 in
               let x_2022 = fst x_2021 in
               let x_2023 = snd x_2021 in
               let x_2025 = x_2022 = false in
               let b_1684 = not x_2025 in
               let n_1691 = if b_1684 then
                              x_2023
                            else
                              _|_ in
               let x_2026 = n_1690 = n_1691 in
               if x_2026 then
                 ()
               else
                 let f_1692 = {fail} in
                 let x_2027 = f_1692 () in
                 x_2027
             in
             let f_1695 = rand_int in
             let x_2028 = f_1695 () in
             let f_1698 = rand_int in
             let x_2029 = f_1698 () in
             let x_2030 = main_1021 x_2028 in
             let x_2031 = x_2030 x_2029 in
             ()
sort_let_pair: let List.nth_1082 (x_1083:(int -> (bool * int))) =
                 let x_1842 (x_1084:int) = let f_1500 = rand_int in
                                           let x_1843 = f_1500 () in
                                           x_1843 in
                 let x_1846 = (x_1842, x_1083) in
                 x_1846
               in
               let rec append_1085 (x_1014:((int -> (bool * int)) * (int -> (bool * int)))) =
                 let x_1847 = fst x_1014 in
                 let x_1848 = snd x_1014 in
                 let x_1849 = x_1847 0 in
                 let x_1850 = fst x_1849 in
                 let x_1851 = snd x_1849 in
                 let x_1853 = x_1850 = false in
                 if x_1853 then
                   let x_1905 = (x_1848, x_1014) in
                   x_1905
                 else
                   let x_1854 = x_1847 0 in
                   let x_1855 = fst x_1854 in
                   let x_1856 = snd x_1854 in
                   let x_1858 = x_1855 = false in
                   let b_1512 = not x_1858 in
                   if b_1512 then
                     let xs'_1012 (x_1135:int) =
                       let n_1523 = x_1135 + 1 in
                       let x_1865 = x_1847 n_1523 in
                       let x_1866 = fst x_1865 in
                       let x_1867 = snd x_1865 in
                       x_1865
                     in
                     let x_1868 = x_1847 0 in
                     let x_1869 = fst x_1868 in
                     let x_1870 = snd x_1868 in
                     let cons_1210 (x_1206:int) (xs_1207:(int -> (bool * int))) =
                       let x_1871 (i_1205:int) =
                         let x_1873 = i_1205 = 0 in
                         if x_1873 then
                           let x_1882 = (true, x_1206) in
                           x_1882
                         else
                           let n_1539 = i_1205 - 1 in
                           let x_1876 = xs_1207 n_1539 in
                           let xs_1712 (n_1713:int) =
                             if let x_1878 = n_1713 = n_1539 in
                                x_1878 then
                               x_1876
                             else
                               let x_1877 = xs_1207 n_1713 in
                               x_1877
                           in
                           x_1876
                       in
                       let x_1885 = (x_1871, xs_1207) in
                       x_1885
                     in
                     let x_1888 = (xs'_1012, x_1848) in
                     let x_1889 = fst x_1888 in
                     let x_1890 = snd x_1888 in
                     let x_1891 = append_1085 x_1888 in
                     let x_1892 = fst x_1891 in
                     let x_1893 = snd x_1891 in
                     let x_1894 = fst x_1893 in
                     let x_1895 = snd x_1893 in
                     let x_1896 = cons_1210 x_1870 in
                     let x_1897 = x_1896 x_1892 in
                     let x_1898 = fst x_1897 in
                     let x_1899 = snd x_1897 in
                     let x_1902 = (x_1898, x_1014) in
                     x_1902
                   else
                     let x_1859 = _|_ in
                     let x_1862 = (x_1859, x_1014) in
                     x_1862
               in
               let rec make_list_1013 (n_1014:int) =
                 let x_1907 = n_1014 < 0 in
                 if x_1907 then
                   fun (x_1299:int) -> (let x_1935 = (false, 0) in
                                        x_1935)
                 else
                   let cons_1293 (x_1289:int) (xs_1290:(int -> (bool * int))) =
                     let x_1908 (i_1288:int) =
                       let x_1910 = i_1288 = 0 in
                       if x_1910 then
                         let x_1919 = (true, x_1289) in
                         x_1919
                       else
                         let n_1572 = i_1288 - 1 in
                         let x_1913 = xs_1290 n_1572 in
                         let xs_1746 (n_1747:int) =
                           if let x_1915 = n_1747 = n_1572 in
                              x_1915 then
                             x_1913
                           else
                             let x_1914 = xs_1290 n_1747 in
                             x_1914
                         in
                         x_1913
                     in
                     let x_1922 = (x_1908, xs_1290) in
                     x_1922
                   in
                   let n_1580 = n_1014 - 1 in
                   let x_1925 = make_list_1013 n_1580 in
                   let f_1574 = rand_int in
                   let x_1926 = f_1574 () in
                   let x_1927 = cons_1293 x_1926 in
                   let x_1928 = x_1927 x_1925 in
                   let x_1929 = fst x_1928 in
                   let x_1930 = snd x_1928 in
                   x_1929
               in
               let rec list_eq_1015 (xsys_1016:((int -> (bool * unit)) * (int -> (bool * unit)))) =
                 let x_1936 = fst xsys_1016 in
                 let x_1937 = snd xsys_1016 in
                 let x_1938 = x_1936 0 in
                 let x_1939 = fst x_1938 in
                 let x_1940 = snd x_1938 in
                 let x_1942 = x_1939 = false in
                 let x_1943 = x_1937 0 in
                 let x_1944 = fst x_1943 in
                 let x_1945 = snd x_1943 in
                 let x_1947 = x_1944 = false in
                 let x_1948 = x_1942 && x_1947 in
                 if x_1948 then
                   let x_1999 = (true, xsys_1016) in
                   x_1999
                 else
                   let x_1949 = x_1936 0 in
                   let x_1950 = fst x_1949 in
                   let x_1951 = snd x_1949 in
                   let x_1953 = x_1950 = false in
                   let b_1622 = not x_1953 in
                   let x_1954 = x_1937 0 in
                   let x_1955 = fst x_1954 in
                   let x_1956 = snd x_1954 in
                   let x_1958 = x_1955 = false in
                   let b_1623 = not x_1958 in
                   let x_1959 = b_1622 && b_1623 in
                   if x_1959 then
                     let x_1964 = x_1936 0 in
                     let x_1965 = fst x_1964 in
                     let x_1966 = snd x_1964 in
                     let xs'_1018 (x_1310:int) =
                       let n_1633 = x_1310 + 1 in
                       let x_1969 = x_1936 n_1633 in
                       let x_1970 = fst x_1969 in
                       let x_1971 = snd x_1969 in
                       x_1969
                     in
                     let x_1972 = x_1937 0 in
                     let x_1973 = fst x_1972 in
                     let x_1974 = snd x_1972 in
                     let ys'_1020 (x_1314:int) =
                       let n_1644 = x_1314 + 1 in
                       let x_1977 = x_1937 n_1644 in
                       let x_1978 = fst x_1977 in
                       let x_1979 = snd x_1977 in
                       x_1977
                     in
                     let x_1980 = x_1966 = x_1974 in
                     let x_1983 = (xs'_1018, ys'_1020) in
                     let x_1984 = fst x_1983 in
                     let x_1985 = snd x_1983 in
                     let x_1986 = list_eq_1015 x_1983 in
                     let x_1987 = fst x_1986 in
                     let x_1988 = snd x_1986 in
                     let x_1989 = fst x_1988 in
                     let x_1990 = snd x_1988 in
                     let x_1992 = x_1980 && x_1987 in
                     let x_1995 = (x_1992, xsys_1016) in
                     x_1995
                   else
                     let x_1963 = (false, xsys_1016) in
                     x_1963
               in
               let main_1021 (i_1022:int) (n_1023:int) =
                 let x_2000 = make_list_1013 n_1023 in
                 let f_1665 (x_1446:int) = let x_2005 = (false, 0) in
                                           x_2005 in
                 let x_2008 = (x_2000, f_1665) in
                 let x_2009 = fst x_2008 in
                 let x_2010 = snd x_2008 in
                 let x_2011 = append_1085 x_2008 in
                 let x_2012 = fst x_2011 in
                 let x_2013 = snd x_2011 in
                 let x_2014 = fst x_2013 in
                 let x_2015 = snd x_2013 in
                 let x_2016 = x_2012 i_1022 in
                 let x_2017 = fst x_2016 in
                 let x_2018 = snd x_2016 in
                 let x_2020 = x_2017 = false in
                 let b_1675 = not x_2020 in
                 let n_1690 = if b_1675 then
                                x_2018
                              else
                                _|_ in
                 let x_2021 = x_2000 i_1022 in
                 let x_2022 = fst x_2021 in
                 let x_2023 = snd x_2021 in
                 let x_2025 = x_2022 = false in
                 let b_1684 = not x_2025 in
                 let n_1691 = if b_1684 then
                                x_2023
                              else
                                _|_ in
                 let x_2026 = n_1690 = n_1691 in
                 if x_2026 then
                   ()
                 else
                   let f_1692 = {fail} in
                   let x_2027 = f_1692 () in
                   x_2027
               in
               let f_1695 = rand_int in
               let x_2028 = f_1695 () in
               let f_1698 = rand_int in
               let x_2029 = f_1698 () in
               let x_2030 = main_1021 x_2028 in
               let x_2031 = x_2030 x_2029 in
               ()
unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

int ===>
int

int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

unit ===>
unit

unit ===>
unit

int ===>
int

int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

unit ===>
unit

unit ===>
unit

int ===>
int

(int -> unit) ===>
(int -> unit)

unit ===>
unit

int ===>
int

(int -> unit) ===>
(int -> unit)

int ===>
int

(int -> int -> unit) ===>
(int -> int -> unit)

int ===>
int

int ===>
int

unit ===>
unit

unit ===>
unit

int ===>
int

(int -> unit) ===>
(int -> unit)

int ===>
int

int ===>
int

unit ===>
unit

x: x_2030, y': x_2032
THIS IS ROOT
x: main_1021, y': x_2033
THIS IS ROOT
x: f_1698, y': x_2034
THIS IS ROOT
int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

x: f_1695, y': x_2035
THIS IS ROOT
int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

int ===>
int

unit ===>
unit

(unit -> int) ===>
(unit -> int)

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

int ===>
int

(int -> int -> (bool * int)) ===>
(int -> int -> (bool * int))

int ===>
int

int ===>
int

unit ===>
unit

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int)))) ===>
((int -> (bool * int)) * (((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int)))) ===>
((int -> (bool * int)) * (((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

(((int -> (bool * int)) * (int -> (bool * int))) ->
   ((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int))))) ===>
((((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))) ->
   ((int -> (bool * int)) * (((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int)))) ===>
((int -> (bool * int)) * (((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int)))) ===>
((int -> (bool * int)) * (((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int)))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

((int -> (bool * int)) * (int -> (bool * int))) ===>
(((bool * int) * (bool * int)) -> ((bool * (bool * int)) * (bool * (bool * int))))

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

int ===>
int

int ===>
int

bool ===>
bool

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

unit ===>
unit

unit ===>
unit

unit ===>
unit

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

(int -> (bool * int)) ===>
(int -> (bool * int))

int ===>
int

int ===>
int

bool ===>
bool

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

int ===>
int

bool ===>
bool

int ===>
int

(bool * int) ===>
(bool * int)

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

unit ===>
unit

(unit -> unit) ===>
(unit -> unit)

unit ===>
unit

unit ===>
unit

unit ===>
unit

x: f_1692, y': x_2205
THIS IS ROOT
unit ===>
unit

unit ===>
unit

(unit -> unit) ===>
(unit -> unit)

unit ===>
unit

unit ===>
unit

(unit -> unit) ===>
(unit -> unit)

unit ===>
unit

bool ===>
bool

bool ===>
bool

bool ===>
bool

int ===>
int

int ===>
int

int ===>
int

int ===>
int

bool ===>
bool

int ===>
int

int ===>
int

int ===>
int

int ===>
int

bool ===>
bool

bool ===>
bool

int ===>
int

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

x: x_2000, y': i_2206
THIS IS ROOT
int ===>
int

int ===>
int

int ===>
int

int ===>
int

bool ===>
bool

bool ===>
bool

int ===>
int

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

bool ===>
bool

x: x_2012, y': i_2207
THIS IS NOT ROOT
make_tree: (x_2011:((int -> (bool * int)) * ((int -> (bool * int)) * (int -> (bool * int)))))
Fatal error: exception Option.No_value
Raised at file "option.ml", line 44, characters 17-25
Called from file "ref_trans.ml", line 135, characters 19-33
Called from file "ref_trans.ml", line 239, characters 15-43
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 238, characters 15-40
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 316, characters 15-40
Called from file "ref_trans.ml", line 272, characters 15-40
Called from file "syntax.ml", line 329, characters 39-57
Called from file "syntax.ml", line 354, characters 39-61
Called from file "ref_trans.ml", line 238, characters 15-40
Called from file "syntax.ml", line 328, characters 73-90
Called from file "extList.ml", line 102, characters 17-20
Called from file "syntax.ml", line 329, characters 16-37
Called from file "syntax.ml", line 354, characters 39-61
Called from file "syntax.ml", line 329, characters 39-57
Called from file "syntax.ml", line 354, characters 39-61
Called from file "syntax.ml", line 329, characters 39-57
Called from file "syntax.ml", line 354, characters 39-61
Called from file "syntax.ml", line 329, characters 39-57
Called from file "syntax.ml", line 354, characters 39-61
Called from file "syntax.ml", line 329, characters 39-57
Called from file "syntax.ml", line 354, characters 39-61
Called from file "main_loop.ml", line 9, characters 10-13
Called from file "util.ml", line 19, characters 15-18
Called from file "main_loop.ml", line 212, characters 35-53
Called from file "mochi.ml", line 525, characters 9-17
