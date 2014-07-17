INPUT:
let List.nth_1056 x_1057 x_1058 = rand_int () in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1113 = rand_int () in
    let xs_1114 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1113)
                   else
                     xs_1114 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  (label[IdTerm(x_1023, (xs_1011, ys_1012))]
   (if fst (xs_1011 0) = false then
      (label[IdTerm(xs_1011, (fun x_1277 -> (false, 0)))] ys_1012)
    else
      if fst (xs_1011 0) <> false then
        let xs'_1014 x_1155 = xs_1011 (x_1155 + 1) in
        let x_1013 = snd (xs_1011 0) in
        (label[IdTerm(xs_1011, (fun i_1248 -> (if i_1248 = 0 then
                                                 (true, x_1013)
                                               else
                                                 xs'_1014 (i_1248 - 1))))]
         (let xs_1233 = append_1059 (xs'_1014, ys_1012) in
          fun i_1231 -> (if i_1231 = 0 then
                           (true, x_1013)
                         else
                           xs_1233 (i_1231 - 1))))
      else
        _|_))
in
let main_1015 i_1016 n_1017 =
  let xs_1018 = make_list_1008 n_1017 in
  let ys_1019 = append_1059 (xs_1018, (fun x_1410 -> (false, 0))) in
  if (let x_1460 = ys_1019 i_1016 in
      if fst x_1460 <> false then
        snd x_1460
      else
        _|_)
     = (let x_1450 = xs_1018 i_1016 in
        if fst x_1450 <> false then
          snd x_1450
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

normalize:
let List.nth_1056 x_1057 x_1058 = let u_1465 = () in
                                  let f_1464 = rand_int in
                                  let n_1466 = f_1464 u_1465 in
                                  n_1466 in
let rec make_list_1008 n_1009 =
  let b_1467 = let n_1468 = n_1009 in
               let n_1469 = 0 in
               n_1468 < n_1469 in
  if b_1467 then
    fun x_1122 -> (let b_1470 = false in
                   let n_1471 = 0 in
                   (b_1470, n_1471))
  else
    let x_1113 = let u_1475 = () in
                 let f_1474 = rand_int in
                 let n_1476 = f_1474 u_1475 in
                 n_1476 in
    let xs_1114 =
      let n_1480 = let n_1477 = n_1009 in
                   let n_1478 = 1 in
                   n_1477 - n_1478 in
      let make_list_1479 = make_list_1008 in
      let f_1481 = make_list_1479 n_1480 in
      f_1481
    in
    fun i_1112 ->
      (let b_1482 = let i_1483 = i_1112 in
                    let n_1484 = 0 in
                    i_1483 = n_1484 in
       if b_1482 then
         let b_1485 = true in
         let x_1486 = x_1113 in
         (b_1485, x_1486)
       else
         let n_1492 = let i_1489 = i_1112 in
                      let n_1490 = 1 in
                      i_1489 - n_1490 in
         let xs_1491 = xs_1114 in
         let p_1493 = xs_1491 n_1492 in
         p_1493)
in
let rec append_1059 x_1023 =
  let xs_1011 = let x_1495 = x_1023 in
                fst x_1495 in
  let ys_1012 = let x_1496 = x_1023 in
                snd x_1496 in
  (label[IdTerm(x_1023, (let xs_1558 = xs_1011 in
                         let ys_1559 = ys_1012 in
                         (xs_1558, ys_1559)))]
   (let b_1497 =
      let b_1502 =
        let p_1501 = let n_1499 = 0 in
                     let xs_1498 = xs_1011 in
                     let p_1500 = xs_1498 n_1499 in
                     p_1500 in
        fst p_1501
      in
      let b_1503 = false in
      b_1502 = b_1503
    in
    if b_1497 then
      (label[IdTerm(xs_1011, (fun x_1277 -> (let b_1504 = false in
                                             let n_1505 = 0 in
                                             (b_1504, n_1505))))] ys_1012)
    else
      let b_1508 =
        let b_1515 =
          let b_1513 =
            let p_1512 = let n_1510 = 0 in
                         let xs_1509 = xs_1011 in
                         let p_1511 = xs_1509 n_1510 in
                         p_1511 in
            fst p_1512
          in
          let b_1514 = false in
          b_1513 = b_1514
        in
        not b_1515
      in
      if b_1508 then
        let xs'_1014 x_1155 =
          let n_1519 = let x_1516 = x_1155 in
                       let n_1517 = 1 in
                       x_1516 + n_1517 in
          let xs_1518 = xs_1011 in
          let p_1520 = xs_1518 n_1519 in
          p_1520
        in
        let x_1013 =
          let p_1524 = let n_1522 = 0 in
                       let xs_1521 = xs_1011 in
                       let p_1523 = xs_1521 n_1522 in
                       p_1523 in
          snd p_1524
        in
        (label[IdTerm(xs_1011,
               (fun i_1248 ->
                  (let b_1544 = let i_1545 = i_1248 in
                                let n_1546 = 0 in
                                i_1545 = n_1546 in
                   if b_1544 then
                     let b_1547 = true in
                     let x_1548 = x_1013 in
                     (b_1547, x_1548)
                   else
                     let n_1554 = let i_1551 = i_1248 in
                                  let n_1552 = 1 in
                                  i_1551 - n_1552 in
                     let xs'_1553 = xs'_1014 in
                     let p_1555 = xs'_1553 n_1554 in
                     p_1555)))]
         (let xs_1233 =
            let p_1530 = let xs'_1525 = xs'_1014 in
                         let ys_1526 = ys_1012 in
                         (xs'_1525, ys_1526) in
            let append_1529 = append_1059 in
            let f_1531 = append_1529 p_1530 in
            f_1531
          in
          fun i_1231 ->
            (let b_1532 = let i_1533 = i_1231 in
                          let n_1534 = 0 in
                          i_1533 = n_1534 in
             if b_1532 then
               let b_1535 = true in
               let x_1536 = x_1013 in
               (b_1535, x_1536)
             else
               let n_1542 = let i_1539 = i_1231 in
                            let n_1540 = 1 in
                            i_1539 - n_1540 in
               let xs_1541 = xs_1233 in
               let p_1543 = xs_1541 n_1542 in
               p_1543)))
      else
        _|_))
in
let main_1015 i_1016 n_1017 =
  let xs_1018 =
    let n_1563 = n_1017 in
    let make_list_1562 = make_list_1008 in
    let f_1564 = make_list_1562 n_1563 in
    f_1564
  in
  let ys_1019 =
    let p_1574 =
      let xs_1565 = xs_1018 in
      let f_1570 = fun x_1410 -> (let b_1566 = false in
                                  let n_1567 = 0 in
                                  (b_1566, n_1567)) in
      (xs_1565, f_1570)
    in
    let append_1573 = append_1059 in
    let f_1575 = append_1573 p_1574 in
    f_1575
  in
  let b_1576 =
    let n_1595 =
      let x_1460 = let i_1578 = i_1016 in
                   let ys_1577 = ys_1019 in
                   let p_1579 = ys_1577 i_1578 in
                   p_1579 in
      let b_1580 =
        let b_1584 = let b_1582 = let x_1581 = x_1460 in
                                  fst x_1581 in
                     let b_1583 = false in
                     b_1582 = b_1583 in
        not b_1584
      in
      if b_1580 then
        let x_1585 = x_1460 in
        snd x_1585
      else
        _|_
    in
    let n_1596 =
      let x_1450 = let i_1587 = i_1016 in
                   let xs_1586 = xs_1018 in
                   let p_1588 = xs_1586 i_1587 in
                   p_1588 in
      let b_1589 =
        let b_1593 = let b_1591 = let x_1590 = x_1450 in
                                  fst x_1590 in
                     let b_1592 = false in
                     b_1591 = b_1592 in
        not b_1593
      in
      if b_1589 then
        let x_1594 = x_1450 in
        snd x_1594
      else
        _|_
    in
    n_1595 = n_1596
  in
  if b_1576 then
    ()
  else
    let u_1598 = () in
    let f_1597 = {fail} in
    let u_1599 = f_1597 u_1598 in
    u_1599
in
let main_1055 =
  let arg1_1051 = let u_1601 = () in
                  let f_1600 = rand_int in
                  let n_1602 = f_1600 u_1601 in
                  n_1602 in
  let arg2_1053 = let u_1604 = () in
                  let f_1603 = rand_int in
                  let n_1605 = f_1603 u_1604 in
                  n_1605 in
  let arg2_1608 = arg2_1053 in
  let arg1_1607 = arg1_1051 in
  let main_1606 = main_1015 in
  let f_1609 = main_1606 arg1_1607 in
  let u_1610 = f_1609 arg2_1608 in
  u_1610
in
()

inline_var_const:
let List.nth_1056 x_1057 x_1058 = let f_1464 = rand_int in
                                  let n_1466 = f_1464 () in
                                  n_1466 in
let rec make_list_1008 n_1009 =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun x_1122 -> (false, 0)
  else
    let x_1113 = let f_1474 = rand_int in
                 let n_1476 = f_1474 () in
                 n_1476 in
    let xs_1114 = let n_1480 = n_1009 - 1 in
                  let f_1481 = make_list_1008 n_1480 in
                  f_1481 in
    fun i_1112 ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, x_1113)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 x_1023 =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  (label[IdTerm(x_1023, (xs_1011, ys_1012))]
   (let b_1497 = let b_1502 = let p_1501 = let p_1500 = xs_1011 0 in
                                           p_1500 in
                              fst p_1501 in
                 b_1502 = false in
    if b_1497 then
      (label[IdTerm(xs_1011, (fun x_1277 -> (false, 0)))] ys_1012)
    else
      let b_1508 =
        let b_1515 = let b_1513 = let p_1512 = let p_1511 = xs_1011 0 in
                                               p_1511 in
                                  fst p_1512 in
                     b_1513 = false in
        not b_1515
      in
      if b_1508 then
        let xs'_1014 x_1155 = let n_1519 = x_1155 + 1 in
                              let p_1520 = xs_1011 n_1519 in
                              p_1520 in
        let x_1013 = let p_1524 = let p_1523 = xs_1011 0 in
                                  p_1523 in
                     snd p_1524 in
        (label[IdTerm(xs_1011,
               (fun i_1248 ->
                  (let b_1544 = i_1248 = 0 in
                   if b_1544 then
                     (true, x_1013)
                   else
                     let n_1554 = i_1248 - 1 in
                     let p_1555 = xs'_1014 n_1554 in
                     p_1555)))]
         (let xs_1233 = let p_1530 = (xs'_1014, ys_1012) in
                        let f_1531 = append_1059 p_1530 in
                        f_1531 in
          fun i_1231 ->
            (let b_1532 = i_1231 = 0 in
             if b_1532 then
               (true, x_1013)
             else
               let n_1542 = i_1231 - 1 in
               let p_1543 = xs_1233 n_1542 in
               p_1543)))
      else
        _|_))
in
let main_1015 i_1016 n_1017 =
  let xs_1018 = let f_1564 = make_list_1008 n_1017 in
                f_1564 in
  let ys_1019 =
    let p_1574 = let f_1570 x_1410 = (false, 0) in
                 (xs_1018, f_1570) in
    let f_1575 = append_1059 p_1574 in
    f_1575
  in
  let b_1576 =
    let n_1595 =
      let x_1460 = let p_1579 = ys_1019 i_1016 in
                   p_1579 in
      let b_1580 = let b_1584 = let b_1582 = fst x_1460 in
                                b_1582 = false in
                   not b_1584 in
      if b_1580 then
        snd x_1460
      else
        _|_
    in
    let n_1596 =
      let x_1450 = let p_1588 = xs_1018 i_1016 in
                   p_1588 in
      let b_1589 = let b_1593 = let b_1591 = fst x_1450 in
                                b_1591 = false in
                   not b_1593 in
      if b_1589 then
        snd x_1450
      else
        _|_
    in
    n_1595 = n_1596
  in
  if b_1576 then
    ()
  else
    let f_1597 = {fail} in
    let u_1599 = f_1597 () in
    u_1599
in
let main_1055 =
  let arg1_1051 = let f_1600 = rand_int in
                  let n_1602 = f_1600 () in
                  n_1602 in
  let arg2_1053 = let f_1603 = rand_int in
                  let n_1605 = f_1603 () in
                  n_1605 in
  let f_1609 = main_1015 arg1_1051 in
  let u_1610 = f_1609 arg2_1053 in
  u_1610
in
()

flatten_let:
let List.nth_1056 x_1057 x_1058 = let f_1464 = rand_int in
                                  let n_1466 = f_1464 () in
                                  n_1466 in
let rec make_list_1008 n_1009 =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun x_1122 -> (false, 0)
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let x_1113 = n_1476 in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    let xs_1114 = f_1481 in
    fun i_1112 ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, x_1113)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 x_1023 =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  (label[IdTerm(x_1023, (xs_1011, ys_1012))]
   (let p_1500 = xs_1011 0 in
    let p_1501 = p_1500 in
    let b_1502 = fst p_1501 in
    let b_1497 = b_1502 = false in
    if b_1497 then
      (label[IdTerm(xs_1011, (fun x_1277 -> (false, 0)))] ys_1012)
    else
      let p_1511 = xs_1011 0 in
      let p_1512 = p_1511 in
      let b_1513 = fst p_1512 in
      let b_1515 = b_1513 = false in
      let b_1508 = not b_1515 in
      if b_1508 then
        let xs'_1014 x_1155 = let n_1519 = x_1155 + 1 in
                              let p_1520 = xs_1011 n_1519 in
                              p_1520 in
        let p_1523 = xs_1011 0 in
        let p_1524 = p_1523 in
        let x_1013 = snd p_1524 in
        (label[IdTerm(xs_1011,
               (fun i_1248 ->
                  (let b_1544 = i_1248 = 0 in
                   if b_1544 then
                     (true, x_1013)
                   else
                     let n_1554 = i_1248 - 1 in
                     let p_1555 = xs'_1014 n_1554 in
                     p_1555)))]
         (let p_1530 = (xs'_1014, ys_1012) in
          let f_1531 = append_1059 p_1530 in
          let xs_1233 = f_1531 in
          fun i_1231 ->
            (let b_1532 = i_1231 = 0 in
             if b_1532 then
               (true, x_1013)
             else
               let n_1542 = i_1231 - 1 in
               let p_1543 = xs_1233 n_1542 in
               p_1543)))
      else
        _|_))
in
let main_1015 i_1016 n_1017 =
  let f_1564 = make_list_1008 n_1017 in
  let xs_1018 = f_1564 in
  let f_1570 x_1410 = (false, 0) in
  let p_1574 = (xs_1018, f_1570) in
  let f_1575 = append_1059 p_1574 in
  let ys_1019 = f_1575 in
  let p_1579 = ys_1019 i_1016 in
  let x_1460 = p_1579 in
  let b_1582 = fst x_1460 in
  let b_1584 = b_1582 = false in
  let b_1580 = not b_1584 in
  let n_1595 = if b_1580 then
                 snd x_1460
               else
                 _|_ in
  let p_1588 = xs_1018 i_1016 in
  let x_1450 = p_1588 in
  let b_1591 = fst x_1450 in
  let b_1593 = b_1591 = false in
  let b_1589 = not b_1593 in
  let n_1596 = if b_1589 then
                 snd x_1450
               else
                 _|_ in
  let b_1576 = n_1595 = n_1596 in
  if b_1576 then
    ()
  else
    let f_1597 = {fail} in
    let u_1599 = f_1597 () in
    u_1599
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let arg1_1051 = n_1602 in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let arg2_1053 = n_1605 in
let f_1609 = main_1015 arg1_1051 in
let u_1610 = f_1609 arg2_1053 in
let main_1055 = u_1610 in
()

add_proj_info:
let List.nth_1056 x_1057 x_1058 = let f_1464 = rand_int in
                                  let n_1466 = f_1464 () in
                                  n_1466 in
let rec make_list_1008 n_1009 =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun x_1122 -> (false, 0)
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let x_1113 = n_1476 in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    let xs_1114 = f_1481 in
    fun i_1112 ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, x_1113)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 x_1023 =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  (label[IdTerm(x_1023, (xs_1011, ys_1012))]
   (let p_1500 = xs_1011 0 in
    let p_1501 = p_1500 in
    let b_1502 = fst p_1501 in
    let b_1497 = b_1502 = false in
    if b_1497 then
      (label[IdTerm(xs_1011, (fun x_1277 -> (false, 0)))] ys_1012)
    else
      let p_1511 = xs_1011 0 in
      let p_1512 = p_1511 in
      let b_1513 = fst p_1512 in
      let b_1515 = b_1513 = false in
      let b_1508 = not b_1515 in
      if b_1508 then
        let xs'_1014 x_1155 = let n_1519 = x_1155 + 1 in
                              let p_1520 = xs_1011 n_1519 in
                              p_1520 in
        let p_1523 = xs_1011 0 in
        let p_1524 = p_1523 in
        let x_1013 = snd p_1524 in
        (label[IdTerm(xs_1011,
               (fun i_1248 ->
                  (let b_1544 = i_1248 = 0 in
                   if b_1544 then
                     (true, x_1013)
                   else
                     let n_1554 = i_1248 - 1 in
                     let p_1555 = xs'_1014 n_1554 in
                     p_1555)))]
         (let p_1530 = (xs'_1014, ys_1012) in
          (label[IdTerm(xs'_1014, (fst p_1530))]
           (label[IdTerm(ys_1012, (snd p_1530))]
            (let f_1531 = append_1059 p_1530 in
             let xs_1233 = f_1531 in
             fun i_1231 ->
               (let b_1532 = i_1231 = 0 in
                if b_1532 then
                  (true, x_1013)
                else
                  let n_1542 = i_1231 - 1 in
                  let p_1543 = xs_1233 n_1542 in
                  p_1543))))))
      else
        _|_))
in
let main_1015 i_1016 n_1017 =
  let f_1564 = make_list_1008 n_1017 in
  let xs_1018 = f_1564 in
  let f_1570 x_1410 = (false, 0) in
  let p_1574 = (xs_1018, f_1570) in
  (label[IdTerm(xs_1018, (fst p_1574))]
   (label[IdTerm(f_1570, (snd p_1574))]
    (let f_1575 = append_1059 p_1574 in
     let ys_1019 = f_1575 in
     let p_1579 = ys_1019 i_1016 in
     let x_1460 = p_1579 in
     let b_1582 = fst x_1460 in
     let b_1584 = b_1582 = false in
     let b_1580 = not b_1584 in
     let n_1595 = if b_1580 then
                    snd x_1460
                  else
                    _|_ in
     let p_1588 = xs_1018 i_1016 in
     let x_1450 = p_1588 in
     let b_1591 = fst x_1450 in
     let b_1593 = b_1591 = false in
     let b_1589 = not b_1593 in
     let n_1596 = if b_1589 then
                    snd x_1450
                  else
                    _|_ in
     let b_1576 = n_1595 = n_1596 in
     if b_1576 then
       ()
     else
       let f_1597 = {fail} in
       let u_1599 = f_1597 () in
       u_1599)))
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let arg1_1051 = n_1602 in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let arg2_1053 = n_1605 in
let f_1609 = main_1015 arg1_1051 in
let u_1610 = f_1609 arg2_1053 in
let main_1055 = u_1610 in
()

ret_fun:
let List.nth_1056 (x_1057:(int -> (bool * int))) =
  ((fun (x_1058:int) -> (let f_1464 = rand_int in
                         let n_1466 = f_1464 () in
                         n_1466)), x_1057)
in
let rec make_list_1008 (n_1009:int) =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun (x_1122:int) -> (false, 0)
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let x_1113 = n_1476 in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    let xs_1114 = f_1481 in
    fun (i_1112:int) ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, x_1113)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  (label[IdTerm(x_1023, (xs_1011, ys_1012))]
   (let p_1500 = xs_1011 0 in
    let p_1501 = p_1500 in
    let b_1502 = fst p_1501 in
    let b_1497 = b_1502 = false in
    if b_1497 then
      (label[IdTerm(xs_1011, (fun x_1277 -> (false, 0)))] (ys_1012, x_1023))
    else
      let p_1511 = xs_1011 0 in
      let p_1512 = p_1511 in
      let b_1513 = fst p_1512 in
      let b_1515 = b_1513 = false in
      let b_1508 = not b_1515 in
      if b_1508 then
        let xs'_1014 (x_1155:int) = let n_1519 = x_1155 + 1 in
                                    let p_1520 = xs_1011 n_1519 in
                                    p_1520 in
        let p_1523 = xs_1011 0 in
        let p_1524 = p_1523 in
        let x_1013 = snd p_1524 in
        (label[IdTerm(xs_1011,
               (fun i_1248 ->
                  (let b_1544 = i_1248 = 0 in
                   if b_1544 then
                     (true, x_1013)
                   else
                     let n_1554 = i_1248 - 1 in
                     let p_1555 = xs'_1014 n_1554 in
                     p_1555)))]
         (let p_1530 = (xs'_1014, ys_1012) in
          (label[IdTerm(xs'_1014, (fst p_1530))]
           (label[IdTerm(ys_1012, (snd p_1530))]
            (let p_1616 = append_1059 p_1530 in
             let f_1531 = fst p_1616 in
             let p_1617 = snd p_1616 in
             (label[IdTerm(p_1530, p_1617)]
              (let xs_1233 = f_1531 in
               ((fun (i_1231:int) ->
                   (let b_1532 = i_1231 = 0 in
                    if b_1532 then
                      (true, x_1013)
                    else
                      let n_1542 = i_1231 - 1 in
                      let p_1543 = xs_1233 n_1542 in
                      p_1543)),
                x_1023))))))))
      else
        (_|_, x_1023)))
in
let main_1015 (i_1016:int) (n_1017:int) =
  let f_1564 = make_list_1008 n_1017 in
  let xs_1018 = f_1564 in
  let f_1570 (x_1410:int) = (false, 0) in
  let p_1574 = (xs_1018, f_1570) in
  (label[IdTerm(xs_1018, (fst p_1574))]
   (label[IdTerm(f_1570, (snd p_1574))]
    (let p_1631 = append_1059 p_1574 in
     let f_1575 = fst p_1631 in
     let p_1632 = snd p_1631 in
     (label[IdTerm(p_1574, p_1632)]
      (let ys_1019 = f_1575 in
       let p_1579 = ys_1019 i_1016 in
       let x_1460 = p_1579 in
       let b_1582 = fst x_1460 in
       let b_1584 = b_1582 = false in
       let b_1580 = not b_1584 in
       let n_1595 = if b_1580 then
                      snd x_1460
                    else
                      _|_ in
       let p_1588 = xs_1018 i_1016 in
       let x_1450 = p_1588 in
       let b_1591 = fst x_1450 in
       let b_1593 = b_1591 = false in
       let b_1589 = not b_1593 in
       let n_1596 = if b_1589 then
                      snd x_1450
                    else
                      _|_ in
       let b_1576 = n_1595 = n_1596 in
       if b_1576 then
         ()
       else
         let f_1597 = {fail} in
         let u_1599 = f_1597 () in
         u_1599)))))
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let arg1_1051 = n_1602 in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let arg2_1053 = n_1605 in
let f_1609 = main_1015 arg1_1051 in
let u_1610 = f_1609 arg2_1053 in
let main_1055 = u_1610 in
()

remove_label:
let List.nth_1056 (x_1057:(int -> (bool * int))) =
  ((fun (x_1058:int) -> (let f_1464 = rand_int in
                         let n_1466 = f_1464 () in
                         n_1466)), x_1057)
in
let rec make_list_1008 (n_1009:int) =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun (x_1122:int) -> (false, 0)
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let x_1113 = n_1476 in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    let xs_1114 = f_1481 in
    fun (i_1112:int) ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, x_1113)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  let p_1500 = xs_1011 0 in
  let p_1501 = p_1500 in
  let b_1502 = fst p_1501 in
  let b_1497 = b_1502 = false in
  if b_1497 then
    (ys_1012, ((fun (x_1277:int) -> (false, 0)), ys_1012))
  else
    let p_1511 = xs_1011 0 in
    let p_1512 = p_1511 in
    let b_1513 = fst p_1512 in
    let b_1515 = b_1513 = false in
    let b_1508 = not b_1515 in
    if b_1508 then
      let xs'_1014 (x_1155:int) = let n_1519 = x_1155 + 1 in
                                  let p_1520 = xs_1011 n_1519 in
                                  p_1520 in
      let p_1523 = xs_1011 0 in
      let p_1524 = p_1523 in
      let x_1013 = snd p_1524 in
      let p_1530 = (xs'_1014, ys_1012) in
      let p_1616 = append_1059 p_1530 in
      let f_1531 = fst p_1616 in
      let p_1617 = snd p_1616 in
      let xs_1233 = f_1531 in
      ((fun (i_1231:int) ->
          (let b_1532 = i_1231 = 0 in
           if b_1532 then
             (true, x_1013)
           else
             let n_1542 = i_1231 - 1 in
             let p_1543 = xs_1233 n_1542 in
             p_1543)),
       ((fun (i_1248:int) ->
           (let b_1544 = i_1248 = 0 in
            if b_1544 then
              (true, x_1013)
            else
              let n_1554 = i_1248 - 1 in
              let p_1555 = (fst p_1617) n_1554 in
              p_1555)),
        snd p_1617))
    else
      (_|_, (xs_1011, ys_1012))
in
let main_1015 (i_1016:int) (n_1017:int) =
  let f_1564 = make_list_1008 n_1017 in
  let xs_1018 = f_1564 in
  let f_1570 (x_1410:int) = (false, 0) in
  let p_1574 = (xs_1018, f_1570) in
  let p_1631 = append_1059 p_1574 in
  let f_1575 = fst p_1631 in
  let p_1632 = snd p_1631 in
  let ys_1019 = f_1575 in
  let p_1579 = ys_1019 i_1016 in
  let x_1460 = p_1579 in
  let b_1582 = fst x_1460 in
  let b_1584 = b_1582 = false in
  let b_1580 = not b_1584 in
  let n_1595 = if b_1580 then
                 snd x_1460
               else
                 _|_ in
  let p_1588 = (fst p_1632) i_1016 in
  let x_1450 = p_1588 in
  let b_1591 = fst x_1450 in
  let b_1593 = b_1591 = false in
  let b_1589 = not b_1593 in
  let n_1596 = if b_1589 then
                 snd x_1450
               else
                 _|_ in
  let b_1576 = n_1595 = n_1596 in
  if b_1576 then
    ()
  else
    let f_1597 = {fail} in
    let u_1599 = f_1597 () in
    u_1599
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let arg1_1051 = n_1602 in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let arg2_1053 = n_1605 in
let f_1609 = main_1015 arg1_1051 in
let u_1610 = f_1609 arg2_1053 in
let main_1055 = u_1610 in
()

flatten_tuple:
let List.nth_1056 (x_1057:(int -> (bool * int))) =
  let x_1633 = fun (x_1058:int) -> (let f_1464 = rand_int in
                                    let n_1466 = f_1464 () in
                                    n_1466) in
  let x_1634 = x_1057 in
  let x_1636 = x_1634 in
  let x_1635 = x_1633 in
  (x_1635, x_1636)
in
let rec make_list_1008 (n_1009:int) =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun (x_1122:int) ->
      (let x_1645 = false in
       let x_1646 = 0 in
       let x_1648 = x_1646 in
       let x_1647 = x_1645 in
       (x_1647, x_1648))
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let x_1113 = n_1476 in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    let xs_1114 = f_1481 in
    fun (i_1112:int) ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         let x_1639 = true in
         let x_1640 = x_1113 in
         let x_1642 = x_1640 in
         let x_1641 = x_1639 in
         (x_1641, x_1642)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = xs_1114 n_1492 in
         p_1493)
in
let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = let x_1651 = x_1023 in
                fst x_1651 in
  let ys_1012 = let x_1652 = x_1023 in
                snd x_1652 in
  let p_1500 = xs_1011 0 in
  let p_1501 = p_1500 in
  let b_1502 = let x_1653 = p_1501 in
               fst x_1653 in
  let b_1497 = b_1502 = false in
  if b_1497 then
    let x_1720 = ys_1012 in
    let x_1721 =
      let x_1714 =
        fun (x_1277:int) ->
          (let x_1708 = false in
           let x_1709 = 0 in
           let x_1711 = x_1709 in
           let x_1710 = x_1708 in
           (x_1710, x_1711))
      in
      let x_1715 = ys_1012 in
      let x_1717 = x_1715 in
      let x_1716 = x_1714 in
      (x_1716, x_1717)
    in
    let x_1723 = fst x_1721 in
    let x_1724 = snd x_1721 in
    let x_1722 = x_1720 in
    (x_1722, x_1723, x_1724)
  else
    let p_1511 = xs_1011 0 in
    let p_1512 = p_1511 in
    let b_1513 = let x_1654 = p_1512 in
                 fst x_1654 in
    let b_1515 = b_1513 = false in
    let b_1508 = not b_1515 in
    if b_1508 then
      let xs'_1014 (x_1155:int) = let n_1519 = x_1155 + 1 in
                                  let p_1520 = xs_1011 n_1519 in
                                  p_1520 in
      let p_1523 = xs_1011 0 in
      let p_1524 = p_1523 in
      let x_1013 = let x_1669 = p_1524 in
                   snd x_1669 in
      let p_1530 =
        let x_1670 = xs'_1014 in
        let x_1671 = ys_1012 in
        let x_1673 = x_1671 in
        let x_1672 = x_1670 in
        (x_1672, x_1673)
      in
      let p_1616 = append_1059 p_1530 in
      let f_1531 = let x_1676 = p_1616 in
                   #0 x_1676 in
      let p_1617 = let x_1677 = p_1616 in
                   (#1 x_1677, #2 x_1677) in
      let xs_1233 = f_1531 in
      let x_1700 =
        fun (i_1231:int) ->
          (let b_1532 = i_1231 = 0 in
           if b_1532 then
             let x_1680 = true in
             let x_1681 = x_1013 in
             let x_1683 = x_1681 in
             let x_1682 = x_1680 in
             (x_1682, x_1683)
           else
             let n_1542 = i_1231 - 1 in
             let p_1543 = xs_1233 n_1542 in
             p_1543)
      in
      let x_1701 =
        let x_1694 =
          fun (i_1248:int) ->
            (let b_1544 = i_1248 = 0 in
             if b_1544 then
               let x_1687 = true in
               let x_1688 = x_1013 in
               let x_1690 = x_1688 in
               let x_1689 = x_1687 in
               (x_1689, x_1690)
             else
               let n_1554 = i_1248 - 1 in
               let p_1555 = (let x_1686 = p_1617 in
                             fst x_1686) n_1554 in
               p_1555)
        in
        let x_1695 = let x_1693 = p_1617 in
                     snd x_1693 in
        let x_1697 = x_1695 in
        let x_1696 = x_1694 in
        (x_1696, x_1697)
      in
      let x_1703 = fst x_1701 in
      let x_1704 = snd x_1701 in
      let x_1702 = x_1700 in
      (x_1702, x_1703, x_1704)
    else
      let x_1661 = _|_ in
      let x_1662 =
        let x_1655 = xs_1011 in
        let x_1656 = ys_1012 in
        let x_1658 = x_1656 in
        let x_1657 = x_1655 in
        (x_1657, x_1658)
      in
      let x_1664 = fst x_1662 in
      let x_1665 = snd x_1662 in
      let x_1663 = x_1661 in
      (x_1663, x_1664, x_1665)
in
let main_1015 (i_1016:int) (n_1017:int) =
  let f_1564 = make_list_1008 n_1017 in
  let xs_1018 = f_1564 in
  let f_1570 (x_1410:int) =
    let x_1728 = false in
    let x_1729 = 0 in
    let x_1731 = x_1729 in
    let x_1730 = x_1728 in
    (x_1730, x_1731)
  in
  let p_1574 =
    let x_1734 = xs_1018 in
    let x_1735 = f_1570 in
    let x_1737 = x_1735 in
    let x_1736 = x_1734 in
    (x_1736, x_1737)
  in
  let p_1631 = append_1059 p_1574 in
  let f_1575 = let x_1740 = p_1631 in
               #0 x_1740 in
  let p_1632 = let x_1741 = p_1631 in
               (#1 x_1741, #2 x_1741) in
  let ys_1019 = f_1575 in
  let p_1579 = ys_1019 i_1016 in
  let x_1460 = p_1579 in
  let b_1582 = let x_1744 = x_1460 in
               fst x_1744 in
  let b_1584 = b_1582 = false in
  let b_1580 = not b_1584 in
  let n_1595 = if b_1580 then
                 let x_1745 = x_1460 in
                 snd x_1745
               else
                 _|_ in
  let p_1588 = (let x_1746 = p_1632 in
                fst x_1746) i_1016 in
  let x_1450 = p_1588 in
  let b_1591 = let x_1747 = x_1450 in
               fst x_1747 in
  let b_1593 = b_1591 = false in
  let b_1589 = not b_1593 in
  let n_1596 = if b_1589 then
                 let x_1748 = x_1450 in
                 snd x_1748
               else
                 _|_ in
  let b_1576 = n_1595 = n_1596 in
  if b_1576 then
    ()
  else
    let f_1597 = {fail} in
    let u_1599 = f_1597 () in
    u_1599
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let arg1_1051 = n_1602 in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let arg2_1053 = n_1605 in
let f_1609 = main_1015 arg1_1051 in
let u_1610 = f_1609 arg2_1053 in
let main_1055 = u_1610 in
()

inline_var_const:
let List.nth_1056 (x_1057:(int -> (bool * int))) =
  let x_1633 = fun (x_1058:int) -> (let f_1464 = rand_int in
                                    let n_1466 = f_1464 () in
                                    n_1466) in
  (x_1633, x_1057)
in
let rec make_list_1008 (n_1009:int) =
  let b_1467 = n_1009 < 0 in
  if b_1467 then
    fun (x_1122:int) -> (false, 0)
  else
    let f_1474 = rand_int in
    let n_1476 = f_1474 () in
    let n_1480 = n_1009 - 1 in
    let f_1481 = make_list_1008 n_1480 in
    fun (i_1112:int) ->
      (let b_1482 = i_1112 = 0 in
       if b_1482 then
         (true, n_1476)
       else
         let n_1492 = i_1112 - 1 in
         let p_1493 = f_1481 n_1492 in
         p_1493)
in
let rec append_1059 (x_1023:((int -> (bool * int)) * (int -> (bool * int)))) =
  let xs_1011 = fst x_1023 in
  let ys_1012 = snd x_1023 in
  let p_1500 = xs_1011 0 in
  let b_1502 = fst p_1500 in
  let b_1497 = b_1502 = false in
  if b_1497 then
    let x_1721 = let x_1714 (x_1277:int) = (false, 0) in
                 (x_1714, ys_1012) in
    let x_1723 = fst x_1721 in
    let x_1724 = snd x_1721 in
    (ys_1012, x_1723, x_1724)
  else
    let p_1511 = xs_1011 0 in
    let b_1513 = fst p_1511 in
    let b_1515 = b_1513 = false in
    let b_1508 = not b_1515 in
    if b_1508 then
      let xs'_1014 (x_1155:int) = let n_1519 = x_1155 + 1 in
                                  let p_1520 = xs_1011 n_1519 in
                                  p_1520 in
      let p_1523 = xs_1011 0 in
      let x_1013 = snd p_1523 in
      let p_1530 = (xs'_1014, ys_1012) in
      let p_1616 = append_1059 p_1530 in
      let f_1531 = #0 p_1616 in
      let p_1617 = (#1 p_1616, #2 p_1616) in
      let x_1700 (i_1231:int) =
        let b_1532 = i_1231 = 0 in
        if b_1532 then
          (true, x_1013)
        else
          let n_1542 = i_1231 - 1 in
          let p_1543 = f_1531 n_1542 in
          p_1543
      in
      let x_1701 =
        let x_1694 (i_1248:int) =
          let b_1544 = i_1248 = 0 in
          if b_1544 then
            (true, x_1013)
          else
            let n_1554 = i_1248 - 1 in
            let p_1555 = (fst p_1617) n_1554 in
            p_1555
        in
        let x_1695 = snd p_1617 in
        (x_1694, x_1695)
      in
      let x_1703 = fst x_1701 in
      let x_1704 = snd x_1701 in
      (x_1700, x_1703, x_1704)
    else
      let x_1661 = _|_ in
      let x_1662 = (xs_1011, ys_1012) in
      let x_1664 = fst x_1662 in
      let x_1665 = snd x_1662 in
      (x_1661, x_1664, x_1665)
in
let main_1015 (i_1016:int) (n_1017:int) =
  let f_1564 = make_list_1008 n_1017 in
  let f_1570 (x_1410:int) = (false, 0) in
  let p_1574 = (f_1564, f_1570) in
  let p_1631 = append_1059 p_1574 in
  let f_1575 = #0 p_1631 in
  let p_1632 = (#1 p_1631, #2 p_1631) in
  let p_1579 = f_1575 i_1016 in
  let b_1582 = fst p_1579 in
  let b_1584 = b_1582 = false in
  let b_1580 = not b_1584 in
  let n_1595 = if b_1580 then
                 snd p_1579
               else
                 _|_ in
  let p_1588 = (fst p_1632) i_1016 in
  let b_1591 = fst p_1588 in
  let b_1593 = b_1591 = false in
  let b_1589 = not b_1593 in
  let n_1596 = if b_1589 then
                 snd p_1588
               else
                 _|_ in
  let b_1576 = n_1595 = n_1596 in
  if b_1576 then
    ()
  else
    let f_1597 = {fail} in
    let u_1599 = f_1597 () in
    u_1599
in
let f_1600 = rand_int in
let n_1602 = f_1600 () in
let f_1603 = rand_int in
let n_1605 = f_1603 () in
let f_1609 = main_1015 n_1602 in
let u_1610 = f_1609 n_1605 in
()

inline_wrapped:
let List.nth_1056 x_1057 =
  let x_1633 x_1058 = rand_int () in
  let x_1788 xi_3594 =
    if fst (fst xi_3594) = false then
      if fst (snd xi_3594) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3594))))
    else
      if fst (snd xi_3594) = false then
        ((true, x_1633 (snd (fst xi_3594))), (false, (true, 0)))
      else
        ((true, x_1633 (snd (fst xi_3594))), (true, x_1057 (snd (snd xi_3594))))
  in
  x_1788
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_1811 = let x_3466 = x_1023 ((true, 0), (false, 0)) in
               snd (fst x_3466) in
  let x_1813 = snd x_1811 in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_1816 = let x_3242 = x_1023 ((true, 0), (false, 0)) in
                 snd (fst x_3242) in
    let x_1818 = snd x_1816 in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_1827 = let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
                     snd (fst x_3221) in
        let x_1828 = fst x_1827 in
        let x_1829 = snd x_1827 in
        x_1827
      in
      let x_1830 = let x_3200 = x_1023 ((true, 0), (false, 0)) in
                   snd (fst x_3200) in
      let x_1831 = fst x_1830 in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_1851 = let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
                       snd (#0 x_3060) in
          let x_1852 = fst x_1851 in
          let x_1853 = snd x_1851 in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_1862 = let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
                       snd (fst x_3030) in
          let x_1863 = fst x_1862 in
          let x_1864 = snd x_1862 in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_1912 = let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
               snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
               snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

flatten_let:
let List.nth_1056 x_1057 =
  let x_1633 x_1058 = rand_int () in
  let x_1788 xi_3594 =
    if fst (fst xi_3594) = false then
      if fst (snd xi_3594) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3594))))
    else
      if fst (snd xi_3594) = false then
        ((true, x_1633 (snd (fst xi_3594))), (false, (true, 0)))
      else
        ((true, x_1633 (snd (fst xi_3594))), (true, x_1057 (snd (snd xi_3594))))
  in
  x_1788
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  let x_1813 = snd x_1811 in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_3242 = x_1023 ((true, 0), (false, 0)) in
    let x_1816 = snd (fst x_3242) in
    let x_1818 = snd x_1816 in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        let x_1828 = fst x_1827 in
        let x_1829 = snd x_1827 in
        x_1827
      in
      let x_3200 = x_1023 ((true, 0), (false, 0)) in
      let x_1830 = snd (fst x_3200) in
      let x_1831 = fst x_1830 in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          let x_1852 = fst x_1851 in
          let x_1853 = snd x_1851 in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          let x_1863 = fst x_1862 in
          let x_1864 = snd x_1862 in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

NORMALIZE: n_1595
[x_2165]
NORMALIZE: x_1912
[x_2165]
normalize let:
let List.nth_1056 x_1057 =
  let x_1633 x_1058 = rand_int () in
  let x_1788 xi_3594 =
    if fst (fst xi_3594) = false then
      if fst (snd xi_3594) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3594))))
    else
      if fst (snd xi_3594) = false then
        ((true, x_1633 (snd (fst xi_3594))), (false, (true, 0)))
      else
        ((true, x_1633 (snd (fst xi_3594))), (true, x_1057 (snd (snd xi_3594))))
  in
  x_1788
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  let x_1813 = snd x_1811 in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_3242 = x_1023 ((true, 0), (false, 0)) in
    let x_1816 = snd (fst x_3242) in
    let x_1818 = snd x_1816 in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        let x_1828 = fst x_1827 in
        let x_1829 = snd x_1827 in
        x_1827
      in
      let x_3200 = x_1023 ((true, 0), (false, 0)) in
      let x_1830 = snd (fst x_3200) in
      let x_1831 = fst x_1830 in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          let x_1852 = fst x_1851 in
          let x_1853 = snd x_1851 in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          let x_1863 = fst x_1862 in
          let x_1864 = snd x_1862 in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_1924; is_subsumed: 
rand_int (), x_1926 x_1925; is_subsumed: make_list_1008 n_1017, append_1059 x_1900; is_subsumed: 
make_list_1008 n_1017, x_1903 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_1903 ((true, i_1016), (false, 0), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
append_1059 x_1900, x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
x_1909 ((true, i_1016), (false, 0)), snd (#0 x_2195); is_subsumed: append_1059 x_1900, 
snd (#0 x_2195); is_subsumed: make_list_1008 n_1017, snd (#0 x_2195); is_subsumed: 
x_1909 ((true, i_1016), (false, 0)), if fst x_1912 <> false then
                                       snd x_1912
                                     else
                                       _|_; is_subsumed: x_1903 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_1912 <> false then
  snd x_1912
else
  _|_; is_subsumed: append_1059 x_1900, if fst x_1912 <> false then
                                          snd x_1912
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_1912 <> false then
  snd x_1912
else
  _|_; is_subsumed: if fst x_1912 <> false then
                      snd x_1912
                    else
                      _|_, snd (fst x_2165); is_subsumed: snd (#0 x_2195), 
snd (fst x_2165); is_subsumed: x_1903 ((true, i_1016), (false, 0), (false, 0)), 
snd (fst x_2165); is_subsumed: append_1059 x_1900, snd (fst x_2165); is_subsumed: 
make_list_1008 n_1017, snd (fst x_2165); is_subsumed: if fst x_1912 <> false then
                                                        snd x_1912
                                                      else
                                                        _|_, if fst x_1917 <> false then
                                                               snd x_1917
                                                             else
                                                               _|_; is_subsumed: 
snd (#0 x_2195), if fst x_1917 <> false then
                   snd x_1917
                 else
                   _|_; is_subsumed: x_1909 ((true, i_1016), (false, 0)), 
if fst x_1917 <> false then
  snd x_1917
else
  _|_; is_subsumed: x_1903 ((true, i_1016), (false, 0), (false, 0)), 
if fst x_1917 <> false then
  snd x_1917
else
  _|_; is_subsumed: append_1059 x_1900, if fst x_1917 <> false then
                                          snd x_1917
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst x_1917 <> false then
  snd x_1917
else
  _|_; is_subsumed: x_1023 ((true, 0), (false, 0)), snd x_1811; is_subsumed: 
snd x_1811, x_1023 ((true, 0), (false, 0)); is_subsumed: snd (fst x_3466), 
x_1023 ((true, 0), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_3466 |-> x_3242
is_subsumed: snd x_1811, snd (fst x_3242); is_subsumed: snd (fst x_3466), 
snd (fst x_3242); is_subsumed: x_1023 ((true, 0), (false, 0)), snd (fst x_3242); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1816; is_subsumed: snd x_1811, 
snd x_1816; is_subsumed: snd (fst x_3466), snd x_1816; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1816; is_subsumed: snd x_1816, _|_; is_subsumed: snd (fst x_3242), _|_; is_subsumed: 
x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: snd x_1811, _|_; is_subsumed: 
snd (fst x_3466), _|_; is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: 
snd x_1816, x_1023 ((true, 0), (false, 0)); is_subsumed: snd (fst x_3242), 
x_1023 ((true, 0), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); is_subsumed: snd x_1811, x_1023 ((true, 0), (false, 0)); is_subsumed: 
snd (fst x_3466), x_1023 ((true, 0), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_3242 |-> x_3200
x_3466 |-> x_3200
is_subsumed: snd x_1816, snd (fst x_3200); is_subsumed: snd (fst x_3242), 
snd (fst x_3200); is_subsumed: x_1023 ((true, 0), (false, 0)), snd (fst x_3200); is_subsumed: 
snd x_1811, snd (fst x_3200); is_subsumed: snd (fst x_3466), snd (fst x_3200); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd (fst x_3200); is_subsumed: x_1023 ((true, 0), (false, 0)), 
fst x_1830; is_subsumed: snd x_1816, fst x_1830; is_subsumed: snd (fst x_3242), 
fst x_1830; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1830; is_subsumed: 
snd x_1811, fst x_1830; is_subsumed: snd (fst x_3466), fst x_1830; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1830; is_subsumed: fst x_1830, 
append_1059 x_1835; is_subsumed: snd (fst x_3200), append_1059 x_1835; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1835; is_subsumed: snd x_1816, 
append_1059 x_1835; is_subsumed: snd (fst x_3242), append_1059 x_1835; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1835; is_subsumed: snd x_1811, 
append_1059 x_1835; is_subsumed: snd (fst x_3466), append_1059 x_1835; is_subsumed: 
x_1023 ((true, 0), (false, 0)), append_1059 x_1835; is_subsumed: append_1059 x_1835, 
x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: fst x_1830, x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
snd (fst x_3200), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
snd x_1816, x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: snd (fst x_3242), 
x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: snd x_1811, x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
snd (fst x_3466), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
append_1059 x_1835, snd (fst x_3030); is_subsumed: fst x_1830, snd (fst x_3030); is_subsumed: 
snd (fst x_3200), snd (fst x_3030); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (fst x_3030); is_subsumed: snd x_1816, snd (fst x_3030); is_subsumed: 
snd (fst x_3242), snd (fst x_3030); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (fst x_3030); is_subsumed: snd x_1811, snd (fst x_3030); is_subsumed: 
snd (fst x_3466), snd (fst x_3030); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (fst x_3030); is_subsumed: x_1844 ((true, i_1248 - 1), (false, 0)), 
fst x_1862; is_subsumed: append_1059 x_1835, fst x_1862; is_subsumed: 
fst x_1830, fst x_1862; is_subsumed: snd (fst x_3200), fst x_1862; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1862; is_subsumed: snd x_1816, 
fst x_1862; is_subsumed: snd (fst x_3242), fst x_1862; is_subsumed: x_1023 ((true, 0), (false, 0)), 
fst x_1862; is_subsumed: snd x_1811, fst x_1862; is_subsumed: snd (fst x_3466), 
fst x_1862; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1862; is_subsumed: 
fst x_1862, snd x_1862; is_subsumed: x_1844 ((true, i_1248 - 1), (false, 0)), 
snd x_1862; is_subsumed: append_1059 x_1835, snd x_1862; is_subsumed: 
fst x_1830, snd x_1862; is_subsumed: snd (fst x_3200), snd x_1862; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1862; is_subsumed: snd x_1816, 
snd x_1862; is_subsumed: snd (fst x_3242), snd x_1862; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1862; is_subsumed: snd x_1811, snd x_1862; is_subsumed: snd (fst x_3466), 
snd x_1862; is_subsumed: x_1023 ((true, 0), (false, 0)), snd x_1862; is_subsumed: 
fst x_1830, x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
snd (fst x_3200), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
snd x_1816, x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
snd (fst x_3242), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
snd x_1811, x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
snd (fst x_3466), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
append_1059 x_1835, snd (#0 x_3060); is_subsumed: fst x_1830, snd (#0 x_3060); is_subsumed: 
snd (fst x_3200), snd (#0 x_3060); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (#0 x_3060); is_subsumed: snd x_1816, snd (#0 x_3060); is_subsumed: 
snd (fst x_3242), snd (#0 x_3060); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (#0 x_3060); is_subsumed: snd x_1811, snd (#0 x_3060); is_subsumed: 
snd (fst x_3466), snd (#0 x_3060); is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd (#0 x_3060); is_subsumed: x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)), 
fst x_1851; is_subsumed: append_1059 x_1835, fst x_1851; is_subsumed: 
fst x_1830, fst x_1851; is_subsumed: snd (fst x_3200), fst x_1851; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1851; is_subsumed: snd x_1816, 
fst x_1851; is_subsumed: snd (fst x_3242), fst x_1851; is_subsumed: x_1023 ((true, 0), (false, 0)), 
fst x_1851; is_subsumed: snd x_1811, fst x_1851; is_subsumed: snd (fst x_3466), 
fst x_1851; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1851; is_subsumed: 
fst x_1851, snd x_1851; is_subsumed: x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)), 
snd x_1851; is_subsumed: append_1059 x_1835, snd x_1851; is_subsumed: 
fst x_1830, snd x_1851; is_subsumed: snd (fst x_3200), snd x_1851; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1851; is_subsumed: snd x_1816, 
snd x_1851; is_subsumed: snd (fst x_3242), snd x_1851; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1851; is_subsumed: snd x_1811, snd x_1851; is_subsumed: snd (fst x_3466), 
snd x_1851; is_subsumed: x_1023 ((true, 0), (false, 0)), snd x_1851; is_subsumed: 
snd x_1816, x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: snd (fst x_3242), 
x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: snd x_1811, x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
snd (fst x_3466), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
snd x_1816, snd (fst x_3221); is_subsumed: snd (fst x_3242), snd (fst x_3221); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd (fst x_3221); is_subsumed: snd x_1811, 
snd (fst x_3221); is_subsumed: snd (fst x_3466), snd (fst x_3221); is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd (fst x_3221); is_subsumed: x_1023 ((true, x_1155 + 1), (false, 0)), 
fst x_1827; is_subsumed: snd x_1816, fst x_1827; is_subsumed: snd (fst x_3242), 
fst x_1827; is_subsumed: x_1023 ((true, 0), (false, 0)), fst x_1827; is_subsumed: 
snd x_1811, fst x_1827; is_subsumed: snd (fst x_3466), fst x_1827; is_subsumed: 
x_1023 ((true, 0), (false, 0)), fst x_1827; is_subsumed: fst x_1827, 
snd x_1827; is_subsumed: x_1023 ((true, x_1155 + 1), (false, 0)), snd x_1827; is_subsumed: 
snd x_1816, snd x_1827; is_subsumed: snd (fst x_3242), snd x_1827; is_subsumed: 
x_1023 ((true, 0), (false, 0)), snd x_1827; is_subsumed: snd x_1811, 
snd x_1827; is_subsumed: snd (fst x_3466), snd x_1827; is_subsumed: x_1023 ((true, 0), (false, 0)), 
snd x_1827; is_subsumed: rand_int (), make_list_1008 (n_1009 - 1); x_3466; x_3242; x_3466
x_3242 |-> x_3466
x_3200 |-> x_3466
elim_same_app:
let List.nth_1056 x_1057 =
  let x_1633 x_1058 = rand_int () in
  let x_1788 xi_3594 =
    if fst (fst xi_3594) = false then
      if fst (snd xi_3594) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3594))))
    else
      if fst (snd xi_3594) = false then
        ((true, x_1633 (snd (fst xi_3594))), (false, (true, 0)))
      else
        ((true, x_1633 (snd (fst xi_3594))), (true, x_1057 (snd (snd xi_3594))))
  in
  x_1788
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  let x_1813 = snd x_1811 in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_1816 = snd (fst x_3466) in
    let x_1818 = snd x_1816 in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        let x_1828 = fst x_1827 in
        let x_1829 = snd x_1827 in
        x_1827
      in
      let x_1830 = snd (fst x_3466) in
      let x_1831 = fst x_1830 in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          let x_1852 = fst x_1851 in
          let x_1853 = snd x_1851 in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          let x_1863 = fst x_1862 in
          let x_1864 = snd x_1862 in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

elim_unused_branch:
let List.nth_1056 x_1057 =
  let x_1633 x_1058 = rand_int () in
  let x_1788 xi_3594 =
    if fst (fst xi_3594) = false then
      if fst (snd xi_3594) = false then
        ((false, 0), (false, (true, 0)))
      else
        ((false, 0), (true, x_1057 (snd (snd xi_3594))))
    else
      if fst (snd xi_3594) = false then
        ((true, x_1633 (snd (fst xi_3594))), (false, (true, 0)))
      else
        ((true, x_1633 (snd (fst xi_3594))), (true, x_1057 (snd (snd xi_3594))))
  in
  x_1788
in
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  let x_1813 = snd x_1811 in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_1816 = snd (fst x_3466) in
    let x_1818 = snd x_1816 in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        let x_1828 = fst x_1827 in
        let x_1829 = snd x_1827 in
        x_1827
      in
      let x_1830 = snd (fst x_3466) in
      let x_1831 = fst x_1830 in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          let x_1852 = fst x_1851 in
          let x_1853 = snd x_1851 in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          let x_1863 = fst x_1862 in
          let x_1864 = snd x_1862 in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

elim_unused_let:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          ((true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429))))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            ((true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377))))
    in
    x_1891
  else
    let x_1816 = snd (fst x_3466) in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        x_1827
      in
      let x_1830 = snd (fst x_3466) in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            ((true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163))))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            ((true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088))))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          x_1862
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            ((true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993))))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941))))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              ((true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), 
               (true, x_1873 (snd (#2 iii_2941))))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532))))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        ((true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223))))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

TUPLE: (true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532)))
x_1661
TUPLE: (true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0))
x_1661
TUPLE: (true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532)))
x_1661
TUPLE: (false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (true, x_1810 (snd (#2 iii_2532)))
x_1809
x_1810
compose: x_1809, snd
                 (fst
                  (x_1023
                    (let x1_3776 = let x1_3768 = true in
                                   let x2_3769 = x_3766 in
                                   (x1_3768, x2_3769) in
                     let x2_3777 = let x1_3772 = false in
                                   let x2_3773 = 0 in
                                   (x1_3772, x2_3773) in
                     (x1_3776, x2_3777)))); x_1810, snd
                                                    (snd
                                                     (x_1023
                                                       (let x1_3788 =
                                                          let x1_3780 = false in
                                                          let x2_3781 = 0 in
                                                          (x1_3780, x2_3781)
                                                        in
                                                        let x2_3789 =
                                                          let x1_3784 = true in
                                                          let x2_3785 = x_3767 in
                                                          (x1_3784, x2_3785)
                                                        in
                                                        (x1_3788, x2_3789)))); 
PB: x:x_1809
CHECK: snd
       (fst
        (x_1023
          (let x1_3776 = let x1_3768 = true in
                         let x2_3769 = x_3766 in
                         (x1_3768, x2_3769) in
           let x2_3777 = let x1_3772 = false in
                         let x2_3773 = 0 in
                         (x1_3772, x2_3773) in
           (x1_3776, x2_3777))))
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_3788 = let x1_3780 = false in
                         let x2_3781 = 0 in
                         (x1_3780, x2_3781) in
           let x2_3789 = let x1_3784 = true in
                         let x2_3785 = x_3767 in
                         (x1_3784, x2_3785) in
           (x1_3788, x2_3789))))
compose_let
x_1809:snd
       (fst
        (x_1023
          (let x1_3776 = let x1_3768 = true in
                         let x2_3769 = x_3766 in
                         (x1_3768, x2_3769) in
           let x2_3777 = let x1_3772 = false in
                         let x2_3773 = 0 in
                         (x1_3772, x2_3773) in
           (x1_3776, x2_3777))))

x_1810:snd
       (snd
        (x_1023
          (let x1_3788 = let x1_3780 = false in
                         let x2_3781 = 0 in
                         (x1_3780, x2_3781) in
           let x2_3789 = let x1_3784 = true in
                         let x2_3785 = x_3767 in
                         (x1_3784, x2_3785) in
           (x1_3788, x2_3789))))

ADD: (x_x_3792:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, xs'_1014 (snd (fst ii_3163))), (true, x_1810 (snd (snd ii_3163)))
xs'_1014
x_1810
compose: xs'_1014, let x_3221 =
                     x_1023
                       (let x1_3821 = let x1_3813 = true in
                                      let x2_3814 = x_3811 + 1 in
                                      (x1_3813, x2_3814) in
                        let x2_3822 = let x1_3817 = false in
                                      let x2_3818 = 0 in
                                      (x1_3817, x2_3818) in
                        (x1_3821, x2_3822))
                   in
                   let x_1827 = snd (fst x_3221) in
                   x_1827; x_1810, snd
                                   (snd
                                    (x_1023
                                      (let x1_3833 = let x1_3825 = false in
                                                     let x2_3826 = 0 in
                                                     (x1_3825, x2_3826) in
                                       let x2_3834 = let x1_3829 = true in
                                                     let x2_3830 = x_3812 in
                                                     (x1_3829, x2_3830) in
                                       (x1_3833, x2_3834)))); 
PB: x:xs'_1014
CHECK: x_1827
CHECK: snd (fst x_3221)
CHECK: x_1023
         (let x1_3821 = let x1_3813 = true in
                        let x2_3814 = x_3811 + 1 in
                        (x1_3813, x2_3814) in
          let x2_3822 = let x1_3817 = false in
                        let x2_3818 = 0 in
                        (x1_3817, x2_3818) in
          (x1_3821, x2_3822))
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_3833 = let x1_3825 = false in
                         let x2_3826 = 0 in
                         (x1_3825, x2_3826) in
           let x2_3834 = let x1_3829 = true in
                         let x2_3830 = x_3812 in
                         (x1_3829, x2_3830) in
           (x1_3833, x2_3834))))
compose_let
xs'_1014:let x_3221 =
           x_1023
             (let x1_3821 = let x1_3813 = true in
                            let x2_3814 = x_3811 + 1 in
                            (x1_3813, x2_3814) in
              let x2_3822 = let x1_3817 = false in
                            let x2_3818 = 0 in
                            (x1_3817, x2_3818) in
              (x1_3821, x2_3822))
         in
         let x_1827 = snd (fst x_3221) in
         x_1827

x_1810:snd
       (snd
        (x_1023
          (let x1_3833 = let x1_3825 = false in
                         let x2_3826 = 0 in
                         (x1_3825, x2_3826) in
           let x2_3834 = let x1_3829 = true in
                         let x2_3830 = x_3812 in
                         (x1_3829, x2_3830) in
           (x1_3833, x2_3834))))

ADD: (xs'_x_3837:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1840 (snd (fst ii_3088))), (true, x_1841 (snd (snd ii_3088)))
x_1840
x_1841
compose: x_1840, snd
                 (#1
                  (x_1838
                    (let x1_3865 = let x1_3853 = false in
                                   let x2_3854 = 0 in
                                   (x1_3853, x2_3854) in
                     let x2_3866 = let x1_3857 = true in
                                   let x2_3858 = x_3851 in
                                   (x1_3857, x2_3858) in
                     let x3_3867 = let x1_3861 = false in
                                   let x2_3862 = 0 in
                                   (x1_3861, x2_3862) in
                     (x1_3865, x2_3866, x3_3867)))); x_1841, snd
                                                             (#2
                                                              (x_1838
                                                                (let x1_3883 =
                                                                   let x1_3871 = false in
                                                                   let x2_3872 = 0 in
                                                                   (x1_3871, x2_3872)
                                                                 in
                                                                 let x2_3884 =
                                                                   let x1_3875 = false in
                                                                   let x2_3876 = 0 in
                                                                   (x1_3875, x2_3876)
                                                                 in
                                                                 let x3_3885 =
                                                                   let x1_3879 = true in
                                                                   let x2_3880 = x_3852 in
                                                                   (x1_3879, x2_3880)
                                                                 in
                                                                 (x1_3883, x2_3884, x3_3885)))); 
PB: x:x_1840
CHECK: snd
       (#1
        (x_1838
          (let x1_3865 = let x1_3853 = false in
                         let x2_3854 = 0 in
                         (x1_3853, x2_3854) in
           let x2_3866 = let x1_3857 = true in
                         let x2_3858 = x_3851 in
                         (x1_3857, x2_3858) in
           let x3_3867 = let x1_3861 = false in
                         let x2_3862 = 0 in
                         (x1_3861, x2_3862) in
           (x1_3865, x2_3866, x3_3867))))
PB: x:x_1841
CHECK: snd
       (#2
        (x_1838
          (let x1_3883 = let x1_3871 = false in
                         let x2_3872 = 0 in
                         (x1_3871, x2_3872) in
           let x2_3884 = let x1_3875 = false in
                         let x2_3876 = 0 in
                         (x1_3875, x2_3876) in
           let x3_3885 = let x1_3879 = true in
                         let x2_3880 = x_3852 in
                         (x1_3879, x2_3880) in
           (x1_3883, x2_3884, x3_3885))))
compose_let
x_1840:snd
       (#1
        (x_1838
          (let x1_3865 = let x1_3853 = false in
                         let x2_3854 = 0 in
                         (x1_3853, x2_3854) in
           let x2_3866 = let x1_3857 = true in
                         let x2_3858 = x_3851 in
                         (x1_3857, x2_3858) in
           let x3_3867 = let x1_3861 = false in
                         let x2_3862 = 0 in
                         (x1_3861, x2_3862) in
           (x1_3865, x2_3866, x3_3867))))

x_1841:snd
       (#2
        (x_1838
          (let x1_3883 = let x1_3871 = false in
                         let x2_3872 = 0 in
                         (x1_3871, x2_3872) in
           let x2_3884 = let x1_3875 = false in
                         let x2_3876 = 0 in
                         (x1_3875, x2_3876) in
           let x3_3885 = let x1_3879 = true in
                         let x2_3880 = x_3852 in
                         (x1_3879, x2_3880) in
           (x1_3883, x2_3884, x3_3885))))

ADD: (x_x_3889:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1694 (snd (fst ii_2993))), (true, x_1846 (snd (snd ii_2993)))
x_1694
x_1846
compose: x_1694, if x_3903 = 0 then
                   let x1_3917 = true in
                   let x2_3918 = snd x_1830 in
                   (x1_3917, x2_3918)
                 else
                   let x_3030 =
                     x_1844
                       (let x1_3913 = let x1_3905 = true in
                                      let x2_3906 = x_3903 - 1 in
                                      (x1_3905, x2_3906) in
                        let x2_3914 = let x1_3909 = false in
                                      let x2_3910 = 0 in
                                      (x1_3909, x2_3910) in
                        (x1_3913, x2_3914))
                   in
                   let x_1862 = snd (fst x_3030) in
                   x_1862; x_1846, snd
                                   (snd
                                    (x_1844
                                      (let x1_3929 = let x1_3921 = false in
                                                     let x2_3922 = 0 in
                                                     (x1_3921, x2_3922) in
                                       let x2_3930 = let x1_3925 = true in
                                                     let x2_3926 = x_3904 in
                                                     (x1_3925, x2_3926) in
                                       (x1_3929, x2_3930)))); 
compose: x_1694, let x_3030 =
                   x_1844
                     (let x1_3913 = let x1_3905 = true in
                                    let x2_3906 = x_3903 - 1 in
                                    (x1_3905, x2_3906) in
                      let x2_3914 = let x1_3909 = false in
                                    let x2_3910 = 0 in
                                    (x1_3909, x2_3910) in
                      (x1_3913, x2_3914))
                 in
                 let x_1862 = snd (fst x_3030) in
                 x_1862; x_1846, snd
                                 (snd
                                  (x_1844
                                    (let x1_3929 = let x1_3921 = false in
                                                   let x2_3922 = 0 in
                                                   (x1_3921, x2_3922) in
                                     let x2_3930 = let x1_3925 = true in
                                                   let x2_3926 = x_3904 in
                                                   (x1_3925, x2_3926) in
                                     (x1_3929, x2_3930)))); 
PB: x:x_1694
CHECK: x_1862
CHECK: snd (fst x_3030)
CHECK: x_1844
         (let x1_3913 = let x1_3905 = true in
                        let x2_3906 = x_3903 - 1 in
                        (x1_3905, x2_3906) in
          let x2_3914 = let x1_3909 = false in
                        let x2_3910 = 0 in
                        (x1_3909, x2_3910) in
          (x1_3913, x2_3914))
PB: x:x_1846
CHECK: snd
       (snd
        (x_1844
          (let x1_3929 = let x1_3921 = false in
                         let x2_3922 = 0 in
                         (x1_3921, x2_3922) in
           let x2_3930 = let x1_3925 = true in
                         let x2_3926 = x_3904 in
                         (x1_3925, x2_3926) in
           (x1_3929, x2_3930))))
compose_let
x_1694:let x_3030 =
         x_1844
           (let x1_3913 = let x1_3905 = true in
                          let x2_3906 = x_3903 - 1 in
                          (x1_3905, x2_3906) in
            let x2_3914 = let x1_3909 = false in
                          let x2_3910 = 0 in
                          (x1_3909, x2_3910) in
            (x1_3913, x2_3914))
       in
       let x_1862 = snd (fst x_3030) in
       x_1862

x_1846:snd
       (snd
        (x_1844
          (let x1_3929 = let x1_3921 = false in
                         let x2_3922 = 0 in
                         (x1_3921, x2_3922) in
           let x2_3930 = let x1_3925 = true in
                         let x2_3926 = x_3904 in
                         (x1_3925, x2_3926) in
           (x1_3929, x2_3930))))

compose: x_1694, let x1_3917 = true in
                 let x2_3918 = snd x_1830 in
                 (x1_3917, x2_3918); x_1846, snd
                                             (snd
                                              (x_1844
                                                (let x1_3929 =
                                                   let x1_3921 = false in
                                                   let x2_3922 = 0 in
                                                   (x1_3921, x2_3922)
                                                 in
                                                 let x2_3930 =
                                                   let x1_3925 = true in
                                                   let x2_3926 = x_3904 in
                                                   (x1_3925, x2_3926)
                                                 in
                                                 (x1_3929, x2_3930)))); 
PB: x:x_1694
CHECK: (x1_3917, x2_3918)
CHECK: snd x_1830
CHECK: true
PB: x:x_1846
CHECK: snd
       (snd
        (x_1844
          (let x1_3929 = let x1_3921 = false in
                         let x2_3922 = 0 in
                         (x1_3921, x2_3922) in
           let x2_3930 = let x1_3925 = true in
                         let x2_3926 = x_3904 in
                         (x1_3925, x2_3926) in
           (x1_3929, x2_3930))))
compose_let
x_1694:let x1_3917 = true in
       let x2_3918 = snd x_1830 in
       (x1_3917, x2_3918)

x_1846:snd
       (snd
        (x_1844
          (let x1_3929 = let x1_3921 = false in
                         let x2_3922 = 0 in
                         (x1_3921, x2_3922) in
           let x2_3930 = let x1_3925 = true in
                         let x2_3926 = x_3904 in
                         (x1_3925, x2_3926) in
           (x1_3929, x2_3930))))

ADD: (x_x_3933:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941)))
x_1700
x_1872
x_1873
compose: x_1700, if x_3953 = 0 then
                   let x1_3974 = true in
                   let x2_3975 = snd x_1830 in
                   (x1_3974, x2_3975)
                 else
                   let x_3060 =
                     x_1838
                       (let x1_3968 = let x1_3956 = true in
                                      let x2_3957 = x_3953 - 1 in
                                      (x1_3956, x2_3957) in
                        let x2_3969 = let x1_3960 = false in
                                      let x2_3961 = 0 in
                                      (x1_3960, x2_3961) in
                        let x3_3970 = let x1_3964 = false in
                                      let x2_3965 = 0 in
                                      (x1_3964, x2_3965) in
                        (x1_3968, x2_3969, x3_3970))
                   in
                   let x_1851 = snd (#0 x_3060) in
                   x_1851; x_1872, snd
                                   (fst
                                    (x_1871
                                      (let x1_3986 = let x1_3978 = true in
                                                     let x2_3979 = x_3954 in
                                                     (x1_3978, x2_3979) in
                                       let x2_3987 = let x1_3982 = false in
                                                     let x2_3983 = 0 in
                                                     (x1_3982, x2_3983) in
                                       (x1_3986, x2_3987)))); x_1873, 
snd
(snd
 (x_1871
   (let x1_3998 = let x1_3990 = false in
                  let x2_3991 = 0 in
                  (x1_3990, x2_3991) in
    let x2_3999 = let x1_3994 = true in
                  let x2_3995 = x_3955 in
                  (x1_3994, x2_3995) in
    (x1_3998, x2_3999)))); 
compose: x_1700, let x_3060 =
                   x_1838
                     (let x1_3968 = let x1_3956 = true in
                                    let x2_3957 = x_3953 - 1 in
                                    (x1_3956, x2_3957) in
                      let x2_3969 = let x1_3960 = false in
                                    let x2_3961 = 0 in
                                    (x1_3960, x2_3961) in
                      let x3_3970 = let x1_3964 = false in
                                    let x2_3965 = 0 in
                                    (x1_3964, x2_3965) in
                      (x1_3968, x2_3969, x3_3970))
                 in
                 let x_1851 = snd (#0 x_3060) in
                 x_1851; x_1872, snd
                                 (fst
                                  (x_1871
                                    (let x1_3986 = let x1_3978 = true in
                                                   let x2_3979 = x_3954 in
                                                   (x1_3978, x2_3979) in
                                     let x2_3987 = let x1_3982 = false in
                                                   let x2_3983 = 0 in
                                                   (x1_3982, x2_3983) in
                                     (x1_3986, x2_3987)))); x_1873, snd
                                                                    (
                                                                    snd
                                                                    (x_1871
                                                                    (let x1_3998 =
                                                                    let x1_3990 = false in
                                                                    let x2_3991 = 0 in
                                                                    (x1_3990, x2_3991)
                                                                    in
                                                                    let x2_3999 =
                                                                    let x1_3994 = true in
                                                                    let x2_3995 = x_3955 in
                                                                    (x1_3994, x2_3995)
                                                                    in
                                                                    (x1_3998, x2_3999)))); 
PB: x:x_1700
CHECK: x_1851
CHECK: snd (#0 x_3060)
CHECK: x_1838
         (let x1_3968 = let x1_3956 = true in
                        let x2_3957 = x_3953 - 1 in
                        (x1_3956, x2_3957) in
          let x2_3969 = let x1_3960 = false in
                        let x2_3961 = 0 in
                        (x1_3960, x2_3961) in
          let x3_3970 = let x1_3964 = false in
                        let x2_3965 = 0 in
                        (x1_3964, x2_3965) in
          (x1_3968, x2_3969, x3_3970))
PB: x:x_1872
CHECK: snd
       (fst
        (x_1871
          (let x1_3986 = let x1_3978 = true in
                         let x2_3979 = x_3954 in
                         (x1_3978, x2_3979) in
           let x2_3987 = let x1_3982 = false in
                         let x2_3983 = 0 in
                         (x1_3982, x2_3983) in
           (x1_3986, x2_3987))))
PB: x:x_1873
CHECK: snd
       (snd
        (x_1871
          (let x1_3998 = let x1_3990 = false in
                         let x2_3991 = 0 in
                         (x1_3990, x2_3991) in
           let x2_3999 = let x1_3994 = true in
                         let x2_3995 = x_3955 in
                         (x1_3994, x2_3995) in
           (x1_3998, x2_3999))))
compose_let
x_1700:let x_3060 =
         x_1838
           (let x1_3968 = let x1_3956 = true in
                          let x2_3957 = x_3953 - 1 in
                          (x1_3956, x2_3957) in
            let x2_3969 = let x1_3960 = false in
                          let x2_3961 = 0 in
                          (x1_3960, x2_3961) in
            let x3_3970 = let x1_3964 = false in
                          let x2_3965 = 0 in
                          (x1_3964, x2_3965) in
            (x1_3968, x2_3969, x3_3970))
       in
       let x_1851 = snd (#0 x_3060) in
       x_1851

x_1872:snd
       (fst
        (x_1871
          (let x1_3986 = let x1_3978 = true in
                         let x2_3979 = x_3954 in
                         (x1_3978, x2_3979) in
           let x2_3987 = let x1_3982 = false in
                         let x2_3983 = 0 in
                         (x1_3982, x2_3983) in
           (x1_3986, x2_3987))))

x_1873:snd
       (snd
        (x_1871
          (let x1_3998 = let x1_3990 = false in
                         let x2_3991 = 0 in
                         (x1_3990, x2_3991) in
           let x2_3999 = let x1_3994 = true in
                         let x2_3995 = x_3955 in
                         (x1_3994, x2_3995) in
           (x1_3998, x2_3999))))

compose: x_1700, let x1_3974 = true in
                 let x2_3975 = snd x_1830 in
                 (x1_3974, x2_3975); x_1872, snd
                                             (fst
                                              (x_1871
                                                (let x1_3986 =
                                                   let x1_3978 = true in
                                                   let x2_3979 = x_3954 in
                                                   (x1_3978, x2_3979)
                                                 in
                                                 let x2_3987 =
                                                   let x1_3982 = false in
                                                   let x2_3983 = 0 in
                                                   (x1_3982, x2_3983)
                                                 in
                                                 (x1_3986, x2_3987)))); x_1873, 
snd
(snd
 (x_1871
   (let x1_3998 = let x1_3990 = false in
                  let x2_3991 = 0 in
                  (x1_3990, x2_3991) in
    let x2_3999 = let x1_3994 = true in
                  let x2_3995 = x_3955 in
                  (x1_3994, x2_3995) in
    (x1_3998, x2_3999)))); 
PB: x:x_1700
CHECK: (x1_3974, x2_3975)
CHECK: snd x_1830
CHECK: true
PB: x:x_1872
CHECK: snd
       (fst
        (x_1871
          (let x1_3986 = let x1_3978 = true in
                         let x2_3979 = x_3954 in
                         (x1_3978, x2_3979) in
           let x2_3987 = let x1_3982 = false in
                         let x2_3983 = 0 in
                         (x1_3982, x2_3983) in
           (x1_3986, x2_3987))))
PB: x:x_1873
CHECK: snd
       (snd
        (x_1871
          (let x1_3998 = let x1_3990 = false in
                         let x2_3991 = 0 in
                         (x1_3990, x2_3991) in
           let x2_3999 = let x1_3994 = true in
                         let x2_3995 = x_3955 in
                         (x1_3994, x2_3995) in
           (x1_3998, x2_3999))))
compose_let
x_1700:let x1_3974 = true in
       let x2_3975 = snd x_1830 in
       (x1_3974, x2_3975)

x_1872:snd
       (fst
        (x_1871
          (let x1_3986 = let x1_3978 = true in
                         let x2_3979 = x_3954 in
                         (x1_3978, x2_3979) in
           let x2_3987 = let x1_3982 = false in
                         let x2_3983 = 0 in
                         (x1_3982, x2_3983) in
           (x1_3986, x2_3987))))

x_1873:snd
       (snd
        (x_1871
          (let x1_3998 = let x1_3990 = false in
                         let x2_3991 = 0 in
                         (x1_3990, x2_3991) in
           let x2_3999 = let x1_3994 = true in
                         let x2_3995 = x_3955 in
                         (x1_3994, x2_3995) in
           (x1_3998, x2_3999))))

ADD: (x_x_x_4002:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1700 (snd (#0 iii_2941))), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0))
x_1700
x_1872
compose: x_1700, if x_4031 = 0 then
                   let x1_4051 = true in
                   let x2_4052 = snd x_1830 in
                   (x1_4051, x2_4052)
                 else
                   let x_3060 =
                     x_1838
                       (let x1_4045 = let x1_4033 = true in
                                      let x2_4034 = x_4031 - 1 in
                                      (x1_4033, x2_4034) in
                        let x2_4046 = let x1_4037 = false in
                                      let x2_4038 = 0 in
                                      (x1_4037, x2_4038) in
                        let x3_4047 = let x1_4041 = false in
                                      let x2_4042 = 0 in
                                      (x1_4041, x2_4042) in
                        (x1_4045, x2_4046, x3_4047))
                   in
                   let x_1851 = snd (#0 x_3060) in
                   x_1851; x_1872, snd
                                   (fst
                                    (x_1871
                                      (let x1_4063 = let x1_4055 = true in
                                                     let x2_4056 = x_4032 in
                                                     (x1_4055, x2_4056) in
                                       let x2_4064 = let x1_4059 = false in
                                                     let x2_4060 = 0 in
                                                     (x1_4059, x2_4060) in
                                       (x1_4063, x2_4064)))); 
compose: x_1700, let x_3060 =
                   x_1838
                     (let x1_4045 = let x1_4033 = true in
                                    let x2_4034 = x_4031 - 1 in
                                    (x1_4033, x2_4034) in
                      let x2_4046 = let x1_4037 = false in
                                    let x2_4038 = 0 in
                                    (x1_4037, x2_4038) in
                      let x3_4047 = let x1_4041 = false in
                                    let x2_4042 = 0 in
                                    (x1_4041, x2_4042) in
                      (x1_4045, x2_4046, x3_4047))
                 in
                 let x_1851 = snd (#0 x_3060) in
                 x_1851; x_1872, snd
                                 (fst
                                  (x_1871
                                    (let x1_4063 = let x1_4055 = true in
                                                   let x2_4056 = x_4032 in
                                                   (x1_4055, x2_4056) in
                                     let x2_4064 = let x1_4059 = false in
                                                   let x2_4060 = 0 in
                                                   (x1_4059, x2_4060) in
                                     (x1_4063, x2_4064)))); 
PB: x:x_1700
CHECK: x_1851
CHECK: snd (#0 x_3060)
CHECK: x_1838
         (let x1_4045 = let x1_4033 = true in
                        let x2_4034 = x_4031 - 1 in
                        (x1_4033, x2_4034) in
          let x2_4046 = let x1_4037 = false in
                        let x2_4038 = 0 in
                        (x1_4037, x2_4038) in
          let x3_4047 = let x1_4041 = false in
                        let x2_4042 = 0 in
                        (x1_4041, x2_4042) in
          (x1_4045, x2_4046, x3_4047))
PB: x:x_1872
CHECK: snd
       (fst
        (x_1871
          (let x1_4063 = let x1_4055 = true in
                         let x2_4056 = x_4032 in
                         (x1_4055, x2_4056) in
           let x2_4064 = let x1_4059 = false in
                         let x2_4060 = 0 in
                         (x1_4059, x2_4060) in
           (x1_4063, x2_4064))))
compose_let
x_1700:let x_3060 =
         x_1838
           (let x1_4045 = let x1_4033 = true in
                          let x2_4034 = x_4031 - 1 in
                          (x1_4033, x2_4034) in
            let x2_4046 = let x1_4037 = false in
                          let x2_4038 = 0 in
                          (x1_4037, x2_4038) in
            let x3_4047 = let x1_4041 = false in
                          let x2_4042 = 0 in
                          (x1_4041, x2_4042) in
            (x1_4045, x2_4046, x3_4047))
       in
       let x_1851 = snd (#0 x_3060) in
       x_1851

x_1872:snd
       (fst
        (x_1871
          (let x1_4063 = let x1_4055 = true in
                         let x2_4056 = x_4032 in
                         (x1_4055, x2_4056) in
           let x2_4064 = let x1_4059 = false in
                         let x2_4060 = 0 in
                         (x1_4059, x2_4060) in
           (x1_4063, x2_4064))))

compose: x_1700, let x1_4051 = true in
                 let x2_4052 = snd x_1830 in
                 (x1_4051, x2_4052); x_1872, snd
                                             (fst
                                              (x_1871
                                                (let x1_4063 =
                                                   let x1_4055 = true in
                                                   let x2_4056 = x_4032 in
                                                   (x1_4055, x2_4056)
                                                 in
                                                 let x2_4064 =
                                                   let x1_4059 = false in
                                                   let x2_4060 = 0 in
                                                   (x1_4059, x2_4060)
                                                 in
                                                 (x1_4063, x2_4064)))); 
PB: x:x_1700
CHECK: (x1_4051, x2_4052)
CHECK: snd x_1830
CHECK: true
PB: x:x_1872
CHECK: snd
       (fst
        (x_1871
          (let x1_4063 = let x1_4055 = true in
                         let x2_4056 = x_4032 in
                         (x1_4055, x2_4056) in
           let x2_4064 = let x1_4059 = false in
                         let x2_4060 = 0 in
                         (x1_4059, x2_4060) in
           (x1_4063, x2_4064))))
compose_let
x_1700:let x1_4051 = true in
       let x2_4052 = snd x_1830 in
       (x1_4051, x2_4052)

x_1872:snd
       (fst
        (x_1871
          (let x1_4063 = let x1_4055 = true in
                         let x2_4056 = x_4032 in
                         (x1_4055, x2_4056) in
           let x2_4064 = let x1_4059 = false in
                         let x2_4060 = 0 in
                         (x1_4059, x2_4060) in
           (x1_4063, x2_4064))))

ADD: (x_x_4067:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941)))
x_1700
x_1873
compose: x_1700, if x_4092 = 0 then
                   let x1_4112 = true in
                   let x2_4113 = snd x_1830 in
                   (x1_4112, x2_4113)
                 else
                   let x_3060 =
                     x_1838
                       (let x1_4106 = let x1_4094 = true in
                                      let x2_4095 = x_4092 - 1 in
                                      (x1_4094, x2_4095) in
                        let x2_4107 = let x1_4098 = false in
                                      let x2_4099 = 0 in
                                      (x1_4098, x2_4099) in
                        let x3_4108 = let x1_4102 = false in
                                      let x2_4103 = 0 in
                                      (x1_4102, x2_4103) in
                        (x1_4106, x2_4107, x3_4108))
                   in
                   let x_1851 = snd (#0 x_3060) in
                   x_1851; x_1873, snd
                                   (snd
                                    (x_1871
                                      (let x1_4124 = let x1_4116 = false in
                                                     let x2_4117 = 0 in
                                                     (x1_4116, x2_4117) in
                                       let x2_4125 = let x1_4120 = true in
                                                     let x2_4121 = x_4093 in
                                                     (x1_4120, x2_4121) in
                                       (x1_4124, x2_4125)))); 
compose: x_1700, let x_3060 =
                   x_1838
                     (let x1_4106 = let x1_4094 = true in
                                    let x2_4095 = x_4092 - 1 in
                                    (x1_4094, x2_4095) in
                      let x2_4107 = let x1_4098 = false in
                                    let x2_4099 = 0 in
                                    (x1_4098, x2_4099) in
                      let x3_4108 = let x1_4102 = false in
                                    let x2_4103 = 0 in
                                    (x1_4102, x2_4103) in
                      (x1_4106, x2_4107, x3_4108))
                 in
                 let x_1851 = snd (#0 x_3060) in
                 x_1851; x_1873, snd
                                 (snd
                                  (x_1871
                                    (let x1_4124 = let x1_4116 = false in
                                                   let x2_4117 = 0 in
                                                   (x1_4116, x2_4117) in
                                     let x2_4125 = let x1_4120 = true in
                                                   let x2_4121 = x_4093 in
                                                   (x1_4120, x2_4121) in
                                     (x1_4124, x2_4125)))); 
PB: x:x_1700
CHECK: x_1851
CHECK: snd (#0 x_3060)
CHECK: x_1838
         (let x1_4106 = let x1_4094 = true in
                        let x2_4095 = x_4092 - 1 in
                        (x1_4094, x2_4095) in
          let x2_4107 = let x1_4098 = false in
                        let x2_4099 = 0 in
                        (x1_4098, x2_4099) in
          let x3_4108 = let x1_4102 = false in
                        let x2_4103 = 0 in
                        (x1_4102, x2_4103) in
          (x1_4106, x2_4107, x3_4108))
PB: x:x_1873
CHECK: snd
       (snd
        (x_1871
          (let x1_4124 = let x1_4116 = false in
                         let x2_4117 = 0 in
                         (x1_4116, x2_4117) in
           let x2_4125 = let x1_4120 = true in
                         let x2_4121 = x_4093 in
                         (x1_4120, x2_4121) in
           (x1_4124, x2_4125))))
compose_let
x_1700:let x_3060 =
         x_1838
           (let x1_4106 = let x1_4094 = true in
                          let x2_4095 = x_4092 - 1 in
                          (x1_4094, x2_4095) in
            let x2_4107 = let x1_4098 = false in
                          let x2_4099 = 0 in
                          (x1_4098, x2_4099) in
            let x3_4108 = let x1_4102 = false in
                          let x2_4103 = 0 in
                          (x1_4102, x2_4103) in
            (x1_4106, x2_4107, x3_4108))
       in
       let x_1851 = snd (#0 x_3060) in
       x_1851

x_1873:snd
       (snd
        (x_1871
          (let x1_4124 = let x1_4116 = false in
                         let x2_4117 = 0 in
                         (x1_4116, x2_4117) in
           let x2_4125 = let x1_4120 = true in
                         let x2_4121 = x_4093 in
                         (x1_4120, x2_4121) in
           (x1_4124, x2_4125))))

compose: x_1700, let x1_4112 = true in
                 let x2_4113 = snd x_1830 in
                 (x1_4112, x2_4113); x_1873, snd
                                             (snd
                                              (x_1871
                                                (let x1_4124 =
                                                   let x1_4116 = false in
                                                   let x2_4117 = 0 in
                                                   (x1_4116, x2_4117)
                                                 in
                                                 let x2_4125 =
                                                   let x1_4120 = true in
                                                   let x2_4121 = x_4093 in
                                                   (x1_4120, x2_4121)
                                                 in
                                                 (x1_4124, x2_4125)))); 
PB: x:x_1700
CHECK: (x1_4112, x2_4113)
CHECK: snd x_1830
CHECK: true
PB: x:x_1873
CHECK: snd
       (snd
        (x_1871
          (let x1_4124 = let x1_4116 = false in
                         let x2_4117 = 0 in
                         (x1_4116, x2_4117) in
           let x2_4125 = let x1_4120 = true in
                         let x2_4121 = x_4093 in
                         (x1_4120, x2_4121) in
           (x1_4124, x2_4125))))
compose_let
x_1700:let x1_4112 = true in
       let x2_4113 = snd x_1830 in
       (x1_4112, x2_4113)

x_1873:snd
       (snd
        (x_1871
          (let x1_4124 = let x1_4116 = false in
                         let x2_4117 = 0 in
                         (x1_4116, x2_4117) in
           let x2_4125 = let x1_4120 = true in
                         let x2_4121 = x_4093 in
                         (x1_4120, x2_4121) in
           (x1_4124, x2_4125))))

ADD: (x_x_4128:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (true, x_1873 (snd (#2 iii_2941)))
x_1872
x_1873
compose: x_1872, snd
                 (fst
                  (x_1871
                    (let x1_4163 = let x1_4155 = true in
                                   let x2_4156 = x_4153 in
                                   (x1_4155, x2_4156) in
                     let x2_4164 = let x1_4159 = false in
                                   let x2_4160 = 0 in
                                   (x1_4159, x2_4160) in
                     (x1_4163, x2_4164)))); x_1873, snd
                                                    (snd
                                                     (x_1871
                                                       (let x1_4175 =
                                                          let x1_4167 = false in
                                                          let x2_4168 = 0 in
                                                          (x1_4167, x2_4168)
                                                        in
                                                        let x2_4176 =
                                                          let x1_4171 = true in
                                                          let x2_4172 = x_4154 in
                                                          (x1_4171, x2_4172)
                                                        in
                                                        (x1_4175, x2_4176)))); 
PB: x:x_1872
CHECK: snd
       (fst
        (x_1871
          (let x1_4163 = let x1_4155 = true in
                         let x2_4156 = x_4153 in
                         (x1_4155, x2_4156) in
           let x2_4164 = let x1_4159 = false in
                         let x2_4160 = 0 in
                         (x1_4159, x2_4160) in
           (x1_4163, x2_4164))))
PB: x:x_1873
CHECK: snd
       (snd
        (x_1871
          (let x1_4175 = let x1_4167 = false in
                         let x2_4168 = 0 in
                         (x1_4167, x2_4168) in
           let x2_4176 = let x1_4171 = true in
                         let x2_4172 = x_4154 in
                         (x1_4171, x2_4172) in
           (x1_4175, x2_4176))))
compose_let
x_1872:snd
       (fst
        (x_1871
          (let x1_4163 = let x1_4155 = true in
                         let x2_4156 = x_4153 in
                         (x1_4155, x2_4156) in
           let x2_4164 = let x1_4159 = false in
                         let x2_4160 = 0 in
                         (x1_4159, x2_4160) in
           (x1_4163, x2_4164))))

x_1873:snd
       (snd
        (x_1871
          (let x1_4175 = let x1_4167 = false in
                         let x2_4168 = 0 in
                         (x1_4167, x2_4168) in
           let x2_4176 = let x1_4171 = true in
                         let x2_4172 = x_4154 in
                         (x1_4171, x2_4172) in
           (x1_4175, x2_4176))))

ADD: (x_x_4179:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1714 (snd (fst xi_3429))), (true, x_1810 (snd (snd xi_3429)))
x_1714
x_1810
compose: x_1714, let x1_4200 = false in
                 let x2_4201 = 0 in
                 (x1_4200, x2_4201); x_1810, snd
                                             (snd
                                              (x_1023
                                                (let x1_4212 =
                                                   let x1_4204 = false in
                                                   let x2_4205 = 0 in
                                                   (x1_4204, x2_4205)
                                                 in
                                                 let x2_4213 =
                                                   let x1_4208 = true in
                                                   let x2_4209 = x_4199 in
                                                   (x1_4208, x2_4209)
                                                 in
                                                 (x1_4212, x2_4213)))); 
PB: x:x_1714
CHECK: (x1_4200, x2_4201)
CHECK: 0
CHECK: false
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_4212 = let x1_4204 = false in
                         let x2_4205 = 0 in
                         (x1_4204, x2_4205) in
           let x2_4213 = let x1_4208 = true in
                         let x2_4209 = x_4199 in
                         (x1_4208, x2_4209) in
           (x1_4212, x2_4213))))
compose_let
x_1714:let x1_4200 = false in
       let x2_4201 = 0 in
       (x1_4200, x2_4201)

x_1810:snd
       (snd
        (x_1023
          (let x1_4212 = let x1_4204 = false in
                         let x2_4205 = 0 in
                         (x1_4204, x2_4205) in
           let x2_4213 = let x1_4208 = true in
                         let x2_4209 = x_4199 in
                         (x1_4208, x2_4209) in
           (x1_4212, x2_4213))))

ADD: (x_x_4216:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
TUPLE: (true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377)))
x_1810
x_1886
x_1887
compose: x_1810, snd
                 (snd
                  (x_1023
                    (let x1_4241 = let x1_4233 = false in
                                   let x2_4234 = 0 in
                                   (x1_4233, x2_4234) in
                     let x2_4242 = let x1_4237 = true in
                                   let x2_4238 = x_4230 in
                                   (x1_4237, x2_4238) in
                     (x1_4241, x2_4242)))); x_1886, snd
                                                    (fst
                                                     (x_1885
                                                       (let x1_4253 =
                                                          let x1_4245 = true in
                                                          let x2_4246 = x_4231 in
                                                          (x1_4245, x2_4246)
                                                        in
                                                        let x2_4254 =
                                                          let x1_4249 = false in
                                                          let x2_4250 = 0 in
                                                          (x1_4249, x2_4250)
                                                        in
                                                        (x1_4253, x2_4254)))); x_1887, 
snd
(snd
 (x_1885
   (let x1_4265 = let x1_4257 = false in
                  let x2_4258 = 0 in
                  (x1_4257, x2_4258) in
    let x2_4266 = let x1_4261 = true in
                  let x2_4262 = x_4232 in
                  (x1_4261, x2_4262) in
    (x1_4265, x2_4266)))); 
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_4241 = let x1_4233 = false in
                         let x2_4234 = 0 in
                         (x1_4233, x2_4234) in
           let x2_4242 = let x1_4237 = true in
                         let x2_4238 = x_4230 in
                         (x1_4237, x2_4238) in
           (x1_4241, x2_4242))))
PB: x:x_1886
CHECK: snd
       (fst
        (x_1885
          (let x1_4253 = let x1_4245 = true in
                         let x2_4246 = x_4231 in
                         (x1_4245, x2_4246) in
           let x2_4254 = let x1_4249 = false in
                         let x2_4250 = 0 in
                         (x1_4249, x2_4250) in
           (x1_4253, x2_4254))))
PB: x:x_1887
CHECK: snd
       (snd
        (x_1885
          (let x1_4265 = let x1_4257 = false in
                         let x2_4258 = 0 in
                         (x1_4257, x2_4258) in
           let x2_4266 = let x1_4261 = true in
                         let x2_4262 = x_4232 in
                         (x1_4261, x2_4262) in
           (x1_4265, x2_4266))))
compose_let
x_1810:snd
       (snd
        (x_1023
          (let x1_4241 = let x1_4233 = false in
                         let x2_4234 = 0 in
                         (x1_4233, x2_4234) in
           let x2_4242 = let x1_4237 = true in
                         let x2_4238 = x_4230 in
                         (x1_4237, x2_4238) in
           (x1_4241, x2_4242))))

x_1886:snd
       (fst
        (x_1885
          (let x1_4253 = let x1_4245 = true in
                         let x2_4246 = x_4231 in
                         (x1_4245, x2_4246) in
           let x2_4254 = let x1_4249 = false in
                         let x2_4250 = 0 in
                         (x1_4249, x2_4250) in
           (x1_4253, x2_4254))))

x_1887:snd
       (snd
        (x_1885
          (let x1_4265 = let x1_4257 = false in
                         let x2_4258 = 0 in
                         (x1_4257, x2_4258) in
           let x2_4266 = let x1_4261 = true in
                         let x2_4262 = x_4232 in
                         (x1_4261, x2_4262) in
           (x1_4265, x2_4266))))

ADD: (x_x_x_4269:(int -> int -> int -> ((bool * int) * (bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1810 (snd (#0 ixi_3377))), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0))
x_1810
x_1886
compose: x_1810, snd
                 (snd
                  (x_1023
                    (let x1_4299 = let x1_4291 = false in
                                   let x2_4292 = 0 in
                                   (x1_4291, x2_4292) in
                     let x2_4300 = let x1_4295 = true in
                                   let x2_4296 = x_4289 in
                                   (x1_4295, x2_4296) in
                     (x1_4299, x2_4300)))); x_1886, snd
                                                    (fst
                                                     (x_1885
                                                       (let x1_4311 =
                                                          let x1_4303 = true in
                                                          let x2_4304 = x_4290 in
                                                          (x1_4303, x2_4304)
                                                        in
                                                        let x2_4312 =
                                                          let x1_4307 = false in
                                                          let x2_4308 = 0 in
                                                          (x1_4307, x2_4308)
                                                        in
                                                        (x1_4311, x2_4312)))); 
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_4299 = let x1_4291 = false in
                         let x2_4292 = 0 in
                         (x1_4291, x2_4292) in
           let x2_4300 = let x1_4295 = true in
                         let x2_4296 = x_4289 in
                         (x1_4295, x2_4296) in
           (x1_4299, x2_4300))))
PB: x:x_1886
CHECK: snd
       (fst
        (x_1885
          (let x1_4311 = let x1_4303 = true in
                         let x2_4304 = x_4290 in
                         (x1_4303, x2_4304) in
           let x2_4312 = let x1_4307 = false in
                         let x2_4308 = 0 in
                         (x1_4307, x2_4308) in
           (x1_4311, x2_4312))))
compose_let
x_1810:snd
       (snd
        (x_1023
          (let x1_4299 = let x1_4291 = false in
                         let x2_4292 = 0 in
                         (x1_4291, x2_4292) in
           let x2_4300 = let x1_4295 = true in
                         let x2_4296 = x_4289 in
                         (x1_4295, x2_4296) in
           (x1_4299, x2_4300))))

x_1886:snd
       (fst
        (x_1885
          (let x1_4311 = let x1_4303 = true in
                         let x2_4304 = x_4290 in
                         (x1_4303, x2_4304) in
           let x2_4312 = let x1_4307 = false in
                         let x2_4308 = 0 in
                         (x1_4307, x2_4308) in
           (x1_4311, x2_4312))))

ADD: (x_x_4315:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377)))
x_1810
x_1887
compose: x_1810, snd
                 (snd
                  (x_1023
                    (let x1_4344 = let x1_4336 = false in
                                   let x2_4337 = 0 in
                                   (x1_4336, x2_4337) in
                     let x2_4345 = let x1_4340 = true in
                                   let x2_4341 = x_4334 in
                                   (x1_4340, x2_4341) in
                     (x1_4344, x2_4345)))); x_1887, snd
                                                    (snd
                                                     (x_1885
                                                       (let x1_4356 =
                                                          let x1_4348 = false in
                                                          let x2_4349 = 0 in
                                                          (x1_4348, x2_4349)
                                                        in
                                                        let x2_4357 =
                                                          let x1_4352 = true in
                                                          let x2_4353 = x_4335 in
                                                          (x1_4352, x2_4353)
                                                        in
                                                        (x1_4356, x2_4357)))); 
PB: x:x_1810
CHECK: snd
       (snd
        (x_1023
          (let x1_4344 = let x1_4336 = false in
                         let x2_4337 = 0 in
                         (x1_4336, x2_4337) in
           let x2_4345 = let x1_4340 = true in
                         let x2_4341 = x_4334 in
                         (x1_4340, x2_4341) in
           (x1_4344, x2_4345))))
PB: x:x_1887
CHECK: snd
       (snd
        (x_1885
          (let x1_4356 = let x1_4348 = false in
                         let x2_4349 = 0 in
                         (x1_4348, x2_4349) in
           let x2_4357 = let x1_4352 = true in
                         let x2_4353 = x_4335 in
                         (x1_4352, x2_4353) in
           (x1_4356, x2_4357))))
compose_let
x_1810:snd
       (snd
        (x_1023
          (let x1_4344 = let x1_4336 = false in
                         let x2_4337 = 0 in
                         (x1_4336, x2_4337) in
           let x2_4345 = let x1_4340 = true in
                         let x2_4341 = x_4334 in
                         (x1_4340, x2_4341) in
           (x1_4344, x2_4345))))

x_1887:snd
       (snd
        (x_1885
          (let x1_4356 = let x1_4348 = false in
                         let x2_4349 = 0 in
                         (x1_4348, x2_4349) in
           let x2_4357 = let x1_4352 = true in
                         let x2_4353 = x_4335 in
                         (x1_4352, x2_4353) in
           (x1_4356, x2_4357))))

ADD: (x_x_4360:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (true, x_1887 (snd (#2 ixi_3377)))
x_1886
x_1887
compose: x_1886, snd
                 (fst
                  (x_1885
                    (let x1_4389 = let x1_4381 = true in
                                   let x2_4382 = x_4379 in
                                   (x1_4381, x2_4382) in
                     let x2_4390 = let x1_4385 = false in
                                   let x2_4386 = 0 in
                                   (x1_4385, x2_4386) in
                     (x1_4389, x2_4390)))); x_1887, snd
                                                    (snd
                                                     (x_1885
                                                       (let x1_4401 =
                                                          let x1_4393 = false in
                                                          let x2_4394 = 0 in
                                                          (x1_4393, x2_4394)
                                                        in
                                                        let x2_4402 =
                                                          let x1_4397 = true in
                                                          let x2_4398 = x_4380 in
                                                          (x1_4397, x2_4398)
                                                        in
                                                        (x1_4401, x2_4402)))); 
PB: x:x_1886
CHECK: snd
       (fst
        (x_1885
          (let x1_4389 = let x1_4381 = true in
                         let x2_4382 = x_4379 in
                         (x1_4381, x2_4382) in
           let x2_4390 = let x1_4385 = false in
                         let x2_4386 = 0 in
                         (x1_4385, x2_4386) in
           (x1_4389, x2_4390))))
PB: x:x_1887
CHECK: snd
       (snd
        (x_1885
          (let x1_4401 = let x1_4393 = false in
                         let x2_4394 = 0 in
                         (x1_4393, x2_4394) in
           let x2_4402 = let x1_4397 = true in
                         let x2_4398 = x_4380 in
                         (x1_4397, x2_4398) in
           (x1_4401, x2_4402))))
compose_let
x_1886:snd
       (fst
        (x_1885
          (let x1_4389 = let x1_4381 = true in
                         let x2_4382 = x_4379 in
                         (x1_4381, x2_4382) in
           let x2_4390 = let x1_4385 = false in
                         let x2_4386 = 0 in
                         (x1_4385, x2_4386) in
           (x1_4389, x2_4390))))

x_1887:snd
       (snd
        (x_1885
          (let x1_4401 = let x1_4393 = false in
                         let x2_4394 = 0 in
                         (x1_4393, x2_4394) in
           let x2_4402 = let x1_4397 = true in
                         let x2_4398 = x_4380 in
                         (x1_4397, x2_4398) in
           (x1_4401, x2_4402))))

ADD: (x_x_4405:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
#2
TUPLE: (true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298)))
x_1892
TUPLE: (true, x_1905 (snd (fst ii_2223))), (true, x_1906 (snd (snd ii_2223)))
x_1905
x_1906
compose: x_1905, snd
                 (#1
                  (x_1903
                    (let x1_4440 = let x1_4428 = false in
                                   let x2_4429 = 0 in
                                   (x1_4428, x2_4429) in
                     let x2_4441 = let x1_4432 = true in
                                   let x2_4433 = x_4426 in
                                   (x1_4432, x2_4433) in
                     let x3_4442 = let x1_4436 = false in
                                   let x2_4437 = 0 in
                                   (x1_4436, x2_4437) in
                     (x1_4440, x2_4441, x3_4442)))); x_1906, snd
                                                             (#2
                                                              (x_1903
                                                                (let x1_4458 =
                                                                   let x1_4446 = false in
                                                                   let x2_4447 = 0 in
                                                                   (x1_4446, x2_4447)
                                                                 in
                                                                 let x2_4459 =
                                                                   let x1_4450 = false in
                                                                   let x2_4451 = 0 in
                                                                   (x1_4450, x2_4451)
                                                                 in
                                                                 let x3_4460 =
                                                                   let x1_4454 = true in
                                                                   let x2_4455 = x_4427 in
                                                                   (x1_4454, x2_4455)
                                                                 in
                                                                 (x1_4458, x2_4459, x3_4460)))); 
PB: x:x_1905
CHECK: snd
       (#1
        (x_1903
          (let x1_4440 = let x1_4428 = false in
                         let x2_4429 = 0 in
                         (x1_4428, x2_4429) in
           let x2_4441 = let x1_4432 = true in
                         let x2_4433 = x_4426 in
                         (x1_4432, x2_4433) in
           let x3_4442 = let x1_4436 = false in
                         let x2_4437 = 0 in
                         (x1_4436, x2_4437) in
           (x1_4440, x2_4441, x3_4442))))
PB: x:x_1906
CHECK: snd
       (#2
        (x_1903
          (let x1_4458 = let x1_4446 = false in
                         let x2_4447 = 0 in
                         (x1_4446, x2_4447) in
           let x2_4459 = let x1_4450 = false in
                         let x2_4451 = 0 in
                         (x1_4450, x2_4451) in
           let x3_4460 = let x1_4454 = true in
                         let x2_4455 = x_4427 in
                         (x1_4454, x2_4455) in
           (x1_4458, x2_4459, x3_4460))))
compose_let
x_1905:snd
       (#1
        (x_1903
          (let x1_4440 = let x1_4428 = false in
                         let x2_4429 = 0 in
                         (x1_4428, x2_4429) in
           let x2_4441 = let x1_4432 = true in
                         let x2_4433 = x_4426 in
                         (x1_4432, x2_4433) in
           let x3_4442 = let x1_4436 = false in
                         let x2_4437 = 0 in
                         (x1_4436, x2_4437) in
           (x1_4440, x2_4441, x3_4442))))

x_1906:snd
       (#2
        (x_1903
          (let x1_4458 = let x1_4446 = false in
                         let x2_4447 = 0 in
                         (x1_4446, x2_4447) in
           let x2_4459 = let x1_4450 = false in
                         let x2_4451 = 0 in
                         (x1_4450, x2_4451) in
           let x3_4460 = let x1_4454 = true in
                         let x2_4455 = x_4427 in
                         (x1_4454, x2_4455) in
           (x1_4458, x2_4459, x3_4460))))

ADD: (x_x_4464:(int -> int -> ((bool * int) * (bool * int))))
#0
#1
tupled:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_1791 = rand_int () in
    let x_1794 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_1791)
                   else
                     x_1794 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let rec x_x_3792 x_3766 x_3767 =
    let r_3795 =
      snd
      (fst
       (x_1023
         (let x1_3776 = let x1_3768 = true in
                        let x2_3769 = x_3766 in
                        (x1_3768, x2_3769) in
          let x2_3777 = let x1_3772 = false in
                        let x2_3773 = 0 in
                        (x1_3772, x2_3773) in
          (x1_3776, x2_3777))))
    in
    let r_3796 =
      snd
      (snd
       (x_1023
         (let x1_3788 = let x1_3780 = false in
                        let x2_3781 = 0 in
                        (x1_3780, x2_3781) in
          let x2_3789 = let x1_3784 = true in
                        let x2_3785 = x_3767 in
                        (x1_3784, x2_3785) in
          (x1_3788, x2_3789))))
    in
    (r_3795, r_3796)
  in
  let x_3466 = x_1023 ((true, 0), (false, 0)) in
  let x_1811 = snd (fst x_3466) in
  if fst x_1811 = false then
    let x_1714 x_1277 = (false, 0) in
    let rec x_x_4216 x_4198 x_4199 =
      let x1_4200 = false in
      let x2_4201 = 0 in
      let r_4219 = (x1_4200, x2_4201) in
      let r_4220 =
        snd
        (snd
         (x_1023
           (let x1_4212 = let x1_4204 = false in
                          let x2_4205 = 0 in
                          (x1_4204, x2_4205) in
            let x2_4213 = let x1_4208 = true in
                          let x2_4209 = x_4199 in
                          (x1_4208, x2_4209) in
            (x1_4212, x2_4213))))
      in
      (r_4219, r_4220)
    in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          let r_4223 = x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)) in
          ((true, fst r_4223), (true, snd r_4223))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let rec x_x_4315 x_4289 x_4290 =
      let r_4318 =
        snd
        (snd
         (x_1023
           (let x1_4299 = let x1_4291 = false in
                          let x2_4292 = 0 in
                          (x1_4291, x2_4292) in
            let x2_4300 = let x1_4295 = true in
                          let x2_4296 = x_4289 in
                          (x1_4295, x2_4296) in
            (x1_4299, x2_4300))))
      in
      let r_4319 =
        snd
        (fst
         (x_1885
           (let x1_4311 = let x1_4303 = true in
                          let x2_4304 = x_4290 in
                          (x1_4303, x2_4304) in
            let x2_4312 = let x1_4307 = false in
                          let x2_4308 = 0 in
                          (x1_4307, x2_4308) in
            (x1_4311, x2_4312))))
      in
      (r_4318, r_4319)
    in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let rec x_x_4360 x_4334 x_4335 =
      let r_4363 =
        snd
        (snd
         (x_1023
           (let x1_4344 = let x1_4336 = false in
                          let x2_4337 = 0 in
                          (x1_4336, x2_4337) in
            let x2_4345 = let x1_4340 = true in
                          let x2_4341 = x_4334 in
                          (x1_4340, x2_4341) in
            (x1_4344, x2_4345))))
      in
      let r_4364 =
        snd
        (snd
         (x_1885
           (let x1_4356 = let x1_4348 = false in
                          let x2_4349 = 0 in
                          (x1_4348, x2_4349) in
            let x2_4357 = let x1_4352 = true in
                          let x2_4353 = x_4335 in
                          (x1_4352, x2_4353) in
            (x1_4356, x2_4357))))
      in
      (r_4363, r_4364)
    in
    let rec x_x_4405 x_4379 x_4380 =
      let r_4408 =
        snd
        (fst
         (x_1885
           (let x1_4389 = let x1_4381 = true in
                          let x2_4382 = x_4379 in
                          (x1_4381, x2_4382) in
            let x2_4390 = let x1_4385 = false in
                          let x2_4386 = 0 in
                          (x1_4385, x2_4386) in
            (x1_4389, x2_4390))))
      in
      let r_4409 =
        snd
        (snd
         (x_1885
           (let x1_4401 = let x1_4393 = false in
                          let x2_4394 = 0 in
                          (x1_4393, x2_4394) in
            let x2_4402 = let x1_4397 = true in
                          let x2_4398 = x_4380 in
                          (x1_4397, x2_4398) in
            (x1_4401, x2_4402))))
      in
      (r_4408, r_4409)
    in
    let rec x_x_x_4269 x_4230 x_4231 x_4232 =
      let r_4273 =
        snd
        (snd
         (x_1023
           (let x1_4241 = let x1_4233 = false in
                          let x2_4234 = 0 in
                          (x1_4233, x2_4234) in
            let x2_4242 = let x1_4237 = true in
                          let x2_4238 = x_4230 in
                          (x1_4237, x2_4238) in
            (x1_4241, x2_4242))))
      in
      let r_4274 =
        snd
        (fst
         (x_1885
           (let x1_4253 = let x1_4245 = true in
                          let x2_4246 = x_4231 in
                          (x1_4245, x2_4246) in
            let x2_4254 = let x1_4249 = false in
                          let x2_4250 = 0 in
                          (x1_4249, x2_4250) in
            (x1_4253, x2_4254))))
      in
      let r_4275 =
        snd
        (snd
         (x_1885
           (let x1_4265 = let x1_4257 = false in
                          let x2_4258 = 0 in
                          (x1_4257, x2_4258) in
            let x2_4266 = let x1_4261 = true in
                          let x2_4262 = x_4232 in
                          (x1_4261, x2_4262) in
            (x1_4265, x2_4266))))
      in
      (r_4273, r_4274, r_4275)
    in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            let r_4412 = x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (true, fst r_4412), (true, snd r_4412))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            let r_4367 = x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, fst r_4367), (false, (true, 0)), (true, snd r_4367))
        else
          if fst (#2 ixi_3377) = false then
            let r_4322 = x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) in
            ((true, fst r_4322), (true, snd r_4322), (false, (true, 0)))
          else
            let r_4279 = x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, #0 r_4279), (true, #1 r_4279), (true, #2 r_4279))
    in
    x_1891
  else
    let x_1816 = snd (fst x_3466) in
    if fst x_1816 <> false then
      let xs'_1014 x_1155 =
        let x_3221 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_1827 = snd (fst x_3221) in
        x_1827
      in
      let rec xs'_x_3837 x_3811 x_3812 =
        let x_3221 =
          x_1023
            (let x1_3821 = let x1_3813 = true in
                           let x2_3814 = x_3811 + 1 in
                           (x1_3813, x2_3814) in
             let x2_3822 = let x1_3817 = false in
                           let x2_3818 = 0 in
                           (x1_3817, x2_3818) in
             (x1_3821, x2_3822))
        in
        let x_1827 = snd (fst x_3221) in
        let r_3840 = x_1827 in
        let r_3841 =
          snd
          (snd
           (x_1023
             (let x1_3833 = let x1_3825 = false in
                            let x2_3826 = 0 in
                            (x1_3825, x2_3826) in
              let x2_3834 = let x1_3829 = true in
                            let x2_3830 = x_3812 in
                            (x1_3829, x2_3830) in
              (x1_3833, x2_3834))))
        in
        (r_3840, r_3841)
      in
      let x_1830 = snd (fst x_3466) in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            let r_3844 = xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)) in
            ((true, fst r_3844), (true, snd r_3844))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_1838 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_1838 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_1838 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_1838 ((false, 0), (false, 0), (true, i_3105)))) in
      let rec x_x_3889 x_3851 x_3852 =
        let r_3892 =
          snd
          (#1
           (x_1838
             (let x1_3865 = let x1_3853 = false in
                            let x2_3854 = 0 in
                            (x1_3853, x2_3854) in
              let x2_3866 = let x1_3857 = true in
                            let x2_3858 = x_3851 in
                            (x1_3857, x2_3858) in
              let x3_3867 = let x1_3861 = false in
                            let x2_3862 = 0 in
                            (x1_3861, x2_3862) in
              (x1_3865, x2_3866, x3_3867))))
        in
        let r_3893 =
          snd
          (#2
           (x_1838
             (let x1_3883 = let x1_3871 = false in
                            let x2_3872 = 0 in
                            (x1_3871, x2_3872) in
              let x2_3884 = let x1_3875 = false in
                            let x2_3876 = 0 in
                            (x1_3875, x2_3876) in
              let x3_3885 = let x1_3879 = true in
                            let x2_3880 = x_3852 in
                            (x1_3879, x2_3880) in
              (x1_3883, x2_3884, x3_3885))))
        in
        (r_3892, r_3893)
      in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            let r_3896 = x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)) in
            ((true, fst r_3896), (true, snd r_3896))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd x_1830)
        else
          let x_3060 = x_1838 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_1851 = snd (#0 x_3060) in
          x_1851
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd x_1830)
        else
          let x_3030 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_1862 = snd (fst x_3030) in
          x_1862
      in
      let rec x_x_3933 x_3903 x_3904 =
        if x_3903 = 0 then
          let x1_3917 = true in
          let x2_3918 = snd x_1830 in
          let r_3942 = (x1_3917, x2_3918) in
          let r_3943 =
            snd
            (snd
             (x_1844
               (let x1_3929 = let x1_3921 = false in
                              let x2_3922 = 0 in
                              (x1_3921, x2_3922) in
                let x2_3930 = let x1_3925 = true in
                              let x2_3926 = x_3904 in
                              (x1_3925, x2_3926) in
                (x1_3929, x2_3930))))
          in
          (r_3942, r_3943)
        else
          let x_3030 =
            x_1844
              (let x1_3913 = let x1_3905 = true in
                             let x2_3906 = x_3903 - 1 in
                             (x1_3905, x2_3906) in
               let x2_3914 = let x1_3909 = false in
                             let x2_3910 = 0 in
                             (x1_3909, x2_3910) in
               (x1_3913, x2_3914))
          in
          let x_1862 = snd (fst x_3030) in
          let r_3936 = x_1862 in
          let r_3937 =
            snd
            (snd
             (x_1844
               (let x1_3929 = let x1_3921 = false in
                              let x2_3922 = 0 in
                              (x1_3921, x2_3922) in
                let x2_3930 = let x1_3925 = true in
                              let x2_3926 = x_3904 in
                              (x1_3925, x2_3926) in
                (x1_3929, x2_3930))))
          in
          (r_3936, r_3937)
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            let r_3946 = x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)) in
            ((true, fst r_3946), (true, snd r_3946))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let rec x_x_4067 x_4031 x_4032 =
        if x_4031 = 0 then
          let x1_4051 = true in
          let x2_4052 = snd x_1830 in
          let r_4076 = (x1_4051, x2_4052) in
          let r_4077 =
            snd
            (fst
             (x_1871
               (let x1_4063 = let x1_4055 = true in
                              let x2_4056 = x_4032 in
                              (x1_4055, x2_4056) in
                let x2_4064 = let x1_4059 = false in
                              let x2_4060 = 0 in
                              (x1_4059, x2_4060) in
                (x1_4063, x2_4064))))
          in
          (r_4076, r_4077)
        else
          let x_3060 =
            x_1838
              (let x1_4045 = let x1_4033 = true in
                             let x2_4034 = x_4031 - 1 in
                             (x1_4033, x2_4034) in
               let x2_4046 = let x1_4037 = false in
                             let x2_4038 = 0 in
                             (x1_4037, x2_4038) in
               let x3_4047 = let x1_4041 = false in
                             let x2_4042 = 0 in
                             (x1_4041, x2_4042) in
               (x1_4045, x2_4046, x3_4047))
          in
          let x_1851 = snd (#0 x_3060) in
          let r_4070 = x_1851 in
          let r_4071 =
            snd
            (fst
             (x_1871
               (let x1_4063 = let x1_4055 = true in
                              let x2_4056 = x_4032 in
                              (x1_4055, x2_4056) in
                let x2_4064 = let x1_4059 = false in
                              let x2_4060 = 0 in
                              (x1_4059, x2_4060) in
                (x1_4063, x2_4064))))
          in
          (r_4070, r_4071)
      in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let rec x_x_4128 x_4092 x_4093 =
        if x_4092 = 0 then
          let x1_4112 = true in
          let x2_4113 = snd x_1830 in
          let r_4137 = (x1_4112, x2_4113) in
          let r_4138 =
            snd
            (snd
             (x_1871
               (let x1_4124 = let x1_4116 = false in
                              let x2_4117 = 0 in
                              (x1_4116, x2_4117) in
                let x2_4125 = let x1_4120 = true in
                              let x2_4121 = x_4093 in
                              (x1_4120, x2_4121) in
                (x1_4124, x2_4125))))
          in
          (r_4137, r_4138)
        else
          let x_3060 =
            x_1838
              (let x1_4106 = let x1_4094 = true in
                             let x2_4095 = x_4092 - 1 in
                             (x1_4094, x2_4095) in
               let x2_4107 = let x1_4098 = false in
                             let x2_4099 = 0 in
                             (x1_4098, x2_4099) in
               let x3_4108 = let x1_4102 = false in
                             let x2_4103 = 0 in
                             (x1_4102, x2_4103) in
               (x1_4106, x2_4107, x3_4108))
          in
          let x_1851 = snd (#0 x_3060) in
          let r_4131 = x_1851 in
          let r_4132 =
            snd
            (snd
             (x_1871
               (let x1_4124 = let x1_4116 = false in
                              let x2_4117 = 0 in
                              (x1_4116, x2_4117) in
                let x2_4125 = let x1_4120 = true in
                              let x2_4121 = x_4093 in
                              (x1_4120, x2_4121) in
                (x1_4124, x2_4125))))
          in
          (r_4131, r_4132)
      in
      let rec x_x_4179 x_4153 x_4154 =
        let r_4182 =
          snd
          (fst
           (x_1871
             (let x1_4163 = let x1_4155 = true in
                            let x2_4156 = x_4153 in
                            (x1_4155, x2_4156) in
              let x2_4164 = let x1_4159 = false in
                            let x2_4160 = 0 in
                            (x1_4159, x2_4160) in
              (x1_4163, x2_4164))))
        in
        let r_4183 =
          snd
          (snd
           (x_1871
             (let x1_4175 = let x1_4167 = false in
                            let x2_4168 = 0 in
                            (x1_4167, x2_4168) in
              let x2_4176 = let x1_4171 = true in
                            let x2_4172 = x_4154 in
                            (x1_4171, x2_4172) in
              (x1_4175, x2_4176))))
        in
        (r_4182, r_4183)
      in
      let rec x_x_x_4002 x_3953 x_3954 x_3955 =
        if x_3953 = 0 then
          let x1_3974 = true in
          let x2_3975 = snd x_1830 in
          let r_4015 = (x1_3974, x2_3975) in
          let r_4016 =
            snd
            (fst
             (x_1871
               (let x1_3986 = let x1_3978 = true in
                              let x2_3979 = x_3954 in
                              (x1_3978, x2_3979) in
                let x2_3987 = let x1_3982 = false in
                              let x2_3983 = 0 in
                              (x1_3982, x2_3983) in
                (x1_3986, x2_3987))))
          in
          let r_4017 =
            snd
            (snd
             (x_1871
               (let x1_3998 = let x1_3990 = false in
                              let x2_3991 = 0 in
                              (x1_3990, x2_3991) in
                let x2_3999 = let x1_3994 = true in
                              let x2_3995 = x_3955 in
                              (x1_3994, x2_3995) in
                (x1_3998, x2_3999))))
          in
          (r_4015, r_4016, r_4017)
        else
          let x_3060 =
            x_1838
              (let x1_3968 = let x1_3956 = true in
                             let x2_3957 = x_3953 - 1 in
                             (x1_3956, x2_3957) in
               let x2_3969 = let x1_3960 = false in
                             let x2_3961 = 0 in
                             (x1_3960, x2_3961) in
               let x3_3970 = let x1_3964 = false in
                             let x2_3965 = 0 in
                             (x1_3964, x2_3965) in
               (x1_3968, x2_3969, x3_3970))
          in
          let x_1851 = snd (#0 x_3060) in
          let r_4006 = x_1851 in
          let r_4007 =
            snd
            (fst
             (x_1871
               (let x1_3986 = let x1_3978 = true in
                              let x2_3979 = x_3954 in
                              (x1_3978, x2_3979) in
                let x2_3987 = let x1_3982 = false in
                              let x2_3983 = 0 in
                              (x1_3982, x2_3983) in
                (x1_3986, x2_3987))))
          in
          let r_4008 =
            snd
            (snd
             (x_1871
               (let x1_3998 = let x1_3990 = false in
                              let x2_3991 = 0 in
                              (x1_3990, x2_3991) in
                let x2_3999 = let x1_3994 = true in
                              let x2_3995 = x_3955 in
                              (x1_3994, x2_3995) in
                (x1_3998, x2_3999))))
          in
          (r_4006, r_4007, r_4008)
      in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              let r_4186 = x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((false, (true, 0)), (true, fst r_4186), (true, snd r_4186))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              let r_4141 = x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)) in
              ((true, fst r_4141), (false, (true, 0)), (true, snd r_4141))
          else
            if fst (#2 iii_2941) = false then
              let r_4080 = x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)) in
              ((true, fst r_4080), (true, snd r_4080), (false, (true, 0)))
            else
              let r_4021 = x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((true, #0 r_4021), (true, #1 r_4021), (true, #2 r_4021))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              let r_3799 = x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)) in
              ((false, (true, 0)), (true, fst r_3799), (true, snd r_3799))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              ((true, x_1661 (snd (#0 iii_2532))), (true, x_1809 (snd (#1 iii_2532))), 
               (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_1892 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_1892 (snd (fst ix_2298))), (false, (true, 0)))
      else
        ((true, x_1892 (snd (fst ix_2298))), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_1903 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_1903 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_1903 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_1903 ((false, 0), (false, 0), (true, i_2240)))) in
  let rec x_x_4464 x_4426 x_4427 =
    let r_4467 =
      snd
      (#1
       (x_1903
         (let x1_4440 = let x1_4428 = false in
                        let x2_4429 = 0 in
                        (x1_4428, x2_4429) in
          let x2_4441 = let x1_4432 = true in
                        let x2_4433 = x_4426 in
                        (x1_4432, x2_4433) in
          let x3_4442 = let x1_4436 = false in
                        let x2_4437 = 0 in
                        (x1_4436, x2_4437) in
          (x1_4440, x2_4441, x3_4442))))
    in
    let r_4468 =
      snd
      (#2
       (x_1903
         (let x1_4458 = let x1_4446 = false in
                        let x2_4447 = 0 in
                        (x1_4446, x2_4447) in
          let x2_4459 = let x1_4450 = false in
                        let x2_4451 = 0 in
                        (x1_4450, x2_4451) in
          let x3_4460 = let x1_4454 = true in
                        let x2_4455 = x_4427 in
                        (x1_4454, x2_4455) in
          (x1_4458, x2_4459, x3_4460))))
    in
    (r_4467, r_4468)
  in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        let r_4471 = x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)) in
        ((true, fst r_4471), (true, snd r_4471))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_2195 = x_1903 ((true, i_1016), (false, 0), (false, 0)) in
  let x_2165 = x_1909 ((true, i_1016), (false, 0)) in
  let x_1912 = snd (#0 x_2195) in
  let n_1595 = if fst x_1912 <> false then
                 snd x_1912
               else
                 _|_ in
  let x_1917 = snd (fst x_2165) in
  let n_1596 = if fst x_1917 <> false then
                 snd x_1917
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_1924 = rand_int () in
let x_1925 = rand_int () in
let x_1926 = main_1015 x_1924 in
let x_1927 = x_1926 x_1925 in
()

normalize:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_4479 = rand_int () in
    let x_4482 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_4479)
                   else
                     x_4482 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = let x_4515 = x_1023 ((true, i_3474), (false, 0)) in
                      snd (fst x_4515) in
  let x_1810 i_3467 = let x_4534 = x_1023 ((false, 0), (true, i_3467)) in
                      snd (snd x_4534) in
  let rec x_x_3792 x_3766 x_3767 =
    let x_4548 = x_1023 ((true, x_3766), (false, 0)) in
    let x_4562 = x_1023 ((false, 0), (true, x_3767)) in
    (snd (fst x_4548), snd (snd x_4562))
  in
  let x_4583 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_4583)) = false then
    let x_1714 x_1277 = (false, 0) in
    let rec x_x_4216 x_4198 x_4199 = let x_6261 = x_1023 ((false, 0), (true, x_4199)) in
                                     ((false, 0), snd (snd x_6261)) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          let x_6338 = x_1810 (snd (snd xi_3429)) in
          ((false, (true, 0)), (true, x_6338))
      else
        if fst (snd xi_3429) = false then
          let x_6297 = x_1714 (snd (fst xi_3429)) in
          ((true, x_6297), (false, (true, 0)))
        else
          let x_6273 = x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)) in
          ((true, fst x_6273), (true, snd x_6273))
    in
    let x_1886 x_3409 = let x_6398 = x_1885 ((true, x_3409), (false, 0)) in
                        snd (fst x_6398) in
    let rec x_x_4315 x_4289 x_4290 =
      let x_6412 = x_1023 ((false, 0), (true, x_4289)) in
      let x_6426 = x_1885 ((true, x_4290), (false, 0)) in
      (snd (snd x_6412), snd (fst x_6426))
    in
    let x_1887 i_3402 = let x_6448 = x_1885 ((false, 0), (true, i_3402)) in
                        snd (snd x_6448) in
    let rec x_x_4360 x_4334 x_4335 =
      let x_6462 = x_1023 ((false, 0), (true, x_4334)) in
      let x_6476 = x_1885 ((false, 0), (true, x_4335)) in
      (snd (snd x_6462), snd (snd x_6476))
    in
    let rec x_x_4405 x_4379 x_4380 =
      let x_6493 = x_1885 ((true, x_4379), (false, 0)) in
      let x_6507 = x_1885 ((false, 0), (true, x_4380)) in
      (snd (fst x_6493), snd (snd x_6507))
    in
    let rec x_x_x_4269 x_4230 x_4231 x_4232 =
      let x_6524 = x_1023 ((false, 0), (true, x_4230)) in
      let x_6538 = x_1885 ((true, x_4231), (false, 0)) in
      let x_6552 = x_1885 ((false, 0), (true, x_4232)) in
      (snd (snd x_6524), snd (fst x_6538), snd (snd x_6552))
    in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6826 = x_1887 (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6826))
        else
          if fst (#2 ixi_3377) = false then
            let x_6773 = x_1886 (snd (#1 ixi_3377)) in
            ((false, (true, 0)), (true, x_6773), (false, (true, 0)))
          else
            let x_6726 = x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (true, fst x_6726), (true, snd x_6726))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            let x_6678 = x_1810 (snd (#0 ixi_3377)) in
            ((true, x_6678), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6642 = x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, fst x_6642), (false, (true, 0)), (true, snd x_6642))
        else
          if fst (#2 ixi_3377) = false then
            let x_6600 = x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) in
            ((true, fst x_6600), (true, snd x_6600), (false, (true, 0)))
          else
            let x_6568 = x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, #0 x_6568), (true, #1 x_6568), (true, #2 x_6568))
    in
    x_1891
  else
    if fst (snd (fst x_4583)) <> false then
      let xs'_1014 x_1155 = let x_4934 = x_1023 ((true, x_1155 + 1), (false, 0)) in
                            snd (fst x_4934) in
      let rec xs'_x_3837 x_3811 x_3812 =
        let x_4949 = x_1023 ((true, x_3811 + 1), (false, 0)) in
        let x_4964 = x_1023 ((false, 0), (true, x_3812)) in
        (snd (fst x_4949), snd (snd x_4964))
      in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5044 = x_1810 (snd (snd ii_3163)) in
            ((false, (true, 0)), (true, x_5044))
        else
          if fst (snd ii_3163) = false then
            let x_5003 = xs'_1014 (snd (fst ii_3163)) in
            ((true, x_5003), (false, (true, 0)))
          else
            let x_4979 = xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)) in
            ((true, fst x_4979), (true, snd x_4979))
      in
      let x_1836 i_3143 = let x_5104 = x_1835 ((true, i_3143), (false, 0)) in
                          snd (fst x_5104) in
      let x_1837 i_3136 = let x_5123 = x_1835 ((false, 0), (true, i_3136)) in
                          snd (snd x_5123) in
      let x_5126 = append_1059 x_1835 in
      let x_1839 i_3125 = let x_5150 = x_5126 ((true, i_3125), (false, 0), (false, 0)) in
                          snd (#0 x_5150) in
      let x_1840 i_3115 = let x_5176 = x_5126 ((false, 0), (true, i_3115), (false, 0)) in
                          snd (#1 x_5176) in
      let x_1841 i_3105 = let x_5202 = x_5126 ((false, 0), (false, 0), (true, i_3105)) in
                          snd (#2 x_5202) in
      let rec x_x_3889 x_3851 x_3852 =
        let x_5220 = x_5126 ((false, 0), (true, x_3851), (false, 0)) in
        let x_5238 = x_5126 ((false, 0), (false, 0), (true, x_3852)) in
        (snd (#1 x_5220), snd (#2 x_5238))
      in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5315 = x_1841 (snd (snd ii_3088)) in
            ((false, (true, 0)), (true, x_5315))
        else
          if fst (snd ii_3088) = false then
            let x_5274 = x_1840 (snd (fst ii_3088)) in
            ((true, x_5274), (false, (true, 0)))
          else
            let x_5250 = x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)) in
            ((true, fst x_5250), (true, snd x_5250))
      in
      let x_1845 i_3068 = let x_5375 = x_1844 ((true, i_3068), (false, 0)) in
                          snd (fst x_5375) in
      let x_1846 i_3061 = let x_5394 = x_1844 ((false, 0), (true, i_3061)) in
                          snd (snd x_5394) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd (snd (fst x_4583)))
        else
          let x_5421 = x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          snd (#0 x_5421)
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd (snd (fst x_4583)))
        else
          let x_5450 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          snd (fst x_5450)
      in
      let rec x_x_3933 x_3903 x_3904 =
        if x_3903 = 0 then
          let x_5509 = x_1844 ((false, 0), (true, x_3904)) in
          ((true, snd (snd (fst x_4583))), snd (snd x_5509))
        else
          let x_5473 = x_1844 ((true, x_3903 - 1), (false, 0)) in
          let x_5488 = x_1844 ((false, 0), (true, x_3904)) in
          (snd (fst x_5473), snd (snd x_5488))
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5588 = x_1846 (snd (snd ii_2993)) in
            ((false, (true, 0)), (true, x_5588))
        else
          if fst (snd ii_2993) = false then
            let x_5547 = x_1694 (snd (fst ii_2993)) in
            ((true, x_5547), (false, (true, 0)))
          else
            let x_5523 = x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)) in
            ((true, fst x_5523), (true, snd x_5523))
      in
      let x_1872 i_2973 = let x_5648 = x_1871 ((true, i_2973), (false, 0)) in
                          snd (fst x_5648) in
      let rec x_x_4067 x_4031 x_4032 =
        if x_4031 = 0 then
          let x_5702 = x_1871 ((true, x_4032), (false, 0)) in
          ((true, snd (snd (fst x_4583))), snd (fst x_5702))
        else
          let x_5666 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)) in
          let x_5681 = x_1871 ((true, x_4032), (false, 0)) in
          (snd (#0 x_5666), snd (fst x_5681))
      in
      let x_1873 i_2966 = let x_5726 = x_1871 ((false, 0), (true, i_2966)) in
                          snd (snd x_5726) in
      let rec x_x_4128 x_4092 x_4093 =
        if x_4092 = 0 then
          let x_5780 = x_1871 ((false, 0), (true, x_4093)) in
          ((true, snd (snd (fst x_4583))), snd (snd x_5780))
        else
          let x_5744 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)) in
          let x_5759 = x_1871 ((false, 0), (true, x_4093)) in
          (snd (#0 x_5744), snd (snd x_5759))
      in
      let rec x_x_4179 x_4153 x_4154 =
        let x_5799 = x_1871 ((true, x_4153), (false, 0)) in
        let x_5813 = x_1871 ((false, 0), (true, x_4154)) in
        (snd (fst x_5799), snd (snd x_5813))
      in
      let rec x_x_x_4002 x_3953 x_3954 x_3955 =
        if x_3953 = 0 then
          let x_5885 = x_1871 ((true, x_3954), (false, 0)) in
          let x_5899 = x_1871 ((false, 0), (true, x_3955)) in
          ((true, snd (snd (fst x_4583))), snd (fst x_5885), snd (snd x_5899))
        else
          let x_5834 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)) in
          let x_5849 = x_1871 ((true, x_3954), (false, 0)) in
          let x_5863 = x_1871 ((false, 0), (true, x_3955)) in
          (snd (#0 x_5834), snd (fst x_5849), snd (snd x_5863))
      in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6175 = x_1873 (snd (#2 iii_2941)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6175))
          else
            if fst (#2 iii_2941) = false then
              let x_6122 = x_1872 (snd (#1 iii_2941)) in
              ((false, (true, 0)), (true, x_6122), (false, (true, 0)))
            else
              let x_6075 = x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((false, (true, 0)), (true, fst x_6075), (true, snd x_6075))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              let x_6027 = x_1700 (snd (#0 iii_2941)) in
              ((true, x_6027), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5991 = x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)) in
              ((true, fst x_5991), (false, (true, 0)), (true, snd x_5991))
          else
            if fst (#2 iii_2941) = false then
              let x_5949 = x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)) in
              ((true, fst x_5949), (true, snd x_5949), (false, (true, 0)))
            else
              let x_5917 = x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((true, #0 x_5917), (true, #1 x_5917), (true, #2 x_5917))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4854 = x_1810 (snd (#2 iii_2532)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4854))
          else
            if fst (#2 iii_2532) = false then
              let x_4801 = x_1809 (snd (#1 iii_2532)) in
              ((false, (true, 0)), (true, x_4801), (false, (true, 0)))
            else
              let x_4754 = x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)) in
              ((false, (true, 0)), (true, fst x_4754), (true, snd x_4754))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              let x_4706 = x_1661 (snd (#0 iii_2532)) in
              ((true, x_4706), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4671 = x_1661 (snd (#0 iii_2532)) in
              let x_4692 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4671), (false, (true, 0)), (true, x_4692))
          else
            if fst (#2 iii_2532) = false then
              let x_4630 = x_1661 (snd (#0 iii_2532)) in
              let x_4640 = x_1809 (snd (#1 iii_2532)) in
              ((true, x_4630), (true, x_4640), (false, (true, 0)))
            else
              let x_4596 = x_1661 (snd (#0 iii_2532)) in
              let x_4606 = x_1809 (snd (#1 iii_2532)) in
              let x_4616 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4596), (true, x_4606), (true, x_4616))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_6893 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6969 = f_1570 (snd (snd ix_2298)) in
        ((false, (true, 0)), (true, x_6969))
    else
      if fst (snd ix_2298) = false then
        let x_6928 = x_6893 (snd (fst ix_2298)) in
        ((true, x_6928), (false, (true, 0)))
      else
        let x_6905 = x_6893 (snd (fst ix_2298)) in
        let x_6915 = f_1570 (snd (snd ix_2298)) in
        ((true, x_6905), (true, x_6915))
  in
  let x_1901 i_2278 = let x_7029 = x_1900 ((true, i_2278), (false, 0)) in
                      snd (fst x_7029) in
  let x_1902 x_2271 = let x_7048 = x_1900 ((false, 0), (true, x_2271)) in
                      snd (snd x_7048) in
  let x_7051 = append_1059 x_1900 in
  let x_1904 i_2260 = let x_7075 = x_7051 ((true, i_2260), (false, 0), (false, 0)) in
                      snd (#0 x_7075) in
  let x_1905 i_2250 = let x_7101 = x_7051 ((false, 0), (true, i_2250), (false, 0)) in
                      snd (#1 x_7101) in
  let x_1906 i_2240 = let x_7127 = x_7051 ((false, 0), (false, 0), (true, i_2240)) in
                      snd (#2 x_7127) in
  let rec x_x_4464 x_4426 x_4427 =
    let x_7145 = x_7051 ((false, 0), (true, x_4426), (false, 0)) in
    let x_7163 = x_7051 ((false, 0), (false, 0), (true, x_4427)) in
    (snd (#1 x_7145), snd (#2 x_7163))
  in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_7240 = x_1906 (snd (snd ii_2223)) in
        ((false, (true, 0)), (true, x_7240))
    else
      if fst (snd ii_2223) = false then
        let x_7199 = x_1905 (snd (fst ii_2223)) in
        ((true, x_7199), (false, (true, 0)))
      else
        let x_7175 = x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)) in
        ((true, fst x_7175), (true, snd x_7175))
  in
  let x_1910 i_2203 = let x_7300 = x_1909 ((true, i_2203), (false, 0)) in
                      snd (fst x_7300) in
  let x_1911 i_2196 = let x_7319 = x_1909 ((false, 0), (true, i_2196)) in
                      snd (snd x_7319) in
  let x_7343 = x_7051 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7358 = x_1909 ((true, i_1016), (false, 0)) in
  let n_1595 = if fst (snd (#0 x_7343)) <> false then
                 snd (snd (#0 x_7343))
               else
                 _|_ in
  let n_1596 = if fst (snd (fst x_7358)) <> false then
                 snd (snd (fst x_7358))
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_7379 = rand_int () in
let x_7381 = rand_int () in
let x_7382 = main_1015 x_7379 in
let x_7383 = x_7382 x_7381 in
let x_1927 = x_7383 in
()

replace[1]: x_7358
APPS: x_7358 = x_1909 ...0... i_1016 ...
USED: x_7358 = x_1909 ...0... i_1016 ...
MUST: x_7358 = x_1909 ...0... i_1016 ...
NEW: x_7384 = x_1909 ((true, i_1016), (false, 0))
replace[1]: x_7343
APPS: x_7343 = x_7051 ...0... i_1016 ...
USED: x_7343 = x_7051 ...0... i_1016 ...
MUST: x_7343 = x_7051 ...0... i_1016 ...
NEW: x_7392 = x_7051 ((true, i_1016), (false, 0), (false, 0))
replace[1]: x_7319
APPS: x_7319 = x_1909 ...1... i_2196 ...
USED: x_7319 = x_1909 ...1... i_2196 ...
MUST: x_7319 = x_1909 ...1... i_2196 ...
NEW: x_7403 = x_1909 ((false, 0), (true, i_2196))
replace[1]: x_7300
APPS: x_7300 = x_1909 ...0... i_2203 ...
USED: x_7300 = x_1909 ...0... i_2203 ...
MUST: x_7300 = x_1909 ...0... i_2203 ...
NEW: x_7411 = x_1909 ((true, i_2203), (false, 0))
replace[2]: x_7145
APPS: x_7163 = x_7051 ...2... x_4427 ...
APPS: x_7145 = x_7051 ...1... x_4426 ...
USED: x_7163 = x_7051 ...2... x_4427 ...
USED: x_7145 = x_7051 ...1... x_4426 ...
MUST: x_7145 = x_7051 ...1... x_4426 ...
MUST: x_7163 = x_7051 ...2... x_4427 ...
NEW: x_7419 = x_7051 ((false, 0), (true, x_4426), (true, x_4427))
replace[1]: x_7127
APPS: x_7127 = x_7051 ...2... i_2240 ...
USED: x_7127 = x_7051 ...2... i_2240 ...
MUST: x_7127 = x_7051 ...2... i_2240 ...
NEW: x_7431 = x_7051 ((false, 0), (false, 0), (true, i_2240))
replace[1]: x_7101
APPS: x_7101 = x_7051 ...1... i_2250 ...
USED: x_7101 = x_7051 ...1... i_2250 ...
MUST: x_7101 = x_7051 ...1... i_2250 ...
NEW: x_7442 = x_7051 ((false, 0), (true, i_2250), (false, 0))
replace[1]: x_7075
APPS: x_7075 = x_7051 ...0... i_2260 ...
USED: x_7075 = x_7051 ...0... i_2260 ...
MUST: x_7075 = x_7051 ...0... i_2260 ...
NEW: x_7453 = x_7051 ((true, i_2260), (false, 0), (false, 0))
replace[1]: x_7048
APPS: x_7048 = x_1900 ...1... x_2271 ...
USED: x_7048 = x_1900 ...1... x_2271 ...
MUST: x_7048 = x_1900 ...1... x_2271 ...
NEW: x_7464 = x_1900 ((false, 0), (true, x_2271))
replace[1]: x_7029
APPS: x_7029 = x_1900 ...0... i_2278 ...
USED: x_7029 = x_1900 ...0... i_2278 ...
MUST: x_7029 = x_1900 ...0... i_2278 ...
NEW: x_7472 = x_1900 ((true, i_2278), (false, 0))
replace[2]: x_5849
APPS: x_5863 = x_1871 ...1... x_3955 ...
APPS: x_5849 = x_1871 ...0... x_3954 ...
USED: x_5863 = x_1871 ...1... x_3955 ...
USED: x_5849 = x_1871 ...0... x_3954 ...
MUST: x_5849 = x_1871 ...0... x_3954 ...
MUST: x_5863 = x_1871 ...1... x_3955 ...
NEW: x_7480 = x_1871 ((true, x_3954), (true, x_3955))
replace[1]: x_5834
APPS: x_5834 = x_5126 ...0... x_3953 - 1 ...
USED: x_5834 = x_5126 ...0... x_3953 - 1 ...
MUST: x_5834 = x_5126 ...0... x_3953 - 1 ...
NEW: x_7489 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0))
replace[2]: x_5885
APPS: x_5899 = x_1871 ...1... x_3955 ...
APPS: x_5885 = x_1871 ...0... x_3954 ...
USED: x_5899 = x_1871 ...1... x_3955 ...
USED: x_5885 = x_1871 ...0... x_3954 ...
MUST: x_5885 = x_1871 ...0... x_3954 ...
MUST: x_5899 = x_1871 ...1... x_3955 ...
NEW: x_7500 = x_1871 ((true, x_3954), (true, x_3955))
replace[2]: x_5799
APPS: x_5813 = x_1871 ...1... x_4154 ...
APPS: x_5799 = x_1871 ...0... x_4153 ...
USED: x_5813 = x_1871 ...1... x_4154 ...
USED: x_5799 = x_1871 ...0... x_4153 ...
MUST: x_5799 = x_1871 ...0... x_4153 ...
MUST: x_5813 = x_1871 ...1... x_4154 ...
NEW: x_7509 = x_1871 ((true, x_4153), (true, x_4154))
replace[1]: x_5759
APPS: x_5759 = x_1871 ...1... x_4093 ...
USED: x_5759 = x_1871 ...1... x_4093 ...
MUST: x_5759 = x_1871 ...1... x_4093 ...
NEW: x_7518 = x_1871 ((false, 0), (true, x_4093))
replace[1]: x_5744
APPS: x_5744 = x_5126 ...0... x_4092 - 1 ...
USED: x_5744 = x_5126 ...0... x_4092 - 1 ...
MUST: x_5744 = x_5126 ...0... x_4092 - 1 ...
NEW: x_7526 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0))
replace[1]: x_5780
APPS: x_5780 = x_1871 ...1... x_4093 ...
USED: x_5780 = x_1871 ...1... x_4093 ...
MUST: x_5780 = x_1871 ...1... x_4093 ...
NEW: x_7537 = x_1871 ((false, 0), (true, x_4093))
replace[1]: x_5726
APPS: x_5726 = x_1871 ...1... i_2966 ...
USED: x_5726 = x_1871 ...1... i_2966 ...
MUST: x_5726 = x_1871 ...1... i_2966 ...
NEW: x_7545 = x_1871 ((false, 0), (true, i_2966))
replace[1]: x_5681
APPS: x_5681 = x_1871 ...0... x_4032 ...
USED: x_5681 = x_1871 ...0... x_4032 ...
MUST: x_5681 = x_1871 ...0... x_4032 ...
NEW: x_7553 = x_1871 ((true, x_4032), (false, 0))
replace[1]: x_5666
APPS: x_5666 = x_5126 ...0... x_4031 - 1 ...
USED: x_5666 = x_5126 ...0... x_4031 - 1 ...
MUST: x_5666 = x_5126 ...0... x_4031 - 1 ...
NEW: x_7561 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0))
replace[1]: x_5702
APPS: x_5702 = x_1871 ...0... x_4032 ...
USED: x_5702 = x_1871 ...0... x_4032 ...
MUST: x_5702 = x_1871 ...0... x_4032 ...
NEW: x_7572 = x_1871 ((true, x_4032), (false, 0))
replace[1]: x_5648
APPS: x_5648 = x_1871 ...0... i_2973 ...
USED: x_5648 = x_1871 ...0... i_2973 ...
MUST: x_5648 = x_1871 ...0... i_2973 ...
NEW: x_7580 = x_1871 ((true, i_2973), (false, 0))
replace[2]: x_5473
APPS: x_5488 = x_1844 ...1... x_3904 ...
APPS: x_5473 = x_1844 ...0... x_3903 - 1 ...
USED: x_5488 = x_1844 ...1... x_3904 ...
USED: x_5473 = x_1844 ...0... x_3903 - 1 ...
MUST: x_5473 = x_1844 ...0... x_3903 - 1 ...
MUST: x_5488 = x_1844 ...1... x_3904 ...
NEW: x_7588 = x_1844 ((true, x_3903 - 1), (true, x_3904))
replace[1]: x_5509
APPS: x_5509 = x_1844 ...1... x_3904 ...
USED: x_5509 = x_1844 ...1... x_3904 ...
MUST: x_5509 = x_1844 ...1... x_3904 ...
NEW: x_7597 = x_1844 ((false, 0), (true, x_3904))
replace[1]: x_5450
APPS: x_5450 = x_1844 ...0... i_1248 - 1 ...
USED: x_5450 = x_1844 ...0... i_1248 - 1 ...
MUST: x_5450 = x_1844 ...0... i_1248 - 1 ...
NEW: x_7605 = x_1844 ((true, i_1248 - 1), (false, 0))
replace[1]: x_5421
APPS: x_5421 = x_5126 ...0... i_1231 - 1 ...
USED: x_5421 = x_5126 ...0... i_1231 - 1 ...
MUST: x_5421 = x_5126 ...0... i_1231 - 1 ...
NEW: x_7613 = x_5126 ((true, i_1231 - 1), (false, 0), (false, 0))
replace[1]: x_5394
APPS: x_5394 = x_1844 ...1... i_3061 ...
USED: x_5394 = x_1844 ...1... i_3061 ...
MUST: x_5394 = x_1844 ...1... i_3061 ...
NEW: x_7624 = x_1844 ((false, 0), (true, i_3061))
replace[1]: x_5375
APPS: x_5375 = x_1844 ...0... i_3068 ...
USED: x_5375 = x_1844 ...0... i_3068 ...
MUST: x_5375 = x_1844 ...0... i_3068 ...
NEW: x_7632 = x_1844 ((true, i_3068), (false, 0))
replace[2]: x_5220
APPS: x_5238 = x_5126 ...2... x_3852 ...
APPS: x_5220 = x_5126 ...1... x_3851 ...
USED: x_5238 = x_5126 ...2... x_3852 ...
USED: x_5220 = x_5126 ...1... x_3851 ...
MUST: x_5220 = x_5126 ...1... x_3851 ...
MUST: x_5238 = x_5126 ...2... x_3852 ...
NEW: x_7640 = x_5126 ((false, 0), (true, x_3851), (true, x_3852))
replace[1]: x_5202
APPS: x_5202 = x_5126 ...2... i_3105 ...
USED: x_5202 = x_5126 ...2... i_3105 ...
MUST: x_5202 = x_5126 ...2... i_3105 ...
NEW: x_7652 = x_5126 ((false, 0), (false, 0), (true, i_3105))
replace[1]: x_5176
APPS: x_5176 = x_5126 ...1... i_3115 ...
USED: x_5176 = x_5126 ...1... i_3115 ...
MUST: x_5176 = x_5126 ...1... i_3115 ...
NEW: x_7663 = x_5126 ((false, 0), (true, i_3115), (false, 0))
replace[1]: x_5150
APPS: x_5150 = x_5126 ...0... i_3125 ...
USED: x_5150 = x_5126 ...0... i_3125 ...
MUST: x_5150 = x_5126 ...0... i_3125 ...
NEW: x_7674 = x_5126 ((true, i_3125), (false, 0), (false, 0))
replace[1]: x_5123
APPS: x_5123 = x_1835 ...1... i_3136 ...
USED: x_5123 = x_1835 ...1... i_3136 ...
MUST: x_5123 = x_1835 ...1... i_3136 ...
NEW: x_7685 = x_1835 ((false, 0), (true, i_3136))
replace[1]: x_5104
APPS: x_5104 = x_1835 ...0... i_3143 ...
USED: x_5104 = x_1835 ...0... i_3143 ...
MUST: x_5104 = x_1835 ...0... i_3143 ...
NEW: x_7693 = x_1835 ((true, i_3143), (false, 0))
replace[2]: x_4949
APPS: x_4964 = x_1023 ...1... x_3812 ...
APPS: x_4949 = x_1023 ...0... x_3811 + 1 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_4964 = x_1023 ...1... x_3812 ...
USED: x_4949 = x_1023 ...0... x_3811 + 1 ...
MUST: x_4949 = x_1023 ...0... x_3811 + 1 ...
MUST: x_4964 = x_1023 ...1... x_3812 ...
NEW: x_7701 = x_1023 ((true, x_3811 + 1), (true, x_3812))
replace[1]: x_4934
APPS: x_4934 = x_1023 ...0... x_1155 + 1 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_4934 = x_1023 ...0... x_1155 + 1 ...
MUST: x_4934 = x_1023 ...0... x_1155 + 1 ...
NEW: x_7710 = x_1023 ((true, x_1155 + 1), (false, 0))
replace[2]: x_6538
APPS: x_6552 = x_1885 ...1... x_4232 ...
APPS: x_6538 = x_1885 ...0... x_4231 ...
USED: x_6552 = x_1885 ...1... x_4232 ...
USED: x_6538 = x_1885 ...0... x_4231 ...
MUST: x_6538 = x_1885 ...0... x_4231 ...
MUST: x_6552 = x_1885 ...1... x_4232 ...
NEW: x_7718 = x_1885 ((true, x_4231), (true, x_4232))
replace[1]: x_6524
APPS: x_6524 = x_1023 ...1... x_4230 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_6524 = x_1023 ...1... x_4230 ...
MUST: x_6524 = x_1023 ...1... x_4230 ...
NEW: x_7727 = x_1023 ((false, 0), (true, x_4230))
replace[2]: x_6493
APPS: x_6507 = x_1885 ...1... x_4380 ...
APPS: x_6493 = x_1885 ...0... x_4379 ...
USED: x_6507 = x_1885 ...1... x_4380 ...
USED: x_6493 = x_1885 ...0... x_4379 ...
MUST: x_6493 = x_1885 ...0... x_4379 ...
MUST: x_6507 = x_1885 ...1... x_4380 ...
NEW: x_7735 = x_1885 ((true, x_4379), (true, x_4380))
replace[1]: x_6476
APPS: x_6476 = x_1885 ...1... x_4335 ...
USED: x_6476 = x_1885 ...1... x_4335 ...
MUST: x_6476 = x_1885 ...1... x_4335 ...
NEW: x_7744 = x_1885 ((false, 0), (true, x_4335))
replace[1]: x_6462
APPS: x_6462 = x_1023 ...1... x_4334 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_6462 = x_1023 ...1... x_4334 ...
MUST: x_6462 = x_1023 ...1... x_4334 ...
NEW: x_7752 = x_1023 ((false, 0), (true, x_4334))
replace[1]: x_6448
APPS: x_6448 = x_1885 ...1... i_3402 ...
USED: x_6448 = x_1885 ...1... i_3402 ...
MUST: x_6448 = x_1885 ...1... i_3402 ...
NEW: x_7760 = x_1885 ((false, 0), (true, i_3402))
replace[1]: x_6426
APPS: x_6426 = x_1885 ...0... x_4290 ...
USED: x_6426 = x_1885 ...0... x_4290 ...
MUST: x_6426 = x_1885 ...0... x_4290 ...
NEW: x_7768 = x_1885 ((true, x_4290), (false, 0))
replace[1]: x_6412
APPS: x_6412 = x_1023 ...1... x_4289 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_6412 = x_1023 ...1... x_4289 ...
MUST: x_6412 = x_1023 ...1... x_4289 ...
NEW: x_7776 = x_1023 ((false, 0), (true, x_4289))
replace[1]: x_6398
APPS: x_6398 = x_1885 ...0... x_3409 ...
USED: x_6398 = x_1885 ...0... x_3409 ...
MUST: x_6398 = x_1885 ...0... x_3409 ...
NEW: x_7784 = x_1885 ((true, x_3409), (false, 0))
replace[1]: x_6261
APPS: x_6261 = x_1023 ...1... x_4199 ...
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_6261 = x_1023 ...1... x_4199 ...
MUST: x_6261 = x_1023 ...1... x_4199 ...
NEW: x_7792 = x_1023 ((false, 0), (true, x_4199))
replace[1]: x_4583
APPS: x_4583 = x_1023 ...0... 0 ...
USED: x_4583 = x_1023 ...0... 0 ...
MUST: x_4583 = x_1023 ...0... 0 ...
NEW: x_7800 = x_1023 ((true, 0), (false, 0))
replace[2]: x_4548
APPS: x_4562 = x_1023 ...1... x_3767 ...
APPS: x_4548 = x_1023 ...0... x_3766 ...
USED: x_4562 = x_1023 ...1... x_3767 ...
USED: x_4548 = x_1023 ...0... x_3766 ...
MUST: x_4548 = x_1023 ...0... x_3766 ...
MUST: x_4562 = x_1023 ...1... x_3767 ...
NEW: x_7808 = x_1023 ((true, x_3766), (true, x_3767))
replace[1]: x_4534
APPS: x_4534 = x_1023 ...1... i_3467 ...
USED: x_4534 = x_1023 ...1... i_3467 ...
MUST: x_4534 = x_1023 ...1... i_3467 ...
NEW: x_7817 = x_1023 ((false, 0), (true, i_3467))
replace[1]: x_4515
APPS: x_4515 = x_1023 ...0... i_3474 ...
USED: x_4515 = x_1023 ...0... i_3474 ...
MUST: x_4515 = x_1023 ...0... i_3474 ...
NEW: x_7825 = x_1023 ((true, i_3474), (false, 0))
replace_app:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_4479 = rand_int () in
    let x_4482 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_4479)
                   else
                     x_4482 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 =
    let x_4515 = x_1023 ((true, i_3474), (false, 0)) in
    let x_7825 = x_1023 ((true, i_3474), (false, 0)) in
    snd (fst x_7825)
  in
  let x_1810 i_3467 =
    let x_4534 = x_1023 ((false, 0), (true, i_3467)) in
    let x_7817 = x_1023 ((false, 0), (true, i_3467)) in
    snd (snd x_7817)
  in
  let rec x_x_3792 x_3766 x_3767 =
    let x_4548 = x_1023 ((true, x_3766), (false, 0)) in
    let x_4562 = x_1023 ((false, 0), (true, x_3767)) in
    let x_7808 = x_1023 ((true, x_3766), (true, x_3767)) in
    (snd (fst x_7808), snd (snd x_7808))
  in
  let x_4583 = x_1023 ((true, 0), (false, 0)) in
  let x_7800 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7800)) = false then
    let x_1714 x_1277 = (false, 0) in
    let rec x_x_4216 x_4198 x_4199 =
      let x_6261 = x_1023 ((false, 0), (true, x_4199)) in
      let x_7792 = x_1023 ((false, 0), (true, x_4199)) in
      ((false, 0), snd (snd x_7792))
    in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          let x_6338 = x_1810 (snd (snd xi_3429)) in
          ((false, (true, 0)), (true, x_6338))
      else
        if fst (snd xi_3429) = false then
          let x_6297 = x_1714 (snd (fst xi_3429)) in
          ((true, x_6297), (false, (true, 0)))
        else
          let x_6273 = x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)) in
          ((true, fst x_6273), (true, snd x_6273))
    in
    let x_1886 x_3409 =
      let x_6398 = x_1885 ((true, x_3409), (false, 0)) in
      let x_7784 = x_1885 ((true, x_3409), (false, 0)) in
      snd (fst x_7784)
    in
    let rec x_x_4315 x_4289 x_4290 =
      let x_6412 = x_1023 ((false, 0), (true, x_4289)) in
      let x_7776 = x_1023 ((false, 0), (true, x_4289)) in
      let x_6426 = x_1885 ((true, x_4290), (false, 0)) in
      let x_7768 = x_1885 ((true, x_4290), (false, 0)) in
      (snd (snd x_7776), snd (fst x_7768))
    in
    let x_1887 i_3402 =
      let x_6448 = x_1885 ((false, 0), (true, i_3402)) in
      let x_7760 = x_1885 ((false, 0), (true, i_3402)) in
      snd (snd x_7760)
    in
    let rec x_x_4360 x_4334 x_4335 =
      let x_6462 = x_1023 ((false, 0), (true, x_4334)) in
      let x_7752 = x_1023 ((false, 0), (true, x_4334)) in
      let x_6476 = x_1885 ((false, 0), (true, x_4335)) in
      let x_7744 = x_1885 ((false, 0), (true, x_4335)) in
      (snd (snd x_7752), snd (snd x_7744))
    in
    let rec x_x_4405 x_4379 x_4380 =
      let x_6493 = x_1885 ((true, x_4379), (false, 0)) in
      let x_6507 = x_1885 ((false, 0), (true, x_4380)) in
      let x_7735 = x_1885 ((true, x_4379), (true, x_4380)) in
      (snd (fst x_7735), snd (snd x_7735))
    in
    let rec x_x_x_4269 x_4230 x_4231 x_4232 =
      let x_6524 = x_1023 ((false, 0), (true, x_4230)) in
      let x_7727 = x_1023 ((false, 0), (true, x_4230)) in
      let x_6538 = x_1885 ((true, x_4231), (false, 0)) in
      let x_6552 = x_1885 ((false, 0), (true, x_4232)) in
      let x_7718 = x_1885 ((true, x_4231), (true, x_4232)) in
      (snd (snd x_7727), snd (fst x_7718), snd (snd x_7718))
    in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6826 = x_1887 (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6826))
        else
          if fst (#2 ixi_3377) = false then
            let x_6773 = x_1886 (snd (#1 ixi_3377)) in
            ((false, (true, 0)), (true, x_6773), (false, (true, 0)))
          else
            let x_6726 = x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (true, fst x_6726), (true, snd x_6726))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            let x_6678 = x_1810 (snd (#0 ixi_3377)) in
            ((true, x_6678), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6642 = x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, fst x_6642), (false, (true, 0)), (true, snd x_6642))
        else
          if fst (#2 ixi_3377) = false then
            let x_6600 = x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) in
            ((true, fst x_6600), (true, snd x_6600), (false, (true, 0)))
          else
            let x_6568 = x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, #0 x_6568), (true, #1 x_6568), (true, #2 x_6568))
    in
    x_1891
  else
    if fst (snd (fst x_7800)) <> false then
      let xs'_1014 x_1155 =
        let x_4934 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        let x_7710 = x_1023 ((true, x_1155 + 1), (false, 0)) in
        snd (fst x_7710)
      in
      let rec xs'_x_3837 x_3811 x_3812 =
        let x_4949 = x_1023 ((true, x_3811 + 1), (false, 0)) in
        let x_4964 = x_1023 ((false, 0), (true, x_3812)) in
        let x_7701 = x_1023 ((true, x_3811 + 1), (true, x_3812)) in
        (snd (fst x_7701), snd (snd x_7701))
      in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5044 = x_1810 (snd (snd ii_3163)) in
            ((false, (true, 0)), (true, x_5044))
        else
          if fst (snd ii_3163) = false then
            let x_5003 = xs'_1014 (snd (fst ii_3163)) in
            ((true, x_5003), (false, (true, 0)))
          else
            let x_4979 = xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)) in
            ((true, fst x_4979), (true, snd x_4979))
      in
      let x_1836 i_3143 =
        let x_5104 = x_1835 ((true, i_3143), (false, 0)) in
        let x_7693 = x_1835 ((true, i_3143), (false, 0)) in
        snd (fst x_7693)
      in
      let x_1837 i_3136 =
        let x_5123 = x_1835 ((false, 0), (true, i_3136)) in
        let x_7685 = x_1835 ((false, 0), (true, i_3136)) in
        snd (snd x_7685)
      in
      let x_5126 = append_1059 x_1835 in
      let x_1839 i_3125 =
        let x_5150 = x_5126 ((true, i_3125), (false, 0), (false, 0)) in
        let x_7674 = x_5126 ((true, i_3125), (false, 0), (false, 0)) in
        snd (#0 x_7674)
      in
      let x_1840 i_3115 =
        let x_5176 = x_5126 ((false, 0), (true, i_3115), (false, 0)) in
        let x_7663 = x_5126 ((false, 0), (true, i_3115), (false, 0)) in
        snd (#1 x_7663)
      in
      let x_1841 i_3105 =
        let x_5202 = x_5126 ((false, 0), (false, 0), (true, i_3105)) in
        let x_7652 = x_5126 ((false, 0), (false, 0), (true, i_3105)) in
        snd (#2 x_7652)
      in
      let rec x_x_3889 x_3851 x_3852 =
        let x_5220 = x_5126 ((false, 0), (true, x_3851), (false, 0)) in
        let x_5238 = x_5126 ((false, 0), (false, 0), (true, x_3852)) in
        let x_7640 = x_5126 ((false, 0), (true, x_3851), (true, x_3852)) in
        (snd (#1 x_7640), snd (#2 x_7640))
      in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5315 = x_1841 (snd (snd ii_3088)) in
            ((false, (true, 0)), (true, x_5315))
        else
          if fst (snd ii_3088) = false then
            let x_5274 = x_1840 (snd (fst ii_3088)) in
            ((true, x_5274), (false, (true, 0)))
          else
            let x_5250 = x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)) in
            ((true, fst x_5250), (true, snd x_5250))
      in
      let x_1845 i_3068 =
        let x_5375 = x_1844 ((true, i_3068), (false, 0)) in
        let x_7632 = x_1844 ((true, i_3068), (false, 0)) in
        snd (fst x_7632)
      in
      let x_1846 i_3061 =
        let x_5394 = x_1844 ((false, 0), (true, i_3061)) in
        let x_7624 = x_1844 ((false, 0), (true, i_3061)) in
        snd (snd x_7624)
      in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          let x_5421 = x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          let x_7613 = x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          snd (#0 x_7613)
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          let x_5450 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          let x_7605 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          snd (fst x_7605)
      in
      let rec x_x_3933 x_3903 x_3904 =
        if x_3903 = 0 then
          let x_5509 = x_1844 ((false, 0), (true, x_3904)) in
          let x_7597 = x_1844 ((false, 0), (true, x_3904)) in
          ((true, snd (snd (fst x_7800))), snd (snd x_7597))
        else
          let x_5473 = x_1844 ((true, x_3903 - 1), (false, 0)) in
          let x_5488 = x_1844 ((false, 0), (true, x_3904)) in
          let x_7588 = x_1844 ((true, x_3903 - 1), (true, x_3904)) in
          (snd (fst x_7588), snd (snd x_7588))
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5588 = x_1846 (snd (snd ii_2993)) in
            ((false, (true, 0)), (true, x_5588))
        else
          if fst (snd ii_2993) = false then
            let x_5547 = x_1694 (snd (fst ii_2993)) in
            ((true, x_5547), (false, (true, 0)))
          else
            let x_5523 = x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)) in
            ((true, fst x_5523), (true, snd x_5523))
      in
      let x_1872 i_2973 =
        let x_5648 = x_1871 ((true, i_2973), (false, 0)) in
        let x_7580 = x_1871 ((true, i_2973), (false, 0)) in
        snd (fst x_7580)
      in
      let rec x_x_4067 x_4031 x_4032 =
        if x_4031 = 0 then
          let x_5702 = x_1871 ((true, x_4032), (false, 0)) in
          let x_7572 = x_1871 ((true, x_4032), (false, 0)) in
          ((true, snd (snd (fst x_7800))), snd (fst x_7572))
        else
          let x_5666 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)) in
          let x_7561 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)) in
          let x_5681 = x_1871 ((true, x_4032), (false, 0)) in
          let x_7553 = x_1871 ((true, x_4032), (false, 0)) in
          (snd (#0 x_7561), snd (fst x_7553))
      in
      let x_1873 i_2966 =
        let x_5726 = x_1871 ((false, 0), (true, i_2966)) in
        let x_7545 = x_1871 ((false, 0), (true, i_2966)) in
        snd (snd x_7545)
      in
      let rec x_x_4128 x_4092 x_4093 =
        if x_4092 = 0 then
          let x_5780 = x_1871 ((false, 0), (true, x_4093)) in
          let x_7537 = x_1871 ((false, 0), (true, x_4093)) in
          ((true, snd (snd (fst x_7800))), snd (snd x_7537))
        else
          let x_5744 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)) in
          let x_7526 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)) in
          let x_5759 = x_1871 ((false, 0), (true, x_4093)) in
          let x_7518 = x_1871 ((false, 0), (true, x_4093)) in
          (snd (#0 x_7526), snd (snd x_7518))
      in
      let rec x_x_4179 x_4153 x_4154 =
        let x_5799 = x_1871 ((true, x_4153), (false, 0)) in
        let x_5813 = x_1871 ((false, 0), (true, x_4154)) in
        let x_7509 = x_1871 ((true, x_4153), (true, x_4154)) in
        (snd (fst x_7509), snd (snd x_7509))
      in
      let rec x_x_x_4002 x_3953 x_3954 x_3955 =
        if x_3953 = 0 then
          let x_5885 = x_1871 ((true, x_3954), (false, 0)) in
          let x_5899 = x_1871 ((false, 0), (true, x_3955)) in
          let x_7500 = x_1871 ((true, x_3954), (true, x_3955)) in
          ((true, snd (snd (fst x_7800))), snd (fst x_7500), snd (snd x_7500))
        else
          let x_5834 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)) in
          let x_7489 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)) in
          let x_5849 = x_1871 ((true, x_3954), (false, 0)) in
          let x_5863 = x_1871 ((false, 0), (true, x_3955)) in
          let x_7480 = x_1871 ((true, x_3954), (true, x_3955)) in
          (snd (#0 x_7489), snd (fst x_7480), snd (snd x_7480))
      in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6175 = x_1873 (snd (#2 iii_2941)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6175))
          else
            if fst (#2 iii_2941) = false then
              let x_6122 = x_1872 (snd (#1 iii_2941)) in
              ((false, (true, 0)), (true, x_6122), (false, (true, 0)))
            else
              let x_6075 = x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((false, (true, 0)), (true, fst x_6075), (true, snd x_6075))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              let x_6027 = x_1700 (snd (#0 iii_2941)) in
              ((true, x_6027), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5991 = x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)) in
              ((true, fst x_5991), (false, (true, 0)), (true, snd x_5991))
          else
            if fst (#2 iii_2941) = false then
              let x_5949 = x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)) in
              ((true, fst x_5949), (true, snd x_5949), (false, (true, 0)))
            else
              let x_5917 = x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((true, #0 x_5917), (true, #1 x_5917), (true, #2 x_5917))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4854 = x_1810 (snd (#2 iii_2532)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4854))
          else
            if fst (#2 iii_2532) = false then
              let x_4801 = x_1809 (snd (#1 iii_2532)) in
              ((false, (true, 0)), (true, x_4801), (false, (true, 0)))
            else
              let x_4754 = x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)) in
              ((false, (true, 0)), (true, fst x_4754), (true, snd x_4754))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              let x_4706 = x_1661 (snd (#0 iii_2532)) in
              ((true, x_4706), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4671 = x_1661 (snd (#0 iii_2532)) in
              let x_4692 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4671), (false, (true, 0)), (true, x_4692))
          else
            if fst (#2 iii_2532) = false then
              let x_4630 = x_1661 (snd (#0 iii_2532)) in
              let x_4640 = x_1809 (snd (#1 iii_2532)) in
              ((true, x_4630), (true, x_4640), (false, (true, 0)))
            else
              let x_4596 = x_1661 (snd (#0 iii_2532)) in
              let x_4606 = x_1809 (snd (#1 iii_2532)) in
              let x_4616 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4596), (true, x_4606), (true, x_4616))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_6893 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6969 = f_1570 (snd (snd ix_2298)) in
        ((false, (true, 0)), (true, x_6969))
    else
      if fst (snd ix_2298) = false then
        let x_6928 = x_6893 (snd (fst ix_2298)) in
        ((true, x_6928), (false, (true, 0)))
      else
        let x_6905 = x_6893 (snd (fst ix_2298)) in
        let x_6915 = f_1570 (snd (snd ix_2298)) in
        ((true, x_6905), (true, x_6915))
  in
  let x_1901 i_2278 =
    let x_7029 = x_1900 ((true, i_2278), (false, 0)) in
    let x_7472 = x_1900 ((true, i_2278), (false, 0)) in
    snd (fst x_7472)
  in
  let x_1902 x_2271 =
    let x_7048 = x_1900 ((false, 0), (true, x_2271)) in
    let x_7464 = x_1900 ((false, 0), (true, x_2271)) in
    snd (snd x_7464)
  in
  let x_7051 = append_1059 x_1900 in
  let x_1904 i_2260 =
    let x_7075 = x_7051 ((true, i_2260), (false, 0), (false, 0)) in
    let x_7453 = x_7051 ((true, i_2260), (false, 0), (false, 0)) in
    snd (#0 x_7453)
  in
  let x_1905 i_2250 =
    let x_7101 = x_7051 ((false, 0), (true, i_2250), (false, 0)) in
    let x_7442 = x_7051 ((false, 0), (true, i_2250), (false, 0)) in
    snd (#1 x_7442)
  in
  let x_1906 i_2240 =
    let x_7127 = x_7051 ((false, 0), (false, 0), (true, i_2240)) in
    let x_7431 = x_7051 ((false, 0), (false, 0), (true, i_2240)) in
    snd (#2 x_7431)
  in
  let rec x_x_4464 x_4426 x_4427 =
    let x_7145 = x_7051 ((false, 0), (true, x_4426), (false, 0)) in
    let x_7163 = x_7051 ((false, 0), (false, 0), (true, x_4427)) in
    let x_7419 = x_7051 ((false, 0), (true, x_4426), (true, x_4427)) in
    (snd (#1 x_7419), snd (#2 x_7419))
  in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_7240 = x_1906 (snd (snd ii_2223)) in
        ((false, (true, 0)), (true, x_7240))
    else
      if fst (snd ii_2223) = false then
        let x_7199 = x_1905 (snd (fst ii_2223)) in
        ((true, x_7199), (false, (true, 0)))
      else
        let x_7175 = x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)) in
        ((true, fst x_7175), (true, snd x_7175))
  in
  let x_1910 i_2203 =
    let x_7300 = x_1909 ((true, i_2203), (false, 0)) in
    let x_7411 = x_1909 ((true, i_2203), (false, 0)) in
    snd (fst x_7411)
  in
  let x_1911 i_2196 =
    let x_7319 = x_1909 ((false, 0), (true, i_2196)) in
    let x_7403 = x_1909 ((false, 0), (true, i_2196)) in
    snd (snd x_7403)
  in
  let x_7343 = x_7051 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7392 = x_7051 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7358 = x_1909 ((true, i_1016), (false, 0)) in
  let x_7384 = x_1909 ((true, i_1016), (false, 0)) in
  let n_1595 = if fst (snd (#0 x_7392)) <> false then
                 snd (snd (#0 x_7392))
               else
                 _|_ in
  let n_1596 = if fst (snd (fst x_7384)) <> false then
                 snd (snd (fst x_7384))
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_7379 = rand_int () in
let x_7381 = rand_int () in
let x_7382 = main_1015 x_7379 in
let x_7383 = x_7382 x_7381 in
let x_1927 = x_7383 in
()

is_subsumed: rand_int (), rand_int (); is_subsumed: rand_int (), main_1015 x_7379; is_subsumed: 
rand_int (), x_7382 x_7381; is_subsumed: main_1015 x_7379, x_7383; is_subsumed: 
rand_int (), x_7383; is_subsumed: rand_int (), x_7383; is_subsumed: make_list_1008 n_1017, 
append_1059 x_1900; is_subsumed: make_list_1008 n_1017, x_7051 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
x_7051 ((true, i_1016), (false, 0), (false, 0)), x_7051 ((true, i_1016), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_7051 ((true, i_1016), (false, 0), (false, 0)); x_7343 |-> x_7392
is_subsumed: x_7051 ((true, i_1016), (false, 0), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
x_7051 ((true, i_1016), (false, 0), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
append_1059 x_1900, x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
x_1909 ((true, i_1016), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
x_7051 ((true, i_1016), (false, 0), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
x_7051 ((true, i_1016), (false, 0), (false, 0)), x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
append_1059 x_1900, x_1909 ((true, i_1016), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((true, i_1016), (false, 0)); x_7358 |-> x_7384
is_subsumed: x_1909 ((true, i_1016), (false, 0)), if fst (snd (#0 x_7392)) <> false then
                                                    snd (snd (#0 x_7392))
                                                  else
                                                    _|_; is_subsumed: 
x_1909 ((true, i_1016), (false, 0)), if fst (snd (#0 x_7392)) <> false then
                                       snd (snd (#0 x_7392))
                                     else
                                       _|_; is_subsumed: x_7051 ((true, i_1016), (false, 0), (false, 0)), 
if fst (snd (#0 x_7392)) <> false then
  snd (snd (#0 x_7392))
else
  _|_; is_subsumed: append_1059 x_1900, if fst (snd (#0 x_7392)) <> false then
                                          snd (snd (#0 x_7392))
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst (snd (#0 x_7392)) <> false then
  snd (snd (#0 x_7392))
else
  _|_; is_subsumed: if fst (snd (#0 x_7392)) <> false then
                      snd (snd (#0 x_7392))
                    else
                      _|_, if fst (snd (fst x_7384)) <> false then
                             snd (snd (fst x_7384))
                           else
                             _|_; is_subsumed: x_1909 ((true, i_1016), (false, 0)), 
if fst (snd (fst x_7384)) <> false then
  snd (snd (fst x_7384))
else
  _|_; is_subsumed: x_7051 ((true, i_1016), (false, 0), (false, 0)), 
if fst (snd (fst x_7384)) <> false then
  snd (snd (fst x_7384))
else
  _|_; is_subsumed: x_7051 ((true, i_1016), (false, 0), (false, 0)), 
if fst (snd (fst x_7384)) <> false then
  snd (snd (fst x_7384))
else
  _|_; is_subsumed: append_1059 x_1900, if fst (snd (fst x_7384)) <> false then
                                          snd (snd (fst x_7384))
                                        else
                                          _|_; is_subsumed: make_list_1008 n_1017, 
if fst (snd (fst x_7384)) <> false then
  snd (snd (fst x_7384))
else
  _|_; is_subsumed: append_1059 x_1900, x_1909 ((false, 0), (true, i_2196)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((false, 0), (true, i_2196)); is_subsumed: 
x_1909 ((false, 0), (true, i_2196)), x_1909 ((false, 0), (true, i_2196)); is_subsumed: 
append_1059 x_1900, x_1909 ((false, 0), (true, i_2196)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((false, 0), (true, i_2196)); x_7319 |-> x_7403
is_subsumed: append_1059 x_1900, x_1909 ((true, i_2203), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((true, i_2203), (false, 0)); is_subsumed: 
x_1909 ((true, i_2203), (false, 0)), x_1909 ((true, i_2203), (false, 0)); is_subsumed: 
append_1059 x_1900, x_1909 ((true, i_2203), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1909 ((true, i_2203), (false, 0)); x_7300 |-> x_7411
is_subsumed: append_1059 x_1900, x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)); is_subsumed: 
make_list_1008 n_1017, x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)); is_subsumed: 
append_1059 x_1900, x_1905 (snd (fst ii_2223)); is_subsumed: make_list_1008 n_1017, 
x_1905 (snd (fst ii_2223)); is_subsumed: append_1059 x_1900, x_1906 (snd (snd ii_2223)); is_subsumed: 
make_list_1008 n_1017, x_1906 (snd (snd ii_2223)); is_subsumed: make_list_1008 n_1017, 
x_7051 ((false, 0), (true, x_4426), (false, 0)); is_subsumed: x_7051 ((false, 0), (true, x_4426), (false, 0)), 
x_7051 ((false, 0), (false, 0), (true, x_4427)); is_subsumed: make_list_1008 n_1017, 
x_7051 ((false, 0), (false, 0), (true, x_4427)); is_subsumed: x_7051 ((false, 0), (false, 0), (true, x_4427)), 
x_7051 ((false, 0), (true, x_4426), (true, x_4427)); is_subsumed: x_7051 ((false, 0), (true, x_4426), (false, 0)), 
x_7051 ((false, 0), (true, x_4426), (true, x_4427)); is_subsumed: make_list_1008 n_1017, 
x_7051 ((false, 0), (true, x_4426), (true, x_4427)); x_7163 |-> x_7419
x_7145 |-> x_7419
is_subsumed: make_list_1008 n_1017, x_7051 ((false, 0), (false, 0), (true, i_2240)); is_subsumed: 
x_7051 ((false, 0), (false, 0), (true, i_2240)), x_7051 ((false, 0), (false, 0), (true, i_2240)); is_subsumed: 
make_list_1008 n_1017, x_7051 ((false, 0), (false, 0), (true, i_2240)); x_7127 |-> x_7431
is_subsumed: make_list_1008 n_1017, x_7051 ((false, 0), (true, i_2250), (false, 0)); is_subsumed: 
x_7051 ((false, 0), (true, i_2250), (false, 0)), x_7051 ((false, 0), (true, i_2250), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_7051 ((false, 0), (true, i_2250), (false, 0)); x_7101 |-> x_7442
is_subsumed: make_list_1008 n_1017, x_7051 ((true, i_2260), (false, 0), (false, 0)); is_subsumed: 
x_7051 ((true, i_2260), (false, 0), (false, 0)), x_7051 ((true, i_2260), (false, 0), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_7051 ((true, i_2260), (false, 0), (false, 0)); x_7075 |-> x_7453
is_subsumed: make_list_1008 n_1017, x_1900 ((false, 0), (true, x_2271)); is_subsumed: 
x_1900 ((false, 0), (true, x_2271)), x_1900 ((false, 0), (true, x_2271)); is_subsumed: 
make_list_1008 n_1017, x_1900 ((false, 0), (true, x_2271)); x_7048 |-> x_7464
is_subsumed: make_list_1008 n_1017, x_1900 ((true, i_2278), (false, 0)); is_subsumed: 
x_1900 ((true, i_2278), (false, 0)), x_1900 ((true, i_2278), (false, 0)); is_subsumed: 
make_list_1008 n_1017, x_1900 ((true, i_2278), (false, 0)); x_7029 |-> x_7472
is_subsumed: x_6893 (snd (fst ix_2298)), f_1570 (snd (snd ix_2298)); is_subsumed: 
make_list_1008 n_1017, f_1570 (snd (snd ix_2298)); is_subsumed: make_list_1008 n_1017, 
f_1570 (snd (snd ix_2298)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1023 ((true, 0), (false, 0)); x_4583 |-> x_7800
is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: x_1023 ((true, 0), (false, 0)), _|_; is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: 
x_1661 (snd (#0 iii_2532)), x_1809 (snd (#1 iii_2532)); is_subsumed: _|_, 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1809 (snd (#1 iii_2532)), 
x_1810 (snd (#2 iii_2532)); is_subsumed: x_1661 (snd (#0 iii_2532)), 
x_1810 (snd (#2 iii_2532)); is_subsumed: _|_, x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: 
x_1661 (snd (#0 iii_2532)), x_1809 (snd (#1 iii_2532)); is_subsumed: _|_, 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1809 (snd (#1 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1661 (snd (#0 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1661 (snd (#0 iii_2532)); is_subsumed: x_1661 (snd (#0 iii_2532)), 
x_1810 (snd (#2 iii_2532)); is_subsumed: _|_, x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#2 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1661 (snd (#0 iii_2532)); is_subsumed: _|_, 
x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)); is_subsumed: _|_, x_1809 (snd (#1 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1809 (snd (#1 iii_2532)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1809 (snd (#1 iii_2532)); is_subsumed: _|_, 
x_1810 (snd (#2 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1810 (snd (#2 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1810 (snd (#2 iii_2532)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
append_1059 x_1835; is_subsumed: x_1023 ((true, 0), (false, 0)), append_1059 x_1835; is_subsumed: 
append_1059 x_1835, x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
append_1059 x_1835, x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)); is_subsumed: 
append_1059 x_1835, x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
append_1059 x_1835, x_1700 (snd (#0 iii_2941)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1700 (snd (#0 iii_2941)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1700 (snd (#0 iii_2941)); is_subsumed: append_1059 x_1835, x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)); is_subsumed: 
append_1059 x_1835, x_1872 (snd (#1 iii_2941)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1872 (snd (#1 iii_2941)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1872 (snd (#1 iii_2941)); is_subsumed: append_1059 x_1835, x_1873 (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1873 (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1873 (snd (#2 iii_2941)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)); x_5834 |-> x_7489
is_subsumed: x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1871 ((true, x_3954), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1871 ((false, 0), (true, x_3955)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1871 ((true, x_3954), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); x_5863 |-> x_7480
x_5849 |-> x_7480
is_subsumed: append_1059 x_1835, x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (false, 0)); is_subsumed: 
x_1871 ((true, x_3954), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_3955)); is_subsumed: 
x_1871 ((false, 0), (true, x_3955)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1871 ((true, x_3954), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_3954), (true, x_3955)); x_5899 |-> x_7500
x_5885 |-> x_7500
is_subsumed: append_1059 x_1835, x_1871 ((true, x_4153), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4153), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4153), (false, 0)); is_subsumed: 
x_1871 ((true, x_4153), (false, 0)), x_1871 ((false, 0), (true, x_4154)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_4154)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4154)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4154)); is_subsumed: 
x_1871 ((false, 0), (true, x_4154)), x_1871 ((true, x_4153), (true, x_4154)); is_subsumed: 
x_1871 ((true, x_4153), (false, 0)), x_1871 ((true, x_4153), (true, x_4154)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_4153), (true, x_4154)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4153), (true, x_4154)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4153), (true, x_4154)); x_5813 |-> x_7509
x_5799 |-> x_7509
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)); is_subsumed: 
x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)), x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)); x_5744 |-> x_7526
is_subsumed: x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1871 ((false, 0), (true, x_4093)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); x_5759 |-> x_7518
is_subsumed: append_1059 x_1835, x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1871 ((false, 0), (true, x_4093)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, x_4093)); x_5780 |-> x_7537
is_subsumed: append_1059 x_1835, x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
x_1871 ((false, 0), (true, i_2966)), x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
append_1059 x_1835, x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, i_2966)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((false, 0), (true, i_2966)); x_5726 |-> x_7545
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)); is_subsumed: 
x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)), x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)); x_5666 |-> x_7561
is_subsumed: x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1871 ((true, x_4032), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); x_5681 |-> x_7553
is_subsumed: append_1059 x_1835, x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1871 ((true, x_4032), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, x_4032), (false, 0)); x_5702 |-> x_7572
is_subsumed: append_1059 x_1835, x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
x_1871 ((true, i_2973), (false, 0)), x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, i_2973), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1871 ((true, i_2973), (false, 0)); x_5648 |-> x_7580
is_subsumed: append_1059 x_1835, x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)); is_subsumed: 
append_1059 x_1835, x_1694 (snd (fst ii_2993)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1694 (snd (fst ii_2993)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1694 (snd (fst ii_2993)); is_subsumed: append_1059 x_1835, x_1846 (snd (snd ii_2993)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1846 (snd (snd ii_2993)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1846 (snd (snd ii_2993)); is_subsumed: 
append_1059 x_1835, x_1844 ((true, x_3903 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, x_3903 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, x_3903 - 1), (false, 0)); is_subsumed: 
x_1844 ((true, x_3903 - 1), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
append_1059 x_1835, x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1844 ((false, 0), (true, x_3904)), x_1844 ((true, x_3903 - 1), (true, x_3904)); is_subsumed: 
x_1844 ((true, x_3903 - 1), (false, 0)), x_1844 ((true, x_3903 - 1), (true, x_3904)); is_subsumed: 
append_1059 x_1835, x_1844 ((true, x_3903 - 1), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, x_3903 - 1), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, x_3903 - 1), (true, x_3904)); x_5488 |-> x_7588
x_5473 |-> x_7588
is_subsumed: append_1059 x_1835, x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1844 ((false, 0), (true, x_3904)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
append_1059 x_1835, x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, x_3904)); x_5509 |-> x_7597
is_subsumed: append_1059 x_1835, x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1844 ((true, i_1248 - 1), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_1248 - 1), (false, 0)); x_5450 |-> x_7605
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)), x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)); x_5421 |-> x_7613
is_subsumed: append_1059 x_1835, x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
x_1844 ((false, 0), (true, i_3061)), x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
append_1059 x_1835, x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, i_3061)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((false, 0), (true, i_3061)); x_5394 |-> x_7624
is_subsumed: append_1059 x_1835, x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
x_1844 ((true, i_3068), (false, 0)), x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
append_1059 x_1835, x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_3068), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1844 ((true, i_3068), (false, 0)); x_5375 |-> x_7632
is_subsumed: append_1059 x_1835, x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)); is_subsumed: 
append_1059 x_1835, x_1840 (snd (fst ii_3088)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1840 (snd (fst ii_3088)); is_subsumed: x_1023 ((true, 0), (false, 0)), 
x_1840 (snd (fst ii_3088)); is_subsumed: append_1059 x_1835, x_1841 (snd (snd ii_3088)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1841 (snd (snd ii_3088)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1841 (snd (snd ii_3088)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, x_3851), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, x_3851), (false, 0)); is_subsumed: 
x_5126 ((false, 0), (true, x_3851), (false, 0)), x_5126 ((false, 0), (false, 0), (true, x_3852)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, x_3852)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, x_3852)); is_subsumed: 
x_5126 ((false, 0), (false, 0), (true, x_3852)), x_5126 ((false, 0), (true, x_3851), (true, x_3852)); is_subsumed: 
x_5126 ((false, 0), (true, x_3851), (false, 0)), x_5126 ((false, 0), (true, x_3851), (true, x_3852)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, x_3851), (true, x_3852)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, x_3851), (true, x_3852)); x_5238 |-> x_7640
x_5220 |-> x_7640
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, i_3105)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, i_3105)); is_subsumed: 
x_5126 ((false, 0), (false, 0), (true, i_3105)), x_5126 ((false, 0), (false, 0), (true, i_3105)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, i_3105)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (false, 0), (true, i_3105)); x_5202 |-> x_7652
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, i_3115), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, i_3115), (false, 0)); is_subsumed: 
x_5126 ((false, 0), (true, i_3115), (false, 0)), x_5126 ((false, 0), (true, i_3115), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, i_3115), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((false, 0), (true, i_3115), (false, 0)); x_5176 |-> x_7663
is_subsumed: x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_3125), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_3125), (false, 0), (false, 0)); is_subsumed: 
x_5126 ((true, i_3125), (false, 0), (false, 0)), x_5126 ((true, i_3125), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_3125), (false, 0), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_5126 ((true, i_3125), (false, 0), (false, 0)); x_5150 |-> x_7674
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1835 ((false, 0), (true, i_3136)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((false, 0), (true, i_3136)); is_subsumed: 
x_1835 ((false, 0), (true, i_3136)), x_1835 ((false, 0), (true, i_3136)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((false, 0), (true, i_3136)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((false, 0), (true, i_3136)); x_5123 |-> x_7685
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1835 ((true, i_3143), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((true, i_3143), (false, 0)); is_subsumed: 
x_1835 ((true, i_3143), (false, 0)), x_1835 ((true, i_3143), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((true, i_3143), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1835 ((true, i_3143), (false, 0)); x_5104 |-> x_7693
is_subsumed: x_1023 ((true, 0), (false, 0)), xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), xs'_1014 (snd (fst ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (snd ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (snd ii_3163)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3811 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3811 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_3811 + 1), (false, 0)), x_1023 ((false, 0), (true, x_3812)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3812)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_3812)); is_subsumed: 
x_1023 ((false, 0), (true, x_3812)), x_1023 ((true, x_3811 + 1), (true, x_3812)); is_subsumed: 
x_1023 ((true, x_3811 + 1), (false, 0)), x_1023 ((true, x_3811 + 1), (true, x_3812)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3811 + 1), (true, x_3812)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_3811 + 1), (true, x_3812)); x_4964 |-> x_7701
x_4949 |-> x_7701
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, x_1155 + 1), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((true, x_1155 + 1), (false, 0)); x_4934 |-> x_7710
is_subsumed: x_1023 ((true, 0), (false, 0)), x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#0 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (#0 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1886 (snd (#1 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1886 (snd (#1 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1887 (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1887 (snd (#2 ixi_3377)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4230)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4230)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1023 ((false, 0), (true, x_4230)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4230)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4230)); x_6524 |-> x_7727
is_subsumed: x_1023 ((false, 0), (true, x_4230)), x_1885 ((true, x_4231), (false, 0)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1885 ((true, x_4231), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4231), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4231), (false, 0)); is_subsumed: 
x_1885 ((true, x_4231), (false, 0)), x_1885 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1885 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1885 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4232)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4232)); is_subsumed: 
x_1885 ((false, 0), (true, x_4232)), x_1885 ((true, x_4231), (true, x_4232)); is_subsumed: 
x_1885 ((true, x_4231), (false, 0)), x_1885 ((true, x_4231), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1885 ((true, x_4231), (true, x_4232)); is_subsumed: 
x_1023 ((false, 0), (true, x_4230)), x_1885 ((true, x_4231), (true, x_4232)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4231), (true, x_4232)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4231), (true, x_4232)); x_6552 |-> x_7718
x_6538 |-> x_7718
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4379), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4379), (false, 0)); is_subsumed: 
x_1885 ((true, x_4379), (false, 0)), x_1885 ((false, 0), (true, x_4380)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4380)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4380)); is_subsumed: 
x_1885 ((false, 0), (true, x_4380)), x_1885 ((true, x_4379), (true, x_4380)); is_subsumed: 
x_1885 ((true, x_4379), (false, 0)), x_1885 ((true, x_4379), (true, x_4380)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4379), (true, x_4380)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4379), (true, x_4380)); x_6507 |-> x_7735
x_6493 |-> x_7735
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4334)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4334)); is_subsumed: 
x_1023 ((false, 0), (true, x_4334)), x_1023 ((false, 0), (true, x_4334)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4334)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4334)); x_6462 |-> x_7752
is_subsumed: x_1023 ((false, 0), (true, x_4334)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((false, 0), (true, x_4334)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1885 ((false, 0), (true, x_4335)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((false, 0), (true, x_4334)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((false, 0), (true, x_4334)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4335)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, x_4335)); x_6476 |-> x_7744
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, i_3402)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, i_3402)); is_subsumed: 
x_1885 ((false, 0), (true, i_3402)), x_1885 ((false, 0), (true, i_3402)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, i_3402)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((false, 0), (true, i_3402)); x_6448 |-> x_7760
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4289)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4289)); is_subsumed: 
x_1023 ((false, 0), (true, x_4289)), x_1023 ((false, 0), (true, x_4289)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4289)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4289)); x_6412 |-> x_7776
is_subsumed: x_1023 ((false, 0), (true, x_4289)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((false, 0), (true, x_4289)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1885 ((true, x_4290), (false, 0)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((false, 0), (true, x_4289)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((false, 0), (true, x_4289)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4290), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_4290), (false, 0)); x_6426 |-> x_7768
is_subsumed: x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_3409), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_3409), (false, 0)); is_subsumed: 
x_1885 ((true, x_3409), (false, 0)), x_1885 ((true, x_3409), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_3409), (false, 0)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1885 ((true, x_3409), (false, 0)); x_6398 |-> x_7784
is_subsumed: x_1023 ((true, 0), (false, 0)), x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1714 (snd (fst xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1714 (snd (fst xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (snd xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1810 (snd (snd xi_3429)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4199)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4199)); is_subsumed: 
x_1023 ((false, 0), (true, x_4199)), x_1023 ((false, 0), (true, x_4199)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4199)); is_subsumed: 
x_1023 ((true, 0), (false, 0)), x_1023 ((false, 0), (true, x_4199)); x_6261 |-> x_7792
is_subsumed: x_1023 ((true, x_3766), (false, 0)), x_1023 ((false, 0), (true, x_3767)); is_subsumed: 
x_1023 ((false, 0), (true, x_3767)), x_1023 ((true, x_3766), (true, x_3767)); is_subsumed: 
x_1023 ((true, x_3766), (false, 0)), x_1023 ((true, x_3766), (true, x_3767)); x_4562 |-> x_7808
x_4548 |-> x_7808
is_subsumed: x_1023 ((false, 0), (true, i_3467)), x_1023 ((false, 0), (true, i_3467)); x_4534 |-> x_7817
is_subsumed: x_1023 ((true, i_3474), (false, 0)), x_1023 ((true, i_3474), (false, 0)); x_4515 |-> x_7825
is_subsumed: rand_int (), make_list_1008 (n_1009 - 1); x_4515; x_4534; x_4548; x_4562; x_6524; x_6552; x_6538; x_6507; 
                                                       x_6493; x_6476; x_6462; x_6448; x_6426; x_6412; x_6398; x_6261; 
                                                       x_4934; x_4949; x_4964; x_5104; x_5123; x_5150; x_5176; x_5202; 
                                                       x_5220; x_5238; x_5375; x_5394; x_5421; x_5450; x_5509; x_5488; 
                                                       x_5473; x_5648; x_5702; x_5681; x_5666; x_5726; x_5780; x_5759; 
                                                       x_5744; x_5799; x_5813; x_5834; x_5863; x_5849; x_5899; x_5885; 
                                                       x_4583; x_7343; x_7358; x_7319; x_7300; x_7163; x_7145; x_7127; 
                                                       x_7101; x_7075; x_7048; x_7029
elim_unnecessary:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_4479 = rand_int () in
    let x_4482 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_4479)
                   else
                     x_4482 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = let x_7825 = x_1023 ((true, i_3474), (false, 0)) in
                      snd (fst x_7825) in
  let x_1810 i_3467 = let x_7817 = x_1023 ((false, 0), (true, i_3467)) in
                      snd (snd x_7817) in
  let rec x_x_3792 x_3766 x_3767 =
    let x_7808 = x_1023 ((true, x_3766), (true, x_3767)) in
    (snd (fst x_7808), snd (snd x_7808))
  in
  let x_7800 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7800)) = false then
    let x_1714 x_1277 = (false, 0) in
    let rec x_x_4216 x_4198 x_4199 = let x_7792 = x_1023 ((false, 0), (true, x_4199)) in
                                     ((false, 0), snd (snd x_7792)) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          let x_6338 = x_1810 (snd (snd xi_3429)) in
          ((false, (true, 0)), (true, x_6338))
      else
        if fst (snd xi_3429) = false then
          let x_6297 = x_1714 (snd (fst xi_3429)) in
          ((true, x_6297), (false, (true, 0)))
        else
          let x_6273 = x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)) in
          ((true, fst x_6273), (true, snd x_6273))
    in
    let x_1886 x_3409 = let x_7784 = x_1885 ((true, x_3409), (false, 0)) in
                        snd (fst x_7784) in
    let rec x_x_4315 x_4289 x_4290 =
      let x_7776 = x_1023 ((false, 0), (true, x_4289)) in
      let x_7768 = x_1885 ((true, x_4290), (false, 0)) in
      (snd (snd x_7776), snd (fst x_7768))
    in
    let x_1887 i_3402 = let x_7760 = x_1885 ((false, 0), (true, i_3402)) in
                        snd (snd x_7760) in
    let rec x_x_4360 x_4334 x_4335 =
      let x_7752 = x_1023 ((false, 0), (true, x_4334)) in
      let x_7744 = x_1885 ((false, 0), (true, x_4335)) in
      (snd (snd x_7752), snd (snd x_7744))
    in
    let rec x_x_4405 x_4379 x_4380 =
      let x_7735 = x_1885 ((true, x_4379), (true, x_4380)) in
      (snd (fst x_7735), snd (snd x_7735))
    in
    let rec x_x_x_4269 x_4230 x_4231 x_4232 =
      let x_7727 = x_1023 ((false, 0), (true, x_4230)) in
      let x_7718 = x_1885 ((true, x_4231), (true, x_4232)) in
      (snd (snd x_7727), snd (fst x_7718), snd (snd x_7718))
    in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6826 = x_1887 (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (false, (true, 0)), (true, x_6826))
        else
          if fst (#2 ixi_3377) = false then
            let x_6773 = x_1886 (snd (#1 ixi_3377)) in
            ((false, (true, 0)), (true, x_6773), (false, (true, 0)))
          else
            let x_6726 = x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (true, fst x_6726), (true, snd x_6726))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            let x_6678 = x_1810 (snd (#0 ixi_3377)) in
            ((true, x_6678), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6642 = x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, fst x_6642), (false, (true, 0)), (true, snd x_6642))
        else
          if fst (#2 ixi_3377) = false then
            let x_6600 = x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) in
            ((true, fst x_6600), (true, snd x_6600), (false, (true, 0)))
          else
            let x_6568 = x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, #0 x_6568), (true, #1 x_6568), (true, #2 x_6568))
    in
    x_1891
  else
    if fst (snd (fst x_7800)) <> false then
      let xs'_1014 x_1155 = let x_7710 = x_1023 ((true, x_1155 + 1), (false, 0)) in
                            snd (fst x_7710) in
      let rec xs'_x_3837 x_3811 x_3812 =
        let x_7701 = x_1023 ((true, x_3811 + 1), (true, x_3812)) in
        (snd (fst x_7701), snd (snd x_7701))
      in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5044 = x_1810 (snd (snd ii_3163)) in
            ((false, (true, 0)), (true, x_5044))
        else
          if fst (snd ii_3163) = false then
            let x_5003 = xs'_1014 (snd (fst ii_3163)) in
            ((true, x_5003), (false, (true, 0)))
          else
            let x_4979 = xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)) in
            ((true, fst x_4979), (true, snd x_4979))
      in
      let x_1836 i_3143 = let x_7693 = x_1835 ((true, i_3143), (false, 0)) in
                          snd (fst x_7693) in
      let x_1837 i_3136 = let x_7685 = x_1835 ((false, 0), (true, i_3136)) in
                          snd (snd x_7685) in
      let x_5126 = append_1059 x_1835 in
      let x_1839 i_3125 = let x_7674 = x_5126 ((true, i_3125), (false, 0), (false, 0)) in
                          snd (#0 x_7674) in
      let x_1840 i_3115 = let x_7663 = x_5126 ((false, 0), (true, i_3115), (false, 0)) in
                          snd (#1 x_7663) in
      let x_1841 i_3105 = let x_7652 = x_5126 ((false, 0), (false, 0), (true, i_3105)) in
                          snd (#2 x_7652) in
      let rec x_x_3889 x_3851 x_3852 =
        let x_7640 = x_5126 ((false, 0), (true, x_3851), (true, x_3852)) in
        (snd (#1 x_7640), snd (#2 x_7640))
      in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5315 = x_1841 (snd (snd ii_3088)) in
            ((false, (true, 0)), (true, x_5315))
        else
          if fst (snd ii_3088) = false then
            let x_5274 = x_1840 (snd (fst ii_3088)) in
            ((true, x_5274), (false, (true, 0)))
          else
            let x_5250 = x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)) in
            ((true, fst x_5250), (true, snd x_5250))
      in
      let x_1845 i_3068 = let x_7632 = x_1844 ((true, i_3068), (false, 0)) in
                          snd (fst x_7632) in
      let x_1846 i_3061 = let x_7624 = x_1844 ((false, 0), (true, i_3061)) in
                          snd (snd x_7624) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          let x_7613 = x_5126 ((true, i_1231 - 1), (false, 0), (false, 0)) in
          snd (#0 x_7613)
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          let x_7605 = x_1844 ((true, i_1248 - 1), (false, 0)) in
          snd (fst x_7605)
      in
      let rec x_x_3933 x_3903 x_3904 =
        if x_3903 = 0 then
          let x_7597 = x_1844 ((false, 0), (true, x_3904)) in
          ((true, snd (snd (fst x_7800))), snd (snd x_7597))
        else
          let x_7588 = x_1844 ((true, x_3903 - 1), (true, x_3904)) in
          (snd (fst x_7588), snd (snd x_7588))
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            let x_5588 = x_1846 (snd (snd ii_2993)) in
            ((false, (true, 0)), (true, x_5588))
        else
          if fst (snd ii_2993) = false then
            let x_5547 = x_1694 (snd (fst ii_2993)) in
            ((true, x_5547), (false, (true, 0)))
          else
            let x_5523 = x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)) in
            ((true, fst x_5523), (true, snd x_5523))
      in
      let x_1872 i_2973 = let x_7580 = x_1871 ((true, i_2973), (false, 0)) in
                          snd (fst x_7580) in
      let rec x_x_4067 x_4031 x_4032 =
        if x_4031 = 0 then
          let x_7572 = x_1871 ((true, x_4032), (false, 0)) in
          ((true, snd (snd (fst x_7800))), snd (fst x_7572))
        else
          let x_7561 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)) in
          let x_7553 = x_1871 ((true, x_4032), (false, 0)) in
          (snd (#0 x_7561), snd (fst x_7553))
      in
      let x_1873 i_2966 = let x_7545 = x_1871 ((false, 0), (true, i_2966)) in
                          snd (snd x_7545) in
      let rec x_x_4128 x_4092 x_4093 =
        if x_4092 = 0 then
          let x_7537 = x_1871 ((false, 0), (true, x_4093)) in
          ((true, snd (snd (fst x_7800))), snd (snd x_7537))
        else
          let x_7526 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)) in
          let x_7518 = x_1871 ((false, 0), (true, x_4093)) in
          (snd (#0 x_7526), snd (snd x_7518))
      in
      let rec x_x_4179 x_4153 x_4154 =
        let x_7509 = x_1871 ((true, x_4153), (true, x_4154)) in
        (snd (fst x_7509), snd (snd x_7509))
      in
      let rec x_x_x_4002 x_3953 x_3954 x_3955 =
        if x_3953 = 0 then
          let x_7500 = x_1871 ((true, x_3954), (true, x_3955)) in
          ((true, snd (snd (fst x_7800))), snd (fst x_7500), snd (snd x_7500))
        else
          let x_7489 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)) in
          let x_7480 = x_1871 ((true, x_3954), (true, x_3955)) in
          (snd (#0 x_7489), snd (fst x_7480), snd (snd x_7480))
      in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_6175 = x_1873 (snd (#2 iii_2941)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_6175))
          else
            if fst (#2 iii_2941) = false then
              let x_6122 = x_1872 (snd (#1 iii_2941)) in
              ((false, (true, 0)), (true, x_6122), (false, (true, 0)))
            else
              let x_6075 = x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((false, (true, 0)), (true, fst x_6075), (true, snd x_6075))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              let x_6027 = x_1700 (snd (#0 iii_2941)) in
              ((true, x_6027), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5991 = x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)) in
              ((true, fst x_5991), (false, (true, 0)), (true, snd x_5991))
          else
            if fst (#2 iii_2941) = false then
              let x_5949 = x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)) in
              ((true, fst x_5949), (true, snd x_5949), (false, (true, 0)))
            else
              let x_5917 = x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((true, #0 x_5917), (true, #1 x_5917), (true, #2 x_5917))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4854 = x_1810 (snd (#2 iii_2532)) in
              ((false, (true, 0)), (false, (true, 0)), (true, x_4854))
          else
            if fst (#2 iii_2532) = false then
              let x_4801 = x_1809 (snd (#1 iii_2532)) in
              ((false, (true, 0)), (true, x_4801), (false, (true, 0)))
            else
              let x_4754 = x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)) in
              ((false, (true, 0)), (true, fst x_4754), (true, snd x_4754))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              let x_4706 = x_1661 (snd (#0 iii_2532)) in
              ((true, x_4706), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4671 = x_1661 (snd (#0 iii_2532)) in
              let x_4692 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4671), (false, (true, 0)), (true, x_4692))
          else
            if fst (#2 iii_2532) = false then
              let x_4630 = x_1661 (snd (#0 iii_2532)) in
              let x_4640 = x_1809 (snd (#1 iii_2532)) in
              ((true, x_4630), (true, x_4640), (false, (true, 0)))
            else
              let x_4596 = x_1661 (snd (#0 iii_2532)) in
              let x_4606 = x_1809 (snd (#1 iii_2532)) in
              let x_4616 = x_1810 (snd (#2 iii_2532)) in
              ((true, x_4596), (true, x_4606), (true, x_4616))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_6893 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_6969 = f_1570 (snd (snd ix_2298)) in
        ((false, (true, 0)), (true, x_6969))
    else
      if fst (snd ix_2298) = false then
        let x_6928 = x_6893 (snd (fst ix_2298)) in
        ((true, x_6928), (false, (true, 0)))
      else
        let x_6905 = x_6893 (snd (fst ix_2298)) in
        let x_6915 = f_1570 (snd (snd ix_2298)) in
        ((true, x_6905), (true, x_6915))
  in
  let x_1901 i_2278 = let x_7472 = x_1900 ((true, i_2278), (false, 0)) in
                      snd (fst x_7472) in
  let x_1902 x_2271 = let x_7464 = x_1900 ((false, 0), (true, x_2271)) in
                      snd (snd x_7464) in
  let x_7051 = append_1059 x_1900 in
  let x_1904 i_2260 = let x_7453 = x_7051 ((true, i_2260), (false, 0), (false, 0)) in
                      snd (#0 x_7453) in
  let x_1905 i_2250 = let x_7442 = x_7051 ((false, 0), (true, i_2250), (false, 0)) in
                      snd (#1 x_7442) in
  let x_1906 i_2240 = let x_7431 = x_7051 ((false, 0), (false, 0), (true, i_2240)) in
                      snd (#2 x_7431) in
  let rec x_x_4464 x_4426 x_4427 =
    let x_7419 = x_7051 ((false, 0), (true, x_4426), (true, x_4427)) in
    (snd (#1 x_7419), snd (#2 x_7419))
  in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        let x_7240 = x_1906 (snd (snd ii_2223)) in
        ((false, (true, 0)), (true, x_7240))
    else
      if fst (snd ii_2223) = false then
        let x_7199 = x_1905 (snd (fst ii_2223)) in
        ((true, x_7199), (false, (true, 0)))
      else
        let x_7175 = x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)) in
        ((true, fst x_7175), (true, snd x_7175))
  in
  let x_1910 i_2203 = let x_7411 = x_1909 ((true, i_2203), (false, 0)) in
                      snd (fst x_7411) in
  let x_1911 i_2196 = let x_7403 = x_1909 ((false, 0), (true, i_2196)) in
                      snd (snd x_7403) in
  let x_7392 = x_7051 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7384 = x_1909 ((true, i_1016), (false, 0)) in
  let n_1595 = if fst (snd (#0 x_7392)) <> false then
                 snd (snd (#0 x_7392))
               else
                 _|_ in
  let n_1596 = if fst (snd (fst x_7384)) <> false then
                 snd (snd (fst x_7384))
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_7379 = rand_int () in
let x_7381 = rand_int () in
let x_7382 = main_1015 x_7379 in
let x_7383 = x_7382 x_7381 in
let x_1927 = x_7383 in
()

inline_next_redex:
let rec make_list_1008 n_1009 =
  if n_1009 < 0 then
    fun x_1122 -> (false, 0)
  else
    let x_4479 = rand_int () in
    let x_4482 = make_list_1008 (n_1009 - 1) in
    fun i_1112 -> (if i_1112 = 0 then
                     (true, x_4479)
                   else
                     x_4482 (i_1112 - 1))
in
let rec append_1059 x_1023 =
  let x_1809 i_3474 = snd (fst (x_1023 ((true, i_3474), (false, 0)))) in
  let x_1810 i_3467 = snd (snd (x_1023 ((false, 0), (true, i_3467)))) in
  let rec x_x_3792 x_3766 x_3767 =
    let x_7808 = x_1023 ((true, x_3766), (true, x_3767)) in
    (snd (fst x_7808), snd (snd x_7808))
  in
  let x_7800 = x_1023 ((true, 0), (false, 0)) in
  if fst (snd (fst x_7800)) = false then
    let x_1714 x_1277 = (false, 0) in
    let rec x_x_4216 x_4198 x_4199 = ((false, 0), snd (snd (x_1023 ((false, 0), (true, x_4199))))) in
    let x_1885 xi_3429 =
      if fst (fst xi_3429) = false then
        if fst (snd xi_3429) = false then
          ((false, (true, 0)), (false, (true, 0)))
        else
          ((false, (true, 0)), (true, x_1810 (snd (snd xi_3429))))
      else
        if fst (snd xi_3429) = false then
          ((true, x_1714 (snd (fst xi_3429))), (false, (true, 0)))
        else
          let x_6273 = x_x_4216 (snd (fst xi_3429)) (snd (snd xi_3429)) in
          ((true, fst x_6273), (true, snd x_6273))
    in
    let x_1886 x_3409 = snd (fst (x_1885 ((true, x_3409), (false, 0)))) in
    let rec x_x_4315 x_4289 x_4290 =
      let x_7776 = x_1023 ((false, 0), (true, x_4289)) in
      (snd (snd x_7776), snd (fst (x_1885 ((true, x_4290), (false, 0)))))
    in
    let x_1887 i_3402 = snd (snd (x_1885 ((false, 0), (true, i_3402)))) in
    let rec x_x_4360 x_4334 x_4335 =
      let x_7752 = x_1023 ((false, 0), (true, x_4334)) in
      (snd (snd x_7752), snd (snd (x_1885 ((false, 0), (true, x_4335)))))
    in
    let rec x_x_4405 x_4379 x_4380 =
      let x_7735 = x_1885 ((true, x_4379), (true, x_4380)) in
      (snd (fst x_7735), snd (snd x_7735))
    in
    let rec x_x_x_4269 x_4230 x_4231 x_4232 =
      let x_7727 = x_1023 ((false, 0), (true, x_4230)) in
      let x_7718 = x_1885 ((true, x_4231), (true, x_4232)) in
      (snd (snd x_7727), snd (fst x_7718), snd (snd x_7718))
    in
    let x_1891 ixi_3377 =
      if fst (#0 ixi_3377) = false then
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (false, (true, 0)), (true, x_1887 (snd (#2 ixi_3377))))
        else
          if fst (#2 ixi_3377) = false then
            ((false, (true, 0)), (true, x_1886 (snd (#1 ixi_3377))), (false, (true, 0)))
          else
            let x_6726 = x_x_4405 (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((false, (true, 0)), (true, fst x_6726), (true, snd x_6726))
      else
        if fst (#1 ixi_3377) = false then
          if fst (#2 ixi_3377) = false then
            ((true, x_1810 (snd (#0 ixi_3377))), (false, (true, 0)), (false, (true, 0)))
          else
            let x_6642 = x_x_4360 (snd (#0 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, fst x_6642), (false, (true, 0)), (true, snd x_6642))
        else
          if fst (#2 ixi_3377) = false then
            let x_6600 = x_x_4315 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) in
            ((true, fst x_6600), (true, snd x_6600), (false, (true, 0)))
          else
            let x_6568 = x_x_x_4269 (snd (#0 ixi_3377)) (snd (#1 ixi_3377)) (snd (#2 ixi_3377)) in
            ((true, #0 x_6568), (true, #1 x_6568), (true, #2 x_6568))
    in
    x_1891
  else
    if fst (snd (fst x_7800)) <> false then
      let xs'_1014 x_1155 = snd (fst (x_1023 ((true, x_1155 + 1), (false, 0)))) in
      let rec xs'_x_3837 x_3811 x_3812 =
        let x_7701 = x_1023 ((true, x_3811 + 1), (true, x_3812)) in
        (snd (fst x_7701), snd (snd x_7701))
      in
      let x_1835 ii_3163 =
        if fst (fst ii_3163) = false then
          if fst (snd ii_3163) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1810 (snd (snd ii_3163))))
        else
          if fst (snd ii_3163) = false then
            ((true, xs'_1014 (snd (fst ii_3163))), (false, (true, 0)))
          else
            let x_4979 = xs'_x_3837 (snd (fst ii_3163)) (snd (snd ii_3163)) in
            ((true, fst x_4979), (true, snd x_4979))
      in
      let x_1836 i_3143 = snd (fst (x_1835 ((true, i_3143), (false, 0)))) in
      let x_1837 i_3136 = snd (snd (x_1835 ((false, 0), (true, i_3136)))) in
      let x_5126 = append_1059 x_1835 in
      let x_1839 i_3125 = snd (#0 (x_5126 ((true, i_3125), (false, 0), (false, 0)))) in
      let x_1840 i_3115 = snd (#1 (x_5126 ((false, 0), (true, i_3115), (false, 0)))) in
      let x_1841 i_3105 = snd (#2 (x_5126 ((false, 0), (false, 0), (true, i_3105)))) in
      let rec x_x_3889 x_3851 x_3852 =
        let x_7640 = x_5126 ((false, 0), (true, x_3851), (true, x_3852)) in
        (snd (#1 x_7640), snd (#2 x_7640))
      in
      let x_1844 ii_3088 =
        if fst (fst ii_3088) = false then
          if fst (snd ii_3088) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1841 (snd (snd ii_3088))))
        else
          if fst (snd ii_3088) = false then
            ((true, x_1840 (snd (fst ii_3088))), (false, (true, 0)))
          else
            let x_5250 = x_x_3889 (snd (fst ii_3088)) (snd (snd ii_3088)) in
            ((true, fst x_5250), (true, snd x_5250))
      in
      let x_1845 i_3068 = snd (fst (x_1844 ((true, i_3068), (false, 0)))) in
      let x_1846 i_3061 = snd (snd (x_1844 ((false, 0), (true, i_3061)))) in
      let x_1700 i_1231 =
        if i_1231 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          snd (#0 (x_5126 ((true, i_1231 - 1), (false, 0), (false, 0))))
      in
      let x_1694 i_1248 =
        if i_1248 = 0 then
          (true, snd (snd (fst x_7800)))
        else
          snd (fst (x_1844 ((true, i_1248 - 1), (false, 0))))
      in
      let rec x_x_3933 x_3903 x_3904 =
        if x_3903 = 0 then
          ((true, snd (snd (fst x_7800))), snd (snd (x_1844 ((false, 0), (true, x_3904)))))
        else
          let x_7588 = x_1844 ((true, x_3903 - 1), (true, x_3904)) in
          (snd (fst x_7588), snd (snd x_7588))
      in
      let x_1871 ii_2993 =
        if fst (fst ii_2993) = false then
          if fst (snd ii_2993) = false then
            ((false, (true, 0)), (false, (true, 0)))
          else
            ((false, (true, 0)), (true, x_1846 (snd (snd ii_2993))))
        else
          if fst (snd ii_2993) = false then
            ((true, x_1694 (snd (fst ii_2993))), (false, (true, 0)))
          else
            let x_5523 = x_x_3933 (snd (fst ii_2993)) (snd (snd ii_2993)) in
            ((true, fst x_5523), (true, snd x_5523))
      in
      let x_1872 i_2973 = snd (fst (x_1871 ((true, i_2973), (false, 0)))) in
      let rec x_x_4067 x_4031 x_4032 =
        if x_4031 = 0 then
          ((true, snd (snd (fst x_7800))), snd (fst (x_1871 ((true, x_4032), (false, 0)))))
        else
          let x_7561 = x_5126 ((true, x_4031 - 1), (false, 0), (false, 0)) in
          (snd (#0 x_7561), snd (fst (x_1871 ((true, x_4032), (false, 0)))))
      in
      let x_1873 i_2966 = snd (snd (x_1871 ((false, 0), (true, i_2966)))) in
      let rec x_x_4128 x_4092 x_4093 =
        if x_4092 = 0 then
          ((true, snd (snd (fst x_7800))), snd (snd (x_1871 ((false, 0), (true, x_4093)))))
        else
          let x_7526 = x_5126 ((true, x_4092 - 1), (false, 0), (false, 0)) in
          (snd (#0 x_7526), snd (snd (x_1871 ((false, 0), (true, x_4093)))))
      in
      let rec x_x_4179 x_4153 x_4154 =
        let x_7509 = x_1871 ((true, x_4153), (true, x_4154)) in
        (snd (fst x_7509), snd (snd x_7509))
      in
      let rec x_x_x_4002 x_3953 x_3954 x_3955 =
        if x_3953 = 0 then
          let x_7500 = x_1871 ((true, x_3954), (true, x_3955)) in
          ((true, snd (snd (fst x_7800))), snd (fst x_7500), snd (snd x_7500))
        else
          let x_7489 = x_5126 ((true, x_3953 - 1), (false, 0), (false, 0)) in
          let x_7480 = x_1871 ((true, x_3954), (true, x_3955)) in
          (snd (#0 x_7489), snd (fst x_7480), snd (snd x_7480))
      in
      let x_1877 iii_2941 =
        if fst (#0 iii_2941) = false then
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1873 (snd (#2 iii_2941))))
          else
            if fst (#2 iii_2941) = false then
              ((false, (true, 0)), (true, x_1872 (snd (#1 iii_2941))), (false, (true, 0)))
            else
              let x_6075 = x_x_4179 (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((false, (true, 0)), (true, fst x_6075), (true, snd x_6075))
        else
          if fst (#1 iii_2941) = false then
            if fst (#2 iii_2941) = false then
              ((true, x_1700 (snd (#0 iii_2941))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_5991 = x_x_4128 (snd (#0 iii_2941)) (snd (#2 iii_2941)) in
              ((true, fst x_5991), (false, (true, 0)), (true, snd x_5991))
          else
            if fst (#2 iii_2941) = false then
              let x_5949 = x_x_4067 (snd (#0 iii_2941)) (snd (#1 iii_2941)) in
              ((true, fst x_5949), (true, snd x_5949), (false, (true, 0)))
            else
              let x_5917 = x_x_x_4002 (snd (#0 iii_2941)) (snd (#1 iii_2941)) (snd (#2 iii_2941)) in
              ((true, #0 x_5917), (true, #1 x_5917), (true, #2 x_5917))
      in
      x_1877
    else
      let x_1661 = _|_ in
      let x_1824 iii_2532 =
        if fst (#0 iii_2532) = false then
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (false, (true, 0)), (false, (true, 0)))
            else
              ((false, (true, 0)), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              ((false, (true, 0)), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              let x_4754 = x_x_3792 (snd (#1 iii_2532)) (snd (#2 iii_2532)) in
              ((false, (true, 0)), (true, fst x_4754), (true, snd x_4754))
        else
          if fst (#1 iii_2532) = false then
            if fst (#2 iii_2532) = false then
              ((true, x_1661 (snd (#0 iii_2532))), (false, (true, 0)), (false, (true, 0)))
            else
              let x_4671 = x_1661 (snd (#0 iii_2532)) in
              ((true, x_4671), (false, (true, 0)), (true, x_1810 (snd (#2 iii_2532))))
          else
            if fst (#2 iii_2532) = false then
              let x_4630 = x_1661 (snd (#0 iii_2532)) in
              ((true, x_4630), (true, x_1809 (snd (#1 iii_2532))), (false, (true, 0)))
            else
              let x_4596 = x_1661 (snd (#0 iii_2532)) in
              let x_4606 = x_1809 (snd (#1 iii_2532)) in
              ((true, x_4596), (true, x_4606), (true, x_1810 (snd (#2 iii_2532))))
      in
      x_1824
in
let main_1015 i_1016 n_1017 =
  let x_6893 = make_list_1008 n_1017 in
  let f_1570 x_1410 = (false, 0) in
  let x_1900 ix_2298 =
    if fst (fst ix_2298) = false then
      if fst (snd ix_2298) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, f_1570 (snd (snd ix_2298))))
    else
      if fst (snd ix_2298) = false then
        ((true, x_6893 (snd (fst ix_2298))), (false, (true, 0)))
      else
        let x_6905 = x_6893 (snd (fst ix_2298)) in
        ((true, x_6905), (true, f_1570 (snd (snd ix_2298))))
  in
  let x_1901 i_2278 = snd (fst (x_1900 ((true, i_2278), (false, 0)))) in
  let x_1902 x_2271 = snd (snd (x_1900 ((false, 0), (true, x_2271)))) in
  let x_7051 = append_1059 x_1900 in
  let x_1904 i_2260 = snd (#0 (x_7051 ((true, i_2260), (false, 0), (false, 0)))) in
  let x_1905 i_2250 = snd (#1 (x_7051 ((false, 0), (true, i_2250), (false, 0)))) in
  let x_1906 i_2240 = snd (#2 (x_7051 ((false, 0), (false, 0), (true, i_2240)))) in
  let rec x_x_4464 x_4426 x_4427 =
    let x_7419 = x_7051 ((false, 0), (true, x_4426), (true, x_4427)) in
    (snd (#1 x_7419), snd (#2 x_7419))
  in
  let x_1909 ii_2223 =
    if fst (fst ii_2223) = false then
      if fst (snd ii_2223) = false then
        ((false, (true, 0)), (false, (true, 0)))
      else
        ((false, (true, 0)), (true, x_1906 (snd (snd ii_2223))))
    else
      if fst (snd ii_2223) = false then
        ((true, x_1905 (snd (fst ii_2223))), (false, (true, 0)))
      else
        let x_7175 = x_x_4464 (snd (fst ii_2223)) (snd (snd ii_2223)) in
        ((true, fst x_7175), (true, snd x_7175))
  in
  let x_1910 i_2203 = snd (fst (x_1909 ((true, i_2203), (false, 0)))) in
  let x_1911 i_2196 = snd (snd (x_1909 ((false, 0), (true, i_2196)))) in
  let x_7392 = x_7051 ((true, i_1016), (false, 0), (false, 0)) in
  let x_7384 = x_1909 ((true, i_1016), (false, 0)) in
  let n_1595 = if fst (snd (#0 x_7392)) <> false then
                 snd (snd (#0 x_7392))
               else
                 _|_ in
  let n_1596 = if fst (snd (fst x_7384)) <> false then
                 snd (snd (fst x_7384))
               else
                 _|_ in
  if n_1595 = n_1596 then
    ()
  else
    {fail} ()
in
let x_7379 = rand_int () in
let x_7381 = rand_int () in
let x_7382 = main_1015 x_7379 in
let x_7383 = x_7382 x_7381 in
let x_1927 = x_7383 in
()

