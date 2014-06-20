(* string-cvt.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Basic routines to convert strings to other primitive types.
 *
 * AUTHOR:  John Reppy
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    jhr@research.att.com
 *)

exception Ord
let ordof (s,i) = Char.code s.[i]
let look = ordof
let real = float_of_int

  (* A table for mapping digits to values.  Whitespace characters map to
   * 128, "-","~" map to 129, "." maps to 130, and 0-9,A-Z,a-z map to their
   * base-36 value.  All others map to 255.
   *)
    let cvtTable = "255255255255255255255255255128128255255255255255
	    255255255255255255255255255255255255255255255255
	    128255255255255255255255255255255255255129130255
	    000001002003004005006007008009255255255255255255
	    255010011012013014015016017018019020021022023024
	    025026027028029030031032033034035255255255255255
	    255010011012013014015016017018019020021022023024
	    025026027028029030031032033034035255255255129255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255
	    255255255255255255255255255255255255255255255255"

    let look (s, i) = try (ordof(cvtTable, ordof(s, i))) with _ -> 255

    let eatWS (s, i) = let
	  rec f j = if (look(s, j) = 128) then f(j+1) else j
	  in
	    f i

    let eatNeg (s, indx) = if (look (s, indx) = 129)
	  then (true, indx+1)
	  else (false, indx)

    let eatDecimalPt (s, indx) = if (look(s, indx) = 130)
	  then (true, indx+1)
	  else (false, indx)

    let eatE (s, indx) = if (look(s, indx) = 14 (* "e" base-36 *))
	  then (true, indx+1)
	  else (false, indx)

    exception Convert

    let scan10 (s, indx) = let
	  rec scan (accum, i) = let
		 d = look(s, i)
		in
		  if (d < 10) then scan(10*accum + d, i+1) else (accum, i)
		in
	  let (v, indx') = scan (0, indx)
	  in
	    if (indx = indx') then raise Convert else (v, indx')

    let strToInt (s, indx, base) =
      try
	  let indx = eatWS(s, indx) in
	  let (isNeg, indx) = eatNeg(s, indx) in
	  let rec scan16 indx =
            let rec scan (accum, i) =
              let  d = look(s, i)
	      in
	      if (d < 16) then scan(16*accum + d, i+1) else (accum, i)
            in
	      (* skip any leading "0x" or "0X" *)
            let indx = if (ordof(s, indx) = 48(*"0"*))
	               then let
		         d = look(s, indx+1)
		       in
			    if (d = 33(* base-36 vlue of "x" *)) then indx+2 else indx
		       else indx
            in
            let (v, indx') = scan (0, indx)
	    in
	    if (indx = indx') then raise Convert else (v, indx')
          in
	  let rec scanRadix (indx, base) =
            let rec scan (accum, i) = let
		       d = look(s, i)
		      in
			if (d < base) then scan(base*accum + d, i+1) else (accum, i)
in
		let (v, indx') = scan (0, indx)
		in
		  if (indx = indx') then raise Convert else (v, indx')
          in
	  let (v, indx) = (match base
		 with 10 -> scan10 (s, indx)
		  | 16 -> scan16 indx
		  | _ -> if ((1 < base) && (base <= 36))
			then scanRadix (indx, base)
			else raise Convert
		(* end case *))
	  in
	    if isNeg then (-v, indx) else (v, indx)
	  with Ord -> raise Convert

    let oatoi s = fst(strToInt(s, 0, 8))
    let atoi s = fst(strToInt(s, 0, 10))
    let xatoi s = fst(strToInt(s, 0, 16))

  (* this is like scan10, except that it uses a floating-pt accumulator.
   * It is used when scan10 overflows.
   *)
    let fscan10 (s, indx) =
      let rec scan (accum, i) =
        let d = look(s, i)
	in
	if (d < 10) then scan(10.0*.accum +. (real d), i+1) else (accum, i)
      in
      try
      let (v, i) = scan10(s, indx) in
      (real v, i)
      with _ -> scan (0.0, indx)


     let negTbl = [
	      1.0e-0; 1.0E-1; 1.0E-2; 1.0E-3; 1.0E-4;
	      1.0E-5; 1.0E-6; 1.0E-7; 1.0E-8; 1.0E-9
	     ]

     let posTbl = [
	      1.0E0; 1.0E1; 1.0E2; 1.0E3; 1.0E4; 1.0E5; 1.0E6; 1.0E7; 1.0E8; 1.0E9
	     ]

      let scale (tbl, step10) =
        let rec f (r,exp) =
          match exp with
            0 -> r
	  | _ ->
              if (exp < 10)
	      then (r *. List.nth tbl exp)
	      else f (step10 *. r, exp-10)
        in
        f

    let scaleUp = scale (posTbl, 1.0E10)
    let scaleDown = scale (negTbl, 1.0E-10)

    let strToReal (s, indx) =
      let indx = eatWS(s, indx) in
      let (isNeg, wholeIndx) = eatNeg(s, indx) in
      let (whole, indx) = fscan10(s, wholeIndx) in
      let hasWhole = (wholeIndx < indx) in
      	  let (hasDecimal, fracIndx) = eatDecimalPt(s, indx) in
	  let (num, indx) = if hasDecimal
		then let (frac, j) = fscan10(s, fracIndx)
		  in
		    (scaleDown (frac, j-fracIndx) +. whole, j)
		else (whole, fracIndx)
          in
	  let hasFrac = (fracIndx < indx) in
	  let num = if (hasWhole || hasFrac)
		then if isNeg then -. num else num
		else raise Convert
          in
	  let (hasExp, indx) = eatE (s, indx)
	  in
	    if hasExp
	      then
		let (negExp, expIndx) = eatNeg(s, indx) in
		let (exp, indx) = scan10(s, expIndx) in
		  if (expIndx = indx)
		    then raise Convert
		  else if negExp
		    then (scaleDown(num, exp), indx)
		    else (scaleUp(num, exp), indx)
	      else if (hasWhole || hasFrac)
		then (num, indx)
		else raise Convert


    let atof s = fst (strToReal (s, 0))

    let strToBool (s, indx) =
      try
	  let indx = eatWS (s, indx) in
	  let rec match_ (prefix, v, indx) =
		let len = String.length prefix in
                let rec f (i, j) =
		  if (i = len)
		  then (v, indx)
		  else if (ordof(prefix, i) = ordof(s, j))
		  then f (i+1, j+1)
		  else raise Convert
		in
		f (0, indx)
	  in
	    match (ordof (s, indx)) with
	      102 (*"f"*) -> match_ ("alse", false, indx+1)
	      | 116 (*"t"*) -> match_ ("rue", true, indx+1)
	      | _ -> raise Convert
	 with
	   Ord -> raise Convert

    let atob s = fst (strToBool (s, 0))




let main s1 i1 b s2 s3 s4 s5 i2 s6 s7 i3 s8 =
  let _ = strToInt (s1,i1,b) in
(*
  let _ = atoi s2 in
  let _ = xatoi s3 in
  let _ = oatoi s4 in
  let _ = strToReal (s5,i2) in
  let _ = atof s6 in
  let _ = strToBool (s7,i3) in
  let _ = atob s8 in
 *)
  ()
