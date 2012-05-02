open ExtList
open ExtString

(** Interface to CVC3 *)

let cvc3in = ref stdin
let cvc3out = ref stdout

let cvc3 = "./cvc3"

(* necessary to avoid a redefinition of a variable *)
let cnt = ref 0

let open_cvc3 () =
  let _ = cnt := 0 in
  let cin, cout = Unix.open_process (cvc3 ^ " +interactive") in
  cvc3in := cin;
  cvc3out := cout

let close_cvc3 () =
  match Unix.close_process (!cvc3in, !cvc3out) with
    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()

let string_of_var x =
  String.map (fun c -> if c = '.' || c = '!' then '_' else c) (Var.print x)

(* encoding unit as 0 *)
let string_of_type ty =
  match ty with
    SimType.Unit -> "INT"
  | SimType.Bool -> "BOOLEAN"
  | SimType.Int -> "INT"
  | SimType.Fun(_, _) -> assert false

let deco s = "cnt" ^ string_of_int !cnt ^ "_" ^ s

let string_of_env env =
  String.concat "; "
    (List.map (fun (x, ty) -> deco (string_of_var x) ^ ":" ^ string_of_type ty) env)

let string_of_env_comma env =
  String.concat ", "
    (List.map (fun (x, ty) -> deco (string_of_var x) ^ ":" ^ string_of_type ty) env)

let rec string_of_term t =
  match Term.fun_args t with
    Term.Var(_, x), [] ->
      deco (string_of_var x)
  | Term.Const(_, Const.Int(n)), [] ->
      string_of_int n
  | Term.Const(_, Const.Add), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " + " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Sub), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " - " ^ string_of_term t2 ^ ")"
(*
  | Term.Const(_, Const.Mul), [Term.Const(_, Const.Int(m)); t]
  | Term.Const(_, Const.Mul), [t; Term.Const(_, Const.Int(m))] ->
      "(" ^ string_of_int m ^ " * " ^ string_of_term t ^ ")"
*)
  | Term.Const(_, Const.Mul), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " * " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Minus), [t] ->
      "(- " ^ string_of_term t ^ ")"
  | Term.Const(_, Const.Leq), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " <= " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Geq), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " >= " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Lt), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " < " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Gt), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " > " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.EqUnit), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " = " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.NeqUnit), [t1; t2] ->
      string_of_term (Formula.bnot (Formula.eqUnit t1 t2))
  | Term.Const(_, Const.EqBool), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " <=> " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.NeqBool), [t1; t2] ->
      string_of_term (Formula.bnot (Formula.eqBool t1 t2))
  | Term.Const(_, Const.EqInt), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " = " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.NeqInt), [t1; t2] ->
      string_of_term (Formula.bnot (Formula.eqInt t1 t2))
  | Term.Const(_, Const.Unit), [] ->
      "0"(*"UNIT"*)
  | Term.Const(_, Const.True), [] ->
      "TRUE"
  | Term.Const(_, Const.False), [] ->
      "FALSE"
  | Term.Const(_, Const.And), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " AND " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Or), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " OR " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Imply), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " => " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Iff), [t1; t2] ->
      "(" ^ string_of_term t1 ^ " <=> " ^ string_of_term t2 ^ ")"
  | Term.Const(_, Const.Not), [t] -> 
      "(NOT " ^ string_of_term t ^ ")"
  | Term.Forall(_, env, t), [] ->
      let benv, env = List.partition (function (_, SimType.Bool) -> true | _ -> false) env in
      let fenv x t y = if x = y then t else raise Not_found in
      let t = List.fold_left
        (fun t (x, _) ->
          Formula.band [Term.subst (fenv x Formula.ttrue) t; Term.subst (fenv x Formula.tfalse) t])
        t benv
      in
      "(" ^ (if env = [] then "" else "FORALL (" ^ string_of_env_comma env ^ "): ") ^ string_of_term t ^ ")"
  | Term.Exists(_, env, t), [] ->
      assert false
  | _, _ ->
      let _ = Format.printf "%a@," Term.pr t in
      assert false

let infer t ty =
  let rec aux t ty =
(*
    Format.printf "%a@," Term.pr t;
*)
    match Term.fun_args t with
      Term.Var(_, x), [] ->
        [x, ty]
    | Term.Const(_, Const.Unit), [] ->
        let _ = assert (SimType.equiv ty SimType.Unit) in []
    | Term.Const(_, Const.True), []
    | Term.Const(_, Const.False), [] ->
        let _ = assert (SimType.equiv ty SimType.Bool) in []
    | Term.Const(_, Const.Int(_)), [] ->
        let _ = assert (SimType.equiv ty SimType.Int) in []
    | Term.Const(_, Const.Not), [t] ->
        let _ = assert (SimType.equiv ty SimType.Bool) in
        aux t SimType.Bool
    | Term.Const(_, Const.Minus), [t] ->
        let _ = assert (SimType.equiv ty SimType.Int) in
        aux t SimType.Int
    | Term.Const(_, Const.EqUnit), [t1; t2]
    | Term.Const(_, Const.NeqUnit), [t1; t2] ->
        let _ = assert (SimType.equiv ty SimType.Bool) in
        aux t1 SimType.Unit @ aux t2 SimType.Unit
    | Term.Const(_, Const.EqBool), [t1; t2]
    | Term.Const(_, Const.NeqBool), [t1; t2]
    | Term.Const(_, Const.And), [t1; t2]
    | Term.Const(_, Const.Or), [t1; t2]
    | Term.Const(_, Const.Imply), [t1; t2]
    | Term.Const(_, Const.Iff), [t1; t2] ->
        let _ = assert (SimType.equiv ty SimType.Bool) in
        aux t1 SimType.Bool @ aux t2 SimType.Bool
    | Term.Const(_, Const.Add), [t1; t2]
    | Term.Const(_, Const.Sub), [t1; t2]
    | Term.Const(_, Const.Mul), [t1; t2] ->
        let _ = assert (SimType.equiv ty SimType.Int) in
        aux t1 SimType.Int @ aux t2 SimType.Int
    | Term.Const(_, Const.Leq), [t1; t2]
    | Term.Const(_, Const.Geq), [t1; t2]
    | Term.Const(_, Const.Lt), [t1; t2]
    | Term.Const(_, Const.Gt), [t1; t2]
    | Term.Const(_, Const.EqInt), [t1; t2]
    | Term.Const(_, Const.NeqInt), [t1; t2] ->
      let _ = assert (SimType.equiv ty SimType.Bool) in
        aux t1 SimType.Int @ aux t2 SimType.Int
    | Term.Forall(_, env, t), []
		  | Term.Exists(_, env, t), [] ->
        let _ = assert (SimType.equiv ty SimType.Bool) in
        let xs = List.map fst env in
        List.filter (fun (x, _) -> not (List.mem x xs)) (aux t SimType.Bool)
    | _, _ ->
        let _ = Format.printf "%a@," Term.pr t in
        assert false
  in
  List.map
    (function (x, ty)::xtys ->
      let _ = assert (List.for_all (fun (_, ty') -> SimType.equiv ty ty') xtys) in
      x, ty
    | _ -> assert false)
    (Util.classify (fun (x, _) (y, _) -> Var.equiv x y) (aux t ty))  

let is_valid t =
  let _ = Global.log_begin "is_valid" in
  let cin = !cvc3in in
  let cout = !cvc3out in
  let _ = cnt := !cnt + 1 in
  let fm = Format.formatter_of_out_channel cout in

  let env =
    infer t SimType.Bool
    (*List.map (fun x -> x, SimType.Int) (Term.fvs t)*)
  in
  let inp =
    "PUSH;" ^
    string_of_env env ^ ";" ^
    String.concat " "
      (List.map (fun t -> "ASSERT " ^ (string_of_term t) ^ "; ") []) ^
    "QUERY " ^ string_of_term t ^ ";" ^
    "POP;"
  in
  let _ = Global.log (fun () -> Format.printf "input to CVC3: %s@," inp) in
  let _ = Format.fprintf fm "%s\n@?" inp in
  let res = input_line cin in
  let res =
		  if Str.string_match (Str.regexp ".*Valid") res 0 then
		    let _ = Format.printf "output of CVC3: valid" in
		    true
		  else if Str.string_match (Str.regexp ".*Invalid") res 0 then
		    let _ = Format.printf "output of CVC3: invalid" in
		    false
		  else
		    let _ = Format.printf "unknown error of CVC3: %s@," res in
		    assert false
  in
  let _ = Global.log_end "is_valid" in
  res

let implies t1 t2 = is_valid (Formula.imply t1 t2)

(*
(* t1 and t2 share only variables that satisfy p *)
let implies_bvs p t1 t2 =
  let t1 = Term.rename_fresh p t1 in
  let t2 = Term.rename_fresh p t2 in
  implies t1 t2
*)

(*
let checksat env p =
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let types = List.fold_left (fun str (x,_) -> str ^ x ^ ":" ^ string_of_typ env x ^ "; ") "" env in
  let query = "CHECKSAT " ^ string_of_term env p ^ ";" in

  let q = "PUSH;"^types^query^"\nPOP;" in
  let _ = if Global.debug > 0 && Flag.print_cvc3 then Format.fprintf Format.std_formatter "checksat: %s@," q in

  let () = Format.fprintf fm "%s@?" q in
  let s = input_line cin in
    if Str.string_match (Str.regexp ".*Satisfiable") s 0 then
      true
    else if Str.string_match (Str.regexp ".*Unsatisfiable") s 0 then
      false
    else begin
      Format.printf "CVC3 reported an error@,"; assert false
    end
*)

let solve t =
  let _ = Global.log_begin "solve" in
  let cin, cout = Unix.open_process (cvc3 ^ " +interactive") in
  let _ = cnt := !cnt + 1 in
  let fm = Format.formatter_of_out_channel cout in
  let inp =
    "PUSH;" ^
    (string_of_env (infer t SimType.Bool)) ^ ";" ^
    "CHECKSAT " ^ string_of_term t ^ ";" ^
    "COUNTERMODEL;" ^
    "POP;\n"
  in
  let _ = Global.log (fun () -> Format.printf "input to CVC3: %s@," inp) in
  let _ = Format.fprintf fm "%s@?" inp in
  let _ = close_out cout in
  let rec aux () =
    try
      let s = input_line cin in
        if Str.string_match (Str.regexp ".*ASSERT") s 0 then
          let pos_begin = String.index s '(' + 1 in
          let pos_end = String.index s ')' in
          let s' = String.sub s pos_begin (pos_end - pos_begin) in
          if Str.string_match (Str.regexp "cvc3") s' 0
          then aux ()
          else s' :: aux ()
        else
          aux ()
    with End_of_file ->
      []
  in
  let ss = aux () in
  let _ = close_in cin in
  let _ =
		  match Unix.close_process (cin, cout) with
		    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()
  in
(*
  let _ = List.iter (fun s -> Format.printf "%s@," s) ss in
*)
  let res =
		  List.map
		    (fun s ->
		(*
		      let _ = Format.printf "?: %s@," s in
		*)
		      let _, s = String.split s "_" in
		(*
		      let _ = Format.printf "%s@," s in
		*)
		      let c, n = String.split s " = " in
		(*
		      let _ = Format.printf "%s, %s@," c n in
		*)
		      Var.parse c, int_of_string n)
		    ss
  in
  let _ = Global.log_end "solve" in
  res

let rbit = ref 1

let string_of_int_bv n =
  let _ = assert (n >= 0) in
  let bv = Util.bv_of_nat n in
  String.concat "" ("0bin" :: List.map string_of_int bv), List.length bv

let int_of_string_bv s =
  let _ = assert (String.starts_with s "0bin") in
  let bv = List.map (fun c -> if c = '0' then 0 else if c = '1' then 1 else assert false) (String.explode (String.sub s 4 (String.length s - 4))) in
  Util.nat_of_bv bv

(* encoding unit as 0 *)
let string_of_type_bv ty =
  match ty with
    SimType.Unit -> "BITVECTOR(1)"
  | SimType.Bool -> "BOOLEAN"
  | SimType.Int -> "BITVECTOR(" ^ string_of_int !rbit ^ ")"
  | SimType.Fun(_, _) -> assert false

let string_of_env_bv env =
  String.concat "; "
    (List.map (fun (x, ty) -> deco (string_of_var x) ^ ":" ^ string_of_type_bv ty) env)

let string_of_env_comma env =
  String.concat ", "
    (List.map (fun (x, ty) -> deco (string_of_var x) ^ ":" ^ string_of_type_bv ty) env)

let rec string_of_term_bv t =
  match Term.fun_args t with
    Term.Var(_, x), [] ->
      deco (string_of_var x), !rbit
  | Term.Const(_, Const.Int(n)), [] ->
      string_of_int_bv n
  | Term.Const(_, Const.Add), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 + 1 in
      "BVPLUS(" ^ string_of_int bit ^ ", " ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.Sub), [t1; t2] ->
      assert false
      (*let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = bit1 in
      "BVSUB(" ^ string_of_int bit ^ ", " ^ s1 ^ ", " ^ s2 ^ ")", bit*)
  | Term.Const(_, Const.Mul), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = bit1 + bit2 in
      "BVMULT(" ^ string_of_int bit ^ ", " ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.Minus), [t] ->
      assert false
      (*let s, bit = string_of_term_bv t in
      "BVUMINUS(" ^ s ^ ")", bit*)
  | Term.Const(_, Const.Leq), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 in
      let s1 = if bit = bit1 then s1 else "BVZEROEXTEND(" ^ s1 ^ ", " ^ string_of_int (bit - bit1) ^")" in
      let s2 = if bit = bit2 then s2 else "BVZEROEXTEND(" ^ s2 ^ ", " ^ string_of_int (bit - bit2) ^")" in
      "BVLE(" ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.Geq), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 in
      let s1 = if bit = bit1 then s1 else "BVZEROEXTEND(" ^ s1 ^ ", " ^ string_of_int (bit - bit1) ^")" in
      let s2 = if bit = bit2 then s2 else "BVZEROEXTEND(" ^ s2 ^ ", " ^ string_of_int (bit - bit2) ^")" in
      "BVGE(" ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.Lt), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 in
      let s1 = if bit = bit1 then s1 else "BVZEROEXTEND(" ^ s1 ^ ", " ^ string_of_int (bit - bit1) ^")" in
      let s2 = if bit = bit2 then s2 else "BVZEROEXTEND(" ^ s2 ^ ", " ^ string_of_int (bit - bit2) ^")" in
      "BVLT(" ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.Gt), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 in
      let s1 = if bit = bit1 then s1 else "BVZEROEXTEND(" ^ s1 ^ ", " ^ string_of_int (bit - bit1) ^")" in
      let s2 = if bit = bit2 then s2 else "BVZEROEXTEND(" ^ s2 ^ ", " ^ string_of_int (bit - bit2) ^")" in
      "BVGT(" ^ s1 ^ ", " ^ s2 ^ ")", bit
  | Term.Const(_, Const.EqUnit), [t1; t2] ->
      assert false
  | Term.Const(_, Const.NeqUnit), [t1; t2] ->
      string_of_term_bv (Formula.bnot (Formula.eqUnit t1 t2))
  | Term.Const(_, Const.EqBool), [t1; t2] ->
      assert false(*"(" ^ string_of_term_bv t1 ^ " <=> " ^ string_of_term_bv t2 ^ ")"*)
  | Term.Const(_, Const.NeqBool), [t1; t2] ->
      string_of_term_bv (Formula.bnot (Formula.eqBool t1 t2))
  | Term.Const(_, Const.EqInt), [t1; t2] ->
      let s1, bit1 = string_of_term_bv t1 in
      let s2, bit2 = string_of_term_bv t2 in
      let bit = max bit1 bit2 in
      let s1 = if bit = bit1 then s1 else "BVZEROEXTEND(" ^ s1 ^ ", " ^ string_of_int (bit - bit1) ^")" in
      let s2 = if bit = bit2 then s2 else "BVZEROEXTEND(" ^ s2 ^ ", " ^ string_of_int (bit - bit2) ^")" in
      "(" ^ s1 ^ " = " ^ s2 ^ ")", bit
  | Term.Const(_, Const.NeqInt), [t1; t2] ->
      string_of_term_bv (Formula.bnot (Formula.eqInt t1 t2))
  | Term.Const(_, Const.Unit), [] ->
      "0bin0"(*"UNIT"*), 1
  | Term.Const(_, Const.True), [] ->
      "TRUE", -1
  | Term.Const(_, Const.False), [] ->
      "FALSE", -1
  | Term.Const(_, Const.And), [t1; t2] ->
      "(" ^ fst (string_of_term_bv t1) ^ " AND " ^ fst (string_of_term_bv t2) ^ ")", -1
  | Term.Const(_, Const.Or), [t1; t2] ->
      "(" ^ fst (string_of_term_bv t1) ^ " OR " ^ fst (string_of_term_bv t2) ^ ")", -1
  | Term.Const(_, Const.Imply), [t1; t2] ->
      "(" ^ fst (string_of_term_bv t1) ^ " => " ^ fst (string_of_term_bv t2) ^ ")", -1
  | Term.Const(_, Const.Iff), [t1; t2] ->
      "(" ^ fst (string_of_term_bv t1) ^ " <=> " ^ fst (string_of_term_bv t2) ^ ")", -1
  | Term.Const(_, Const.Not), [t] -> 
      "(NOT " ^ fst (string_of_term_bv t) ^ ")", -1
  | Term.Forall(_, env, t), [] ->
      assert false
  | Term.Exists(_, env, t), [] ->
      assert false
  | _, _ ->
      let _ = Format.printf "%a@," Term.pr t in
      assert false

exception Unknown

let threshold = 2

let solve_bv t =
  let _ = Global.log_begin "solve_bv" in
  let rec solve_bv_aux () =
    if !rbit > threshold then
      raise Unknown
    else
				  let cin, cout = Unix.open_process (cvc3 ^ " +interactive") in
				  let fm = Format.formatter_of_out_channel cout in
				  let _ = cnt := !cnt + 1 in
				  let _ =
        let _ = Global.log (fun () -> Format.printf "using %d bit@," !rbit) in
						  let inp =
						    "PUSH;" ^
						    (string_of_env_bv (infer t SimType.Bool)) ^ ";" ^
						    "CHECKSAT " ^ fst (string_of_term_bv t) ^ ";" ^
						    "COUNTERMODEL;" ^
						    "POP;\n"
						  in
						  let _ = Global.log (fun () -> Format.printf "input to CVC3: %s@," inp) in
		      let _ = Format.fprintf fm "%s@?" inp in
								close_out cout
		    in
	     let s = input_line cin in
      let _ = Global.log (fun () -> Format.printf "output of CVC3: %s@," s) in
				  if Str.string_match (Str.regexp ".*Unsatisfiable.") s 0 then
								let _ = close_in cin in
								let _ =
										match Unix.close_process (cin, cout) with
										  Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()
								in
        let _ = rbit := !rbit + 1 in
		      solve_bv_aux ()
      else if Str.string_match (Str.regexp ".*Satisfiable.") s 0 then
						  let rec aux () =
						    try
						      let s = input_line cin in
						      let _ = Global.log (fun () -> Format.printf "output of CVC3: %s@," s) in
						      if Str.string_match (Str.regexp ".*ASSERT") s 0 then
						        let pos_begin = String.index s '(' + 1 in
						        let pos_end = String.index s ')' in
						        let s' = String.sub s pos_begin (pos_end - pos_begin) in
						        if Str.string_match (Str.regexp "cvc3") s' 0 then
                aux ()
						        else
                s' :: aux ()
						      else
						        aux ()
						    with End_of_file ->
    								let _ = close_in cin in
												let _ =
														match Unix.close_process (cin, cout) with
														  Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()
												in
						      []
						  in
								let ss = aux () in
								List.map
								  (fun s ->
								    let _, s = String.split s "_" in
								    let c, n = String.split s " = " in
								    let _ = Global.log (fun () -> Format.printf "%s = %s@," c n) in
								    Var.parse c, int_of_string_bv n)
								  ss
      else
        assert false
  in
		let old_bit = !rbit in
  let res = try solve_bv_aux () with Unknown -> rbit := old_bit; raise Unknown in
		let _ = rbit := old_bit in
  let _ = Global.log_end "solve_bv" in
  res


(** @deprecated ?? *)
let simplify_conjuncts ts =
  let ts = Formula.simplify_conjuncts ts in
  let aifs, ts = Util.partition_map (fun t -> try `L(LinArith.aif_of t) with Invalid_argument _ -> `R(t)) ts in
  let sub, ts' = Util.partition_map (function (Const.EqInt, [1, x], n) -> `L(x, Term.tint (-n)) | aif -> `R(LinArith.term_of_aif aif)) aifs in
  let ts = List.filter (fun t -> not (is_valid (Term.subst (fun x -> List.assoc x sub) t))) (ts' @ ts) in
  List.map (fun (x, t) -> Formula.eqInt (Term.make_var x) t) sub @ ts
