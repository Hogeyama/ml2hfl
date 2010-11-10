
open Util
open Syntax






let rec string_of_typ = function
    TInt _
  | TUnknown -> "INT"(*???*)
  | TBool -> "BOOLEAN"
  | TUnit -> "INT"(*???*)
  | TVar({contents = None}) -> "INT"(*???*)
  | TVar({contents = Some(typ)}) -> string_of_typ typ
  | typ -> (Format.printf "%a@." (print_typ ML) typ; assert false)





let rec parse_type acc typ =
  match typ with
      TFun(xtyp1, typ2) -> parse_type (xtyp1::acc) typ2
    | _ -> List.rev acc, typ
let parse_type = parse_type []




let is_bool = function
    Var x -> x.typ = TBool
  | BinOp((And|Or), _, _) -> true
  | Not _ -> true
  | _ -> false


let iff t1 t2 = CsisatAst.Not (CsisatAst.And [CsisatAst.Not (CsisatAst.And[t1; t2]); CsisatAst.Not (CsisatAst.And[CsisatAst.Not t1; CsisatAst.Not t2])])
let iff t1 t2 = CsisatAst.Or [CsisatAst.And[t1; t2]; CsisatAst.And[CsisatAst.Not t1; CsisatAst.Not t2]]

let rec to_exp = function
    True -> CsisatAst.Variable "tru"
  | False -> CsisatAst.Variable "fls"
  | Int n -> CsisatAst.Constant (float_of_int n)
  | NInt x -> CsisatAst.Variable ("_" ^ x.origin ^ "_" ^ string_of_int x.id)
  | Var x -> CsisatAst.Variable (x.origin ^ "_" ^ string_of_int x.id)
  | BinOp(Add, t1, t2) -> CsisatAst.Sum [to_exp t1; to_exp t2]
  | BinOp(Sub, t1, t2) -> CsisatAst.Sum [to_exp t1; CsisatAst.Coeff(-1., to_exp t2)]
  | BinOp(Mult, Int n, t2) | BinOp(Mult, t2, Int n) -> CsisatAst.Coeff(float_of_int n, to_exp t2)
  | BinOp(Mult, t1, t2) ->
      (let t = BinOp(Mult, t1, t2) in
      Format.printf "Nonlinear expression not supported: %a@." (print_term_fm ML true) t; assert false)
  | t -> (Format.printf "@.%a@." (print_term_fm ML true) t; assert false)
let rec to_pred = function
    True -> CsisatAst.True
  | False -> CsisatAst.False
  | Var x -> CsisatAst.Eq (to_exp (Var x), CsisatAst.Variable "tru")
  | BinOp(Eq, t1, t2) ->
      if is_bool t1
      then iff (to_pred t1) (to_pred t2)
      else CsisatAst.Eq (to_exp t1, to_exp t2)
  | BinOp(Lt, t1, t2) -> CsisatAst.Lt (to_exp t1, to_exp t2)
  | BinOp(Gt, t1, t2) -> CsisatAst.Lt (to_exp t2, to_exp t1)
  | BinOp(Leq, t1, t2) -> CsisatAst.Leq (to_exp t1, to_exp t2)
  | BinOp(Geq, t1, t2) -> CsisatAst.Leq (to_exp t2, to_exp t1)
  | BinOp(And, t1, t2) -> CsisatAst.And [to_pred t2; to_pred t1]
  | BinOp(Or, t1, t2) -> CsisatAst.Or [to_pred t2; to_pred t1]
  | Not t -> CsisatAst.Not (to_pred t)
  | _ -> assert false
let to_pred t = CsisatAstUtil.integer_heuristic (to_pred t)

let parse_ident s =
  let len = String.length s in
  let n = String.rindex s '_' in
  let origin = String.sub s 0 n in
  let id = int_of_string (String.sub s (n+1) (len-n-1)) in
    {id=id; origin=origin; typ=TUnknown}


let rec from_exp = function
    CsisatAst.Constant x -> Int (int_of_float x)
  | CsisatAst.Variable x ->
      if x = "tru" then True
      else if x = "fls" then False
      else if x.[0] = '_'
      then NInt (parse_ident (String.sub x 1 (String.length x - 1)))
      else Var (parse_ident x)
  | CsisatAst.Sum[e1;e2] -> BinOp(Add, from_exp e1, from_exp e2)
  | CsisatAst.Sum(e::es) -> List.fold_left (fun e1 e2 -> BinOp(Add, e1, from_exp e2)) (from_exp e) es
  | CsisatAst.Coeff(x,e) -> BinOp(Mult, Int (int_of_float x), from_exp e)
  | CsisatAst.Application _
  | CsisatAst.Sum _ -> assert false
let rec from_pred = function
    CsisatAst.True -> True
  | CsisatAst.False -> False
  | CsisatAst.And(p::ps) ->
      List.fold_left (fun t p -> BinOp(And, from_pred p, t)) (from_pred p) ps
  | CsisatAst.Or(p::ps) ->
      List.fold_left (fun t p -> BinOp(Or, from_pred p, t)) (from_pred p) ps
  | CsisatAst.Not p -> Not (from_pred p)
  | CsisatAst.Eq(p1, p2) -> BinOp(Eq, from_exp p1, from_exp p2)
  | CsisatAst.Lt(p1, p2) -> BinOp(Lt, from_exp p1, from_exp p2)
  | CsisatAst.Leq(p1, p2) -> BinOp(Leq, from_exp p1, from_exp p2)
  | CsisatAst.And _
  | CsisatAst.Or _
  | CsisatAst.Atom _ -> assert false





let cvc3in = ref stdin
let cvc3out = ref stdout

let open_cvc3 () =
  let cin,cout = Unix.open_process (Flag.cvc3 ^ " +int") in
  cvc3in := cin;
  cvc3out := cout

let close_cvc3 () =
  match Unix.close_process (!cvc3in, !cvc3out) with
    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()


let check pre p =
  let cin = !cvc3in in
  let cout = !cvc3out in
(**)
  let fm = Format.formatter_of_out_channel cout in
(**)
(*
  let fm =
    Format.make_formatter
      (output cout)
      (fun () -> flush cout)
  in
*)
  let fv =
				let rec uniq = function
				    [] -> []
				  | x::xs -> if List.exists (fun y -> x.id=y.id) xs then uniq xs else x::(uniq xs)
    in
    uniq (get_fv2 p @@ List.flatten (List.rev_map get_fv2 pre)) in

(**)
  let env = List.map (fun v -> Typing.new_var v) fv in
  let p::pre, _  = List.split (List.map (fun p -> Typing.infer env p) (p::pre)) in
  let fv = List.map fst env in
(**)

(*
List.iter (Format.fprintf Format.std_formatter "%a:INT;" print_id) fv;
*)
(*
  let p::pre = List.map simplify (p::pre) in
  let p::pre = List.map Typing.match_arg (p::pre) in
*)

  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ x.typ ^ "; ") "" fv in
  let assertion = List.fold_left (fun str p -> str ^ "ASSERT " ^ (string_of_term CVC3 p) ^ "; ") "" pre in
  let query = "QUERY " ^ string_of_term CVC3 p ^ ";" in
  let q = "PUSH;"^types^assertion^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "check: %s@." q in
  let _ = Format.fprintf fm "%s" q in
  let _ = Format.pp_print_flush fm () in
  let s = input_line cin in
(*
  let _ = Format.fprintf Format.std_formatter "%s@." s in
  (if s = "CVC> Valid." then
    Format.fprintf Format.std_formatter "ok@."
  else
    Format.fprintf Format.std_formatter "ng@.");
*)
  s = "CVC> Valid."

(*
let check pre p =
  let fv = uniq (get_fv2 p @@ List.flatten (List.rev_map get_fv2 pre)) in
  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ x.typ ^ "; ") "" fv in
  let assertion = List.fold_left (fun str p -> str ^ "ASSERT " ^ (string_of_term CVC3 p) ^ "; ") "" pre in
  let query = "QUERY " ^ string_of_term CVC3 p ^ ";" in
  let command = "RESULT=$(echo '"^types^assertion^query^"' | /home/ryosuke/imp/cvc3); test $RESULT = \"Valid.\"" in
    Sys.command command = 0
*)
(*
let check pre p =
  let fv = uniq (get_fv p @@ List.flatten (List.rev_map get_fv pre)) in
  let cin,cout = Unix.open_process "./cvc3" in
  let fm = Format.formatter_of_out_channel cout in
  let () = List.iter (Format.fprintf fm "%a:INT;" print_id) fv in
  let () = List.iter (Format.fprintf fm "ASSERT %a;" (print_body CVC3)) pre in
  let () = Format.fprintf fm "QUERY %a;" (print_body CVC3) p in
  let () = Format.pp_print_flush fm () in
  let () = close_out cout in
  let s = input_line cin in
  let _ = close_in cin in
    s = "Valid."
*)



let checksat p =
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let fv =
				let rec uniq = function
				    [] -> []
				  | x::xs -> if List.exists (fun y -> x.id=y.id) xs then uniq xs else x::(uniq xs)
    in
    uniq (get_fv2 p) in

  let env = List.map (fun v -> Typing.new_var v) fv in
  let p, _  = Typing.infer env p in
  let fv = List.map fst env in

  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ x.typ ^ "; ") "" fv in
  let query = "CHECKSAT " ^ string_of_term CVC3 p ^ ";" in

  let q = "PUSH;"^types^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "checksat: %s@." q in

  let () = Format.fprintf fm "%s@?" q in
  let _ = Format.pp_print_flush fm () in
  let s = input_line cin in
  if s = "CVC> Satisfiable." then
    true
  else if s = "CVC> Unsatisfiable." then
    false
  else begin
    Format.printf "CVC3 reported an error@."; assert false
  end
(*  let () = close_out cout in*)

(*
  let command = "RESULT=$(echo 'PUSH; "^types^query^" POP;' | "^Flag.cvc3^"); test $RESULT = \"Satisfiable.\"" in
    Sys.command command = 0
*)

let rec rename_ident = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt (parse_ident x.origin)
  | Var x -> Var (parse_ident x.origin)
  | Fun(x, t) ->
      let x' = parse_ident x.origin in
      let t' = rename_ident t in
        Fun(x', t')
  | App(t, ts) ->
      let t' = rename_ident t in
      let ts' = List.map (rename_ident) ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = rename_ident t1 in
      let t2' = rename_ident t2 in
      let t3' = rename_ident t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = rename_ident t1 in
      let t2' = rename_ident t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let f' = parse_ident f.origin in
      let xs' = List.map (fun x -> parse_ident x.origin) xs in
      let t1' = rename_ident t1 in
      let t2' = rename_ident t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = parse_ident f.origin in
      let xs' = List.map (fun x -> parse_ident x.origin) xs in
      let t1' = rename_ident t1 in
      let t2' = rename_ident t2 in
        Letrec(f', xs', t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = rename_ident t1 in
      let t2' = rename_ident t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = rename_ident t in
        Not t'
  | Label(b,t) ->
      let t' = rename_ident t in
        Label(b, t')
  | Fail -> Fail





let get_solution p =
(*
  let cin = !cvc3in in
  let cout = !cvc3out in
*)
(**)
  let cin,cout = Unix.open_process Flag.cvc3 in
(**)
  let fm = Format.formatter_of_out_channel cout in

  let fv =
				let rec uniq = function
				    [] -> []
				  | x::xs -> if List.exists (fun y -> x.id=y.id) xs then uniq xs else x::(uniq xs)
    in
    uniq (get_fv2 p) in

  let env = List.map (fun v -> Typing.new_var v) fv in
  let p, _  = Typing.infer env p in
  let fv = List.map fst env in

  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ x.typ ^ "; ") "" fv in
  let query = "CHECKSAT " ^ string_of_term CVC3 p ^ "; COUNTERMODEL;" in

  let q = "PUSH;"^types^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "get_solution: %s@." q in

  let () = Format.fprintf fm "%s@?" q in
  let _ = Format.pp_print_flush fm () in
(**)
  let () = close_out cout in
(**)
  let () = ignore (input_line cin); ignore (input_line cin); ignore (input_line cin) in
  let rec aux cin =
    try
      let s = input_line cin in
      let s' = String.sub s 7 (String.length s - 8) in
        try
          let t = Parser.file Lexer.token (Lexing.from_string s') in
          let t' = rename_ident t in
            t' :: aux cin
        with _ -> aux cin
    with End_of_file -> []
  in
  let ts = aux cin in
(**)
  let () = close_in cin in
  let () =
    match Unix.close_process (cin, cout) with
        Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()
  in
(**)
    ts



let isTInt = function
    TInt _ -> true
  | _ -> false


let equiv cond t1 t2 =
  check (t1::cond) t2 && check (t2::cond) t1



let rec gcd_arith_exp prev curr =
  if List.sort compare prev = List.sort compare curr
  then
    let aux = function
        Int n -> abs n
      | BinOp(Mult, Int n, t)
      | BinOp(Mult, t, Int n) -> abs n
      | _ -> 1
    in
      List.fold_left gcd 0 (List.rev_map aux curr)
  else
    let aux = function
        BinOp((Add|Sub), t1, t2) -> [t1; t2]
      | t -> [t]
    in
      gcd_arith_exp curr (rev_map_flatten aux curr)
let gcd_arith_exp ts = gcd_arith_exp [] ts


let rec div_arith_exp n = function
    Int m -> assert (m mod n = 0); Int (m/n)
  | BinOp(Add, t1, t2) ->
      let t1' = div_arith_exp n t1 in
      let t2' = div_arith_exp n t2 in
        BinOp(Add, t1', t2')
  | BinOp(Mult, Int m, t)
  | BinOp(Mult, t, Int m) ->
      assert (m mod n = 0);
      BinOp(Mult, Int (m/n), t)
  | t -> Format.printf "@.%a@." (print_term_fm ML true) t; assert false
let div_arith_exp n t =
  if n = 1
  then t
  else div_arith_exp n t


let rec simplify_bool_exp precise t =
  match t with
      True
    | False
    | Var _ -> t
    | BinOp(And|Or as op, t1, t2) ->
        if precise && equiv [] t t1 then simplify_bool_exp precise t1
        else if precise && equiv [] t t2 then simplify_bool_exp precise t2
        else
          BinOp(op, simplify_bool_exp precise t1, simplify_bool_exp precise t2)
    | BinOp(_, (True|False), _) -> t
    | BinOp(_, _, (True|False)) -> t
    | BinOp(Eq, Int 0, BinOp(Mult, Int n, t))
    | BinOp(Eq, BinOp(Mult, Int n, t), Int 0) -> (*unsound if n=0?*)
        BinOp(Eq, t, Int 0)
    | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) ->
        let t1 = simplify_exp t1 in
        let t2 = simplify_exp t2 in
        let d = gcd_arith_exp [t1;t2] in
          if d = 0
          then BinOp(op, t1, t2)
          else
            let t1' = div_arith_exp d t1 in
            let t2' = div_arith_exp d t2 in
              BinOp(op, t1', t2')
    | Not t -> Not (simplify_bool_exp precise t)
    | _ -> Format.printf "@.%a@." (print_term_fm ML true) t; assert false
and simplify_exp t =
  match t with
    Var _ -> t
  | Int _ -> t
  | NInt _ -> t
  | BinOp(Add, t1, t2) ->
      let t1' = simplify_exp t1 in
      let t2' = simplify_exp t2 in
      (match t1', t2' with
        Int(n), Int(m) -> Int(n+m)
      | Int(0), t | t, Int(0) -> t
      | _, _ -> BinOp(Add, t1', t2'))
  | BinOp(Sub, t1, t2) ->
      let t1' = simplify_exp t1 in
      let t2' = simplify_exp t2 in
      (match t1', t2' with
        Int(n), Int(m) -> Int(n-m)
      | Int(0), _ -> BinOp(Mult, Int(-1), t2')
      | t, Int(0) -> t
      | _, _ -> BinOp(Sub, t1', t2'))
  | BinOp(Mult, t1, t2) ->
      let t1' = simplify_exp t1 in
      let t2' = simplify_exp t2 in
      (match t1', t2' with
        Int(n), Int(m) -> Int(n * m)
      | Int(0), t | t, Int(0) -> Int(0)
      | Int(1), t | t, Int(1) -> t
      | _, _ -> BinOp(Mult, t1', t2'))
  | _ -> simplify_bool_exp true t




let rec simplify = function
    CsisatAst.Leq(CsisatAst.Constant n, CsisatAst.Coeff(m,e)) ->
      if m > 0.
      then CsisatAst.Leq(CsisatAst.Constant (ceil (n/.m)), e)
      else CsisatAst.Leq(e, CsisatAst.Constant (floor (n/.m)))
  | CsisatAst.Eq(CsisatAst.Constant 0., CsisatAst.Coeff(m,e)) -> (*unsound if m=0*)
      CsisatAst.Eq(CsisatAst.Constant 0., e)
  | CsisatAst.And es -> CsisatAst.And (List.map simplify es)
  | CsisatAst.Or es -> CsisatAst.Or (List.map simplify es)
  | CsisatAst.Not e -> CsisatAst.Not (simplify e)
  | p -> p

exception Satisfiable

let interpolation ts1 ts2 =
  let bool_theory =
    [CsisatAst.Not(CsisatAst.Eq(CsisatAst.Variable "tru", CsisatAst.Variable "fls"));
    CsisatAst.Eq(CsisatAst.Application("neg", [CsisatAst.Variable "tru"]), CsisatAst.Variable "fls");
    CsisatAst.Eq(CsisatAst.Application("neg", [CsisatAst.Variable "fls"]), CsisatAst.Variable "tru")]
  in
  let ts1' = List.map (fun t -> to_pred (simplify_bool_exp false t)) ts1 in
  let ts2' = List.map (fun t -> to_pred (simplify_bool_exp false t)) ts2 in
  let t1 = CsisatAstUtil.simplify (CsisatAst.And (bool_theory@@ts1')) in
  let t2 = CsisatAstUtil.simplify (CsisatAst.And ts2') in
  let () = if Flag.debug && Flag.print_interpolant then Format.printf "  t1: %s@." (CsisatAstUtil.print_pred t1) in
  let () = if Flag.debug && Flag.print_interpolant then Format.printf "  t2: %s@." (CsisatAstUtil.print_pred t2) in

  let pred = try
      CsisatInterpolate.interpolate_with_proof t1 t2
    with CsisatAst.SAT_FORMULA(pred) -> begin
       (*if Flag.debug then print_string ("satisfiable: \n" ^ (CsisatAstUtil.print_pred pred) ^ "\n");*)
      if checksat (List.fold_left (fun t1 t2 -> BinOp(And,t1,t2)) True (ts1@@ts2))
      then raise Satisfiable
      else
        let rec trans t =
          match t with
              True
            | False
            | Var _ -> t
            | BinOp(And|Or as op, t1, t2) ->
                let t1' = trans t1 in
                let t2' = trans t2 in
                  BinOp(op, t1', t2')
            | BinOp(_, (True|False), _) -> t
            | BinOp(_, _, (True|False)) -> t
            | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) ->
                BinOp(op, t1, t2)
            | Not(BinOp(Eq, t1, t2)) ->
                BinOp(Or, BinOp(Leq, t1, BinOp(Sub, t2, Int 1)), BinOp(Leq, BinOp(Add, t2, Int 1), t1))
            | Not t -> Not (trans t)
            | _ -> Format.printf "@.%a@." (print_term_fm ML true) t; assert false
        in
        let ts1' = List.map (fun t -> to_pred (simplify_bool_exp false (trans t))) ts1 in
        let ts2' = List.map (fun t -> to_pred (simplify_bool_exp false (trans t))) ts2 in
        let t1 = CsisatAstUtil.simplify (CsisatAst.And (bool_theory@@ts1')) in
        let t2 = CsisatAstUtil.simplify (CsisatAst.And ts2') in
        let () = if Flag.debug && Flag.print_interpolant
                 then Format.printf "  t1: %s@." (CsisatAstUtil.print_pred t1)
        in
        let () = if Flag.debug && Flag.print_interpolant
                 then Format.printf "  t2: %s@." (CsisatAstUtil.print_pred t2)
        in
          try
            CsisatInterpolate.interpolate_with_proof t1 t2
          with
              CsisatAst.SAT_FORMULA(pred) -> raise Satisfiable
    end
  in
  let pred' = CsisatAstUtil.simplify (CsisatLIUtils.round_coeff pred) in
  if Flag.debug && Flag.print_interpolant then
    Format.printf "  interpolant: %s@." (CsisatAstUtil.print_pred pred')
  else ();
  let pred'' = from_pred pred' in
  (if Flag.debug then begin
    ()(*assert (check ts1 pred'' &&
            check [pred''] (Not (List.fold_left (fun t p -> BinOp(And, t, p)) (List.hd ts2) (List.tl ts2))))*)
  end);
    pred''









(*
let rec type_of = function
    Unit -> TUnit
  | Bool _ -> TBool
  | Int _ -> TInt (ref [])
  | NInt _ -> TInt (ref [])
  | Var x -> x.typ
  | App(f, ts) ->
      let rec aux typ ts =
          match typ,ts with
              TFun((x,typ1),typ2), t::ts -> aux typ2 ts
            | _, [] -> typ
            | _ -> assert false
      in
        aux f.typ ts
  | BinOp((Eq|Lt|Gt|Leq|Geq|And|Or), _, _) -> TBool
  | BinOp((Add|Sub|Mult), _, _) -> TInt (ref [])
  | If(_, t2, t3) -> type_of t2
  | Branch(t1, t2) -> type_of t1
  | Not _ -> TBool
  | Fail _ ->
      let x = new_var "x" in
        TFun((x,TUnit), TUnit)
  | Label(_,t) -> type_of t
*)

let rec rename_type map = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ps -> TInt ps
  | TVar {contents = None} -> TVar {contents = None}
  | TVar {contents = Some typ} ->
      let typ' = rename_type map typ in
        TVar {contents = Some typ'}
  | TFun((x,typ1),typ2) ->
      let typ1' = rename_type map typ1 in
      let typ2' = rename_type map typ2 in
      let x' = try List.assoc x map with Not_found -> x in
      let x'' = {x' with typ = typ1'} in
        TFun((x'',typ1'),typ2')
  | TUnknown -> TUnknown


let rec congruent cond typ1 typ2 =
  match typ1,typ2 with
      TUnit, TUnit -> true
    | TBool, TBool -> true
    | TInt ps1, TInt ps2 ->
        begin
          try
            List.fold_left2 (fun b p1 p2 -> b && equiv cond p1 p2) true ps1 ps2
          with Invalid_argument "List.fold_left2" -> false
        end
    | TFun((x1,typ11),typ12), TFun((x2,typ21),typ22) ->
        let cond' =
          match typ11 with
              TInt _ -> BinOp(Eq, Var x1, Var x2)::cond
            | _ -> cond
        in
          congruent cond typ11 typ21 && congruent cond' typ12 typ22
    | TUnknown, _ -> true
    | _, TUnknown -> true
    | TVar _, _ -> assert false
    | _, TVar _ -> assert false
    | _ -> false


(*
let rec rename_type2 map = function
    TUnit -> TUnit, []
  | TBool -> TBool, []
  | TInt ps ->
      let ps' = List.map (subst_var_map map) ps in
        TInt ps', []
  | TFun((x,typ1),typ2) ->
      let typ1',map1 = rename_type2 map typ1 in
      let x' = {(new_var x.origin) with typ = typ1'} in
      let map' = (x,x')::map in
      let typ2',map2 = rename_type2 map' typ2 in
        TFun((x',typ1'),typ2'), map1@@map2@@map'
  | TUnknown -> TUnknown, []
  | TVar _ -> assert false
let rename_type2 = rename_type2 []




let rec rename_fun (f,_,t) =
  let f_typ',map = rename_type2 f.typ in
  let f' = {f with typ = f_typ'} in
  let xs = get_args f.typ in
  let xs' = List.map (fun x -> List.assoc x map) xs in
  let t' = subst_var_map map t in
    map, (f', xs', t')
*)

(*
let remove_type_var x = {x with typ=TUnknown}
let rec remove_type = function
    Unit -> Unit
  | Bool b -> Bool b
  | Int n -> Int n
  | NInt x -> NInt (remove_type_var x)
  | Var x -> Var (remove_type_var x)
  | App(lhs, ts) ->
      let lhs' = remove_type_var lhs in
      let ts' = List.map remove_type ts in
        App(lhs', ts')
  | BinOp(binop, t1, t2) ->
      let t1' = remove_type t1 in
      let t2' = remove_type t2 in
        BinOp(binop, t1', t2')
  | Branch(t1, t2) ->
      let t1' = remove_type t1 in
      let t2' = remove_type t2 in
        Branch(t1', t2')
  | Not t ->
      let t' = remove_type t in
        Not t'
  | Fail -> Fail
  | Label(n, t) ->
      let t' = remove_type t in
        Label(n, t')
  | If(t1,t2,t3) ->
      let t1' = remove_type t1 in
      let t2' = remove_type t2 in
      let t3' = remove_type t3 in
        If(t1', t2', t3')
*)












