
open Utilities
open Syntax
open Type



let rec print_typ t = Type.print print_term t
and print_ids fm = function
    [] -> ()
  | x::xs -> Format.fprintf fm "%a %a" print_id x print_ids xs

(*
  and print_id fm x = fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)
*)
and print_id fm x =
  let rec aux = function
      TInt -> Format.fprintf fm "%a_i" Id.print x
    | TBool -> Format.fprintf fm "%a_b" Id.print x
    | TPred(typ,_) -> aux typ
    | TUnknown -> Format.fprintf fm "%a_i" Id.print x
    | typ -> Syntax.pp_print_typ Format.std_formatter typ; assert false
  in
    aux (Id.typ x)

and print_id_typ fm x =
  match Id.typ x with
      TVar _ | TUnknown -> Format.fprintf fm "%a" Id.print x
    | _ -> Format.fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)

and print_ids_typ fm = function
    [] -> ()
  | x::xs -> Format.fprintf fm "%a %a" print_id_typ x print_ids_typ xs

and paren pri p = if pri < p then "","" else "(",")"

and print_binop typ fm op =
  match op with
      Eq ->
        if typ = TBool
        then Format.fprintf fm "<=>"
        else Format.fprintf fm "="
    | Lt -> Format.fprintf fm "<"
    | Gt -> Format.fprintf fm ">"
    | Leq -> Format.fprintf fm "<="
    | Geq -> Format.fprintf fm ">="
    | And -> Format.fprintf fm "AND"
    | Or -> Format.fprintf fm "OR"
    | Add -> Format.fprintf fm "+"
    | Sub -> Format.fprintf fm "-"
    | Mult -> Format.fprintf fm "*"

and print_term fm t =
  match t.desc with
      Unit -> Format.fprintf fm "unit"
    | True -> Format.fprintf fm "TRUE"
    | False -> Format.fprintf fm "FALSE"
    | Int n -> Format.fprintf fm "%d" n
    | NInt x -> print_id fm x
    | RandInt _ -> assert false
    | Var x -> print_id fm x
    | BinOp(Mult, t1, t2) when
        (match t1.desc with Int(_) -> false | _ -> (match t2.desc with Int(_) -> false | _ -> true)) ->
        Format.printf "Nonlinear expression not supported.@.";
          assert false
    | BinOp(Eq, ({typ=TBool|TPred(TBool,_)} as t1), t2) ->
        Format.fprintf fm "(%a <=> %a)" print_term t1 print_term t2
    | BinOp(op, t1, t2) ->
        Format.fprintf fm "(%a %a %a)" print_term t1 (print_binop t1.typ) op print_term t2
    | Not t -> Format.fprintf fm "(NOT %a)" print_term t
    | _ -> assert false


let string_of_ident x =
  print_id Format.str_formatter x;
  Format.flush_str_formatter ()
let string_of_term t =
  print_term Format.str_formatter t;
  Format.flush_str_formatter ()














let cvc3in = ref stdin
let cvc3out = ref stdout


let rec string_of_typ = function
    TInt -> "INT"
  | TBool -> "BOOLEAN"
  | TVar({contents = Some(typ)}) -> string_of_typ typ
  | TPred(typ,_) -> string_of_typ typ
  | TUnknown -> "INT"
  | typ -> (Format.printf "%a@." print_typ typ; assert false)


let iff t1 t2 = CsisatAst.Not (CsisatAst.And [CsisatAst.Not (CsisatAst.And[t1; t2]); CsisatAst.Not (CsisatAst.And[CsisatAst.Not t1; CsisatAst.Not t2])])
let iff t1 t2 = CsisatAst.Or [CsisatAst.And[t1; t2]; CsisatAst.And[CsisatAst.Not t1; CsisatAst.Not t2]]

let rec to_exp t =
  match t.desc with
      True -> CsisatAst.Variable "tru"
    | False -> CsisatAst.Variable "fls"
    | Int n -> CsisatAst.Constant (float_of_int n)
    | Var x | NInt x -> CsisatAst.Variable (Id.to_string x)
    | RandInt t -> assert false
    | BinOp(Add, t1, t2) -> CsisatAst.Sum [to_exp t1; to_exp t2]
    | BinOp(Sub, t1, t2) -> CsisatAst.Sum [to_exp t1; CsisatAst.Coeff(-1., to_exp t2)]
    | BinOp(Mult, {desc=Int n}, t2) | BinOp(Mult, t2, {desc=Int n}) -> CsisatAst.Coeff(float_of_int n, to_exp t2)
    | BinOp(Mult, t1, t2) ->
      Format.printf "Nonlinear expression not supported: %a@." pp_print_term t;
      assert false
    | Nil -> CsisatAst.Variable "nil"
    | _ -> Format.printf "@.%a@." pp_print_term t; assert false
let rec to_pred t =
  match t.desc with
    True -> CsisatAst.True
  | False -> CsisatAst.False
  | Var x -> CsisatAst.Eq (to_exp t, CsisatAst.Variable "tru")
  | BinOp(Eq, t1, t2) ->
      if t1.typ = TBool
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
  try
    let len = String.length s in
    let n = String.rindex s '_' in
    let name = String.sub s 0 n in
    let id = int_of_string (String.sub s (n+1) (len-n-1)) in
      Id.make id name TUnknown
  with Failure "int_of_string" | Not_found ->
    Id.make 0 s TUnknown

let rec from_exp map = function
    CsisatAst.Constant x -> make_int (int_of_float x)
  | CsisatAst.Variable "tru" -> {desc=True; typ=TBool}
  | CsisatAst.Variable "fls" -> {desc=False; typ=TBool}
  | CsisatAst.Variable "nil" -> assert false
(*
  | CsisatAst.Variable x when x.[0] = '_' -> NInt (parse_ident (String.sub x 1 (String.length x - 1)))
*)
  | CsisatAst.Variable x ->
      let x' = Id.assoc (parse_ident x) map in
        {desc=Var x'; typ=Id.typ x'}
  | CsisatAst.Sum[e1;e2] -> {desc=BinOp(Add, from_exp map e1, from_exp map e2); typ=TInt}
  | CsisatAst.Sum(e::es) ->
      let aux t e = {desc=BinOp(Add, t, from_exp map e); typ=TInt} in
        List.fold_left aux (from_exp map e) es
  | CsisatAst.Coeff(x,e) -> {desc=BinOp(Mult, {desc=Int(int_of_float x);typ=TInt}, from_exp map e); typ=TInt}
  | CsisatAst.Application _
  | CsisatAst.Sum _ -> assert false
let rec from_pred map = function
    CsisatAst.True -> {desc=True; typ=TBool}
  | CsisatAst.False -> {desc=True; typ=TBool}
  | CsisatAst.And(p::ps) ->
      List.fold_left (fun t p -> {desc=BinOp(And, from_pred map p, t); typ=TBool}) (from_pred map p) ps
  | CsisatAst.Or(p::ps) ->
      List.fold_left (fun t p -> {desc=BinOp(Or, from_pred map p, t); typ=TBool}) (from_pred map p) ps
  | CsisatAst.Not p -> {desc=Not (from_pred map p); typ=TBool}
  | CsisatAst.Eq(e1, e2) -> {desc=BinOp(Eq, from_exp map e1, from_exp map e2); typ=TBool}
  | CsisatAst.Lt(e1, e2) -> {desc=BinOp(Lt, from_exp map e1, from_exp map e2); typ=TBool}
  | CsisatAst.Leq(e1, e2) -> {desc=BinOp(Leq, from_exp map e1, from_exp map e2); typ=TBool}
  | CsisatAst.And _
  | CsisatAst.Or _
  | CsisatAst.Atom _ -> assert false





let open_cvc3 () =
  let cin,cout = Unix.open_process (!Flag.cvc3 ^ " " ^ Flag.cvc3_option) in
  cvc3in := cin;
  cvc3out := cout

let close_cvc3 () =
  match Unix.close_process (!cvc3in, !cvc3out) with
    Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()

let reopen_cvc3 () = close_cvc3 (); open_cvc3 ()



let check pre p =
(*
  let pre = List.map init_rand_int pre in
  let p = init_rand_int p in
*)
  let cin = !cvc3in in
  let cout = !cvc3out in
    (**)
  let fm = Format.formatter_of_out_channel cout in
    (**)
  let fv = uniq' Id.compare (get_fv2 p @@ List.flatten (List.rev_map get_fv2 pre)) in
  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ (Id.typ x) ^ "; ") "" fv in
  let assertion = List.fold_left (fun str p -> str ^ "ASSERT " ^ (string_of_term p) ^ "; ") "" pre in
  let query = "QUERY " ^ string_of_term p ^ ";" in
  let q = "PUSH;"^types^assertion^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "check: %s@." q in
  let _ = Format.fprintf fm "%s" q in
  let _ = Format.pp_print_flush fm () in
  let s = input_line cin in
  let r = Str.string_match (Str.regexp ".*Valid") s 0 in
    if not (r || Str.string_match (Str.regexp ".*Invalid") s 0)
    then (print_string s; assert false)
    else r






let checksat p =
(*
  let p = init_rand_int p in
*)
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let fv = uniq' Id.compare (get_fv2 p) in
(*
  let env = List.map (fun v -> Typing.new_var v) fv in
  let p, _  = Typing.infer env p in
  let fv = List.map fst env in
*)
  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ (Id.typ x) ^ "; ") "" fv in
  let query = "CHECKSAT " ^ string_of_term p ^ ";" in

  let q = "PUSH;"^types^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "checksat: %s@." q in

  let () = Format.fprintf fm "%s@?" q in
  let s = input_line cin in
    if Str.string_match (Str.regexp ".*Satisfiable") s 0 then
      true
    else if Str.string_match (Str.regexp ".*Unsatisfiable") s 0 then
      false
    else begin
      Format.printf "CVC3 reported an error@."; assert false
    end
(*  let () = close_out cout in*)

(*
  let command = "RESULT=$(echo 'PUSH; "^types^query^" POP;' | "^Flag.cvc3^"); test $RESULT = \"Satisfiable.\"" in
    Sys.command command = 0
*)







let get_solution p t =
  (*
    let cin = !cvc3in in
    let cout = !cvc3out in
  *)
  (**)
  let cin,cout = Unix.open_process !Flag.cvc3 in
    (**)


  let fm = Format.formatter_of_out_channel cout in
  let fv = uniq' compare (get_fv2 p) in

  let types = List.fold_left (fun str x -> str ^ string_of_ident x ^ ":" ^ string_of_typ (Id.typ x) ^ "; ") "" fv in
  let query = "CHECKSAT " ^ string_of_term p ^ "; COUNTERMODEL;" in

  let q = types ^ query in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.printf "get_solution: %s@." q in

  let () = Format.fprintf fm "%s@." q in

    (**)
  let () = close_out cout in
    (**)
  let rec aux cin =
    try
      let s = input_line cin in
        if Str.string_match (Str.regexp ".*ASSERT") s 0
        then
          let pos_begin = String.index s '(' + 1 in
          let pos_end = String.index s ')' in
          let s' = String.sub s pos_begin (pos_end - pos_begin) in
            s' :: aux cin
        else aux cin
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
  let aux s =
    not (Str.string_match (Str.regexp "cvc3") s 0) &&
    not (Str.string_match (Str.regexp "_reach_") s 0)
  in
    List.filter aux ts


let isTInt = function
    TInt _ -> true
  | _ -> false


let equiv cond t1 t2 =
  check (t1::cond) t2 && check (t2::cond) t1



let rec gcd_arith_exp prev curr =
  if List.sort compare prev = List.sort compare curr
  then
    let aux t =
      match t.desc with
          Int n -> abs n
        | BinOp(Mult, {desc=Int n}, t)
        | BinOp(Mult, t, {desc=Int n}) -> abs n
        | _ -> 1
    in
    List.fold_left gcd 0 (List.rev_map aux curr)
  else
    let aux t =
      match t with
          {desc=BinOp((Add|Sub), t1, t2)} -> [t1; t2]
        | _ -> [t]
    in
    gcd_arith_exp curr (rev_map_flatten aux curr)
let gcd_arith_exp ts = gcd_arith_exp [] ts


let rec div_arith_exp n t =
  let desc =
    match t.desc with
        Int m -> assert (m mod n = 0); Int (m/n)
      | BinOp(Add, t1, t2) ->
        let t1' = div_arith_exp n t1 in
        let t2' = div_arith_exp n t2 in
        BinOp(Add, t1', t2')
      | BinOp(Mult, {desc=Int m}, t)
      | BinOp(Mult, t, {desc=Int m}) ->
        assert (m mod n = 0);
        BinOp(Mult, {desc=Int(m/n);typ=TInt}, t)
      | _ -> Format.printf "@.%a@." pp_print_term t; assert false
  in
  {desc=desc; typ=TInt}
let div_arith_exp n t =
  if n = 1
  then t
  else div_arith_exp n t


let rec simplify_bool_exp precise t =
  assert (t.typ = TBool);
  let desc =
    match t.desc with
        True
      | False
      | Var _ -> t.desc
      | BinOp(And|Or as op, t1, t2) ->
          if precise && equiv [] t t1 then (simplify_bool_exp precise t1).desc
          else if precise && equiv [] t t2 then (simplify_bool_exp precise t2).desc
          else BinOp(op, simplify_bool_exp precise t1, simplify_bool_exp precise t2)
      | BinOp(_, {desc=True|False}, _) -> t.desc
      | BinOp(_, _, {desc=True|False}) -> t.desc
      | BinOp(Eq, {desc=BinOp(op1, t11, t12)}, {desc=BinOp(op2, t21, t22)}) when op1 = op2 && t12 = t22 ->
          (simplify_bool_exp precise {desc=BinOp(Eq, t11, t21);typ=TBool}).desc
      | BinOp(Eq, {desc=Int 0}, {desc=BinOp(Mult, {desc=Int n}, t)})
      | BinOp(Eq, {desc=BinOp(Mult, {desc=Int n}, t)}, {desc=Int 0}) ->
          if n = 0
          then True
          else BinOp(Eq, simplify_exp t, {desc=Int 0;typ=TInt})
      | BinOp(Lt, {desc=Int 0}, {desc=BinOp(Mult, {desc=Int n}, t)})
      | BinOp(Gt, {desc=BinOp(Mult, {desc=Int n}, t)}, {desc=Int 0}) ->
          if n > 0 then
            BinOp(Lt, {desc=Int 0;typ=TInt}, simplify_exp t)
          else if n < 0 then
            BinOp(Lt, simplify_exp t, {desc=Int 0;typ=TInt})
          else
            False
      | BinOp(Lt, {desc=BinOp(Mult, {desc=Int n}, t)}, {desc=Int 0})
      | BinOp(Gt, {desc=Int 0}, {desc=BinOp(Mult, {desc=Int n}, t)}) ->
          if n > 0 then
            BinOp(Lt, simplify_exp t, {desc=Int 0;typ=TInt})
          else if n < 0 then
            BinOp(Lt, {desc=Int 0;typ=TInt}, simplify_exp t)
          else
            False
      | BinOp(Leq, {desc=Int 0}, {desc=BinOp(Mult, {desc=Int n}, t)})
      | BinOp(Geq, {desc=BinOp(Mult, {desc=Int n}, t)}, {desc=Int 0}) ->
          if n > 0 then
            BinOp(Leq, {desc=Int 0;typ=TInt}, simplify_exp t)
          else if n < 0 then
            BinOp(Leq, simplify_exp t, {desc=Int 0;typ=TInt})
          else
            True
      | BinOp(Leq, {desc=BinOp(Mult, {desc=Int n}, t)}, {desc=Int 0})
      | BinOp(Geq, {desc=Int 0}, {desc=BinOp(Mult, {desc=Int n}, t)}) ->
          if n > 0 then
            BinOp(Leq, simplify_exp t, {desc=Int 0;typ=TInt})
          else if n < 0 then
            BinOp(Leq, {desc=Int 0;typ=TInt}, simplify_exp t)
          else
            True
      | BinOp(Eq, {desc=Int n}, {desc=Int m}) ->
          if n = m then True else False
      | BinOp(Lt, {desc=Int n}, {desc=Int m}) ->
          if n < m then True else False
      | BinOp(Gt, {desc=Int n}, {desc=Int m}) ->
          if n > m then True else False
      | BinOp(Leq, {desc=Int n}, {desc=Int m}) ->
          if n <= m then True else False
      | BinOp(Geq, {desc=Int n}, {desc=Int m}) ->
          if n >= m then True else False
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) when (function TInt _|TRInt _->true | _->false) t1.typ ->
          (*
            let t1 = simplify_exp t1 in
            let t2 = simplify_exp t2 in
          *)
          let d = gcd_arith_exp [t1;t2] in
            if d = 0
            then BinOp(op, simplify_exp t1, simplify_exp t2)
            else
              let t1' = div_arith_exp d t1 in
              let t2' = div_arith_exp d t2 in
                BinOp(op, simplify_exp t1', simplify_exp t2')
      | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) -> BinOp(op, t1, t2)
      | Not {desc=True} -> False
      | Not {desc=False} -> True
      | Not{desc=BinOp(Lt, t1, t2)} ->
          BinOp(Geq, simplify_exp t1, simplify_exp t2)
      | Not{desc=BinOp(Gt, t1, t2)} ->
          BinOp(Leq, simplify_exp t1, simplify_exp t2)
      | Not{desc=BinOp(Leq, t1, t2)} ->
          BinOp(Gt, simplify_exp t1, simplify_exp t2)
      | Not{desc=BinOp(Geq, t1, t2)} ->
          BinOp(Lt, simplify_exp t1, simplify_exp t2)
      | Not t -> Not (simplify_bool_exp precise t)
      | _ -> Format.printf "@.%a@." pp_print_term t; assert false
  in
    {desc=desc; typ=TBool}
and simplify_exp t =
  let desc =
    match t.desc with
        Var _ -> t.desc
      | Int _ -> t.desc
      | NInt _ -> t.desc
      | RandInt t ->
          assert (not t);
          RandInt false
      | BinOp(Add, t1, t2) ->
          let t1' = simplify_exp t1 in
          let t2' = simplify_exp t2 in
            (match t1'.desc, t2'.desc with
                 Int(n), Int(m) -> Int(n+m)
               | Int(0), t | t, Int(0) -> t
               | _, _ -> BinOp(Add, t1', t2'))
      | BinOp(Sub, t1, t2) ->
          let t1' = simplify_exp t1 in
          let t2' = simplify_exp t2 in
            (match t1'.desc, t2'.desc with
                 Int(n), Int(m) -> Int(n-m)
               | Int(0), _ -> BinOp(Mult, {desc=Int(-1);typ=TInt}, t2')
               | t, Int(0) -> t
               | _, _ -> BinOp(Sub, t1', t2'))
      | BinOp(Mult, t1, t2) ->
          let t1' = simplify_exp t1 in
          let t2' = simplify_exp t2 in
            (match t1'.desc, t2'.desc with
                 Int(n), Int(m) -> Int(n * m)
               | Int(0), t | t, Int(0) -> Int(0)
               | Int(1), t | t, Int(1) -> t
               | _, _ -> BinOp(Mult, t1', t2'))
      | Nil
      | Cons _ -> t.desc
      | Bottom -> Bottom
      | _ -> (simplify_bool_exp true t).desc
  in
    {desc=desc; typ=t.typ}



(*
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
*)

exception Satisfiable

let interpolation ts1 ts2 =
(*
  let ts1 = List.map init_rand_int ts1 in
  let ts2 = List.map init_rand_int ts2 in
*)







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

  let fv =
    let aux acc = List.fold_left (fun acc t -> uniq (acc @@ get_fv2 t)) acc in
      aux (aux [] ts1) ts2
  in
  let env = List.combine fv fv in

  let pred = try
    CsisatInterpolate.interpolate_with_proof t1 t2
  with CsisatAst.SAT_FORMULA(pred) -> begin
    (*if Flag.debug then print_string ("satisfiable: \n" ^ (CsisatAstUtil.print_pred pred) ^ "\n");*)
    if not Flag.check_sat || checksat (List.fold_left make_and true_term (ts1@@ts2))
    then raise Satisfiable
    else
      let rec trans t =
        let desc =
          match t.desc with
              True
            | False
            | Var _ -> t.desc
            | BinOp(And|Or as op, t1, t2) ->
                let t1' = trans t1 in
                let t2' = trans t2 in
                  BinOp(op, t1', t2')
            | BinOp(_, {desc=True|False}, _) -> t.desc
            | BinOp(_, _, {desc=True|False}) -> t.desc
            | BinOp(Eq|Lt|Gt|Leq|Geq as op, t1, t2) ->
                BinOp(op, t1, t2)
            | Not{desc=BinOp(Eq, t1, t2)} ->
                let n1 = {desc=Int 1; typ=TInt} in
                let t1' = {desc=BinOp(Leq, t1, {desc=BinOp(Sub, t2, n1);typ=TInt}); typ=TBool} in
                let t2' = {desc=BinOp(Leq, {desc=BinOp(Add, t2, n1);typ=TInt}, t1); typ=TBool} in
                  BinOp(Or, t1', t2')
            | Not t -> Not (trans t)
            | _ -> assert false
        in
          {desc=desc; typ=t.typ}
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
  let () =
    if Flag.debug && Flag.print_interpolant then
      Format.printf "  interpolant: %s@." (CsisatAstUtil.print_pred pred')
  in
  let pred'' =
    match Trans.normalize_bool_exp (simplify_bool_exp false (from_pred env (CsisatAstUtil.dnf pred'))) with
        {desc=BinOp(Or, _, _)} as p ->
          let rec f = function
              {desc=BinOp(Or, t1, t2)} ->
                f t1 @ f t2
            | t -> [t]
          in
          let disj = f p in
            (try
               List.find (fun p -> check ts1 p) disj
             with Not_found -> p)
      | p -> p
  in
    (if Flag.debug then begin
       ()(*assert (check ts1 pred''' &&
           check [pred'''] (Not (List.fold_left (fun t p -> BinOp(And, t, p)) (List.hd ts2) (List.tl ts2))))*)
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


(*
let rec rename_type map = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt ps -> TInt ps
  | TVar {contents = None} -> TVar {contents = None}
  | TVar {contents = Some typ} ->
      let typ' = rename_type map typ in
        TVar {contents = Some typ'}
  | TFun(x,typ) ->
      let typ1' = rename_type map typ1 in
      let typ2' = rename_type map typ2 in
      let x' = try List.assoc x map with Not_found -> x in
      let x'' = {x' with typ = typ1'} in
        TFun((x'',typ1'),typ2')
  | TUnknown -> TUnknown
  | _ -> assert false
*)


let rec congruent cond typ1 typ2 =
  match typ1,typ2 with
      TUnit, TUnit -> true
    | TBool, TBool -> true
    | TAbsBool, TAbsBool -> true
    | TInt, TInt -> true
    | TPred(typ1,ps1), TPred(typ2,ps2) ->
        List.length ps1 = List.length ps2 &&
        List.for_all2 (equiv cond) ps1 ps2 &&
        congruent cond typ1 typ2
    | TFun(x1,typ1), TFun(x2,typ2) ->
        let cond' =
          match Id.typ x1 with
              TInt _ -> {desc=BinOp(Eq, {desc=Var x1;typ=Id.typ x1}, {desc=Var x2;typ=Id.typ x2});typ=TBool}::cond
            | _ -> cond
        in
          congruent cond (Id.typ x1) (Id.typ x2) && congruent cond' typ1 typ2
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












