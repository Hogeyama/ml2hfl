
open Utilities
open CEGAR_syntax
open CEGAR_type


let cvc3in = ref stdin
let cvc3out = ref stdout


let iff t1 t2 = CsisatAst.Or [CsisatAst.And[t1; t2]; CsisatAst.And[CsisatAst.Not t1; CsisatAst.Not t2]]

let rec to_exp = function
    Const True -> CsisatAst.Variable "tru"
  | Const False -> CsisatAst.Variable "fls"
  | Const (Int n) -> CsisatAst.Constant (float_of_int n)
  | Var x -> CsisatAst.Variable x
  | App(App(Const Add, t1), t2) -> CsisatAst.Sum [to_exp t1; to_exp t2]
  | App(App(Const Sub, t1), t2) -> CsisatAst.Sum [to_exp t1; CsisatAst.Coeff(-1., to_exp t2)]
  | App(App(Const Mul, Const (Int n)), t2) | App(App(Const Mul, t2), Const (Int n)) -> CsisatAst.Coeff(float_of_int n, to_exp t2)
  | App(App(Const Mul, t1), t2) ->
      Format.printf "Nonlinear expression not supported@.";
      assert false
  | _ -> assert false
let rec to_pred env = function
    Const True -> CsisatAst.True
  | Const False -> CsisatAst.False
  | Var x -> CsisatAst.Eq (to_exp (Var x), CsisatAst.Variable "tru")
  | App(App(Const EqInt, t1), t2) -> CsisatAst.Eq (to_exp t1, to_exp t2)
  | App(App(Const EqBool, t1), t2) -> iff (to_pred env t1) (to_pred env t2)
  | App(App(Const Lt, t1), t2) -> CsisatAst.Lt (to_exp t1, to_exp t2)
  | App(App(Const Gt, t1), t2) -> CsisatAst.Lt (to_exp t2, to_exp t1)
  | App(App(Const Leq, t1), t2) -> CsisatAst.Leq (to_exp t1, to_exp t2)
  | App(App(Const Geq, t1), t2) -> CsisatAst.Leq (to_exp t2, to_exp t1)
  | App(App(Const And, t1), t2) -> CsisatAst.And [to_pred env t2; to_pred env t1]
  | App(App(Const Or, t1), t2) -> CsisatAst.Or [to_pred env t2; to_pred env t1]
  | App(Const Not, t) -> CsisatAst.Not (to_pred env t)
  | _ -> assert false
let to_pred env t = CsisatAstUtil.integer_heuristic (to_pred env t)

let rec from_exp map = function
    CsisatAst.Constant x -> Const (Int (int_of_float x))
  | CsisatAst.Variable "tru" -> Const True
  | CsisatAst.Variable "fls" -> Const False
  | CsisatAst.Variable "nil" -> assert false
(*
  | CsisatAst.Variable x when x.[0] = '_' -> NInt (parse_ident (String.sub x 1 (String.length x - 1)))
*)
  | CsisatAst.Variable x -> Var x
  | CsisatAst.Sum[e1;e2] -> App(App(Const Add, from_exp map e1), from_exp map e2)
  | CsisatAst.Sum(e::es) ->
      let aux t e = App(App(Const Add, t), from_exp map e) in
        List.fold_left aux (from_exp map e) es
  | CsisatAst.Coeff(x,e) -> App(App(Const Mul, Const (Int(int_of_float x))), from_exp map e)
  | CsisatAst.Application _
  | CsisatAst.Sum _ -> assert false
let rec from_pred map = function
    CsisatAst.True -> Const True
  | CsisatAst.False -> Const False
  | CsisatAst.And(p::ps) -> List.fold_left (fun t p -> App(App(Const And, from_pred map p), t)) (from_pred map p) ps
  | CsisatAst.Or(p::ps) -> List.fold_left (fun t p -> App(App(Const Or, from_pred map p), t)) (from_pred map p) ps
  | CsisatAst.Not p -> App(Const Not, from_pred map p)
  | CsisatAst.Eq(e1, e2) -> App(App(Const EqInt, from_exp map e1), from_exp map e2)
  | CsisatAst.Lt(e1, e2) -> App(App(Const Lt, from_exp map e1), from_exp map e2)
  | CsisatAst.Leq(e1, e2) -> App(App(Const Leq, from_exp map e1), from_exp map e2)
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

let set_datatype_cvc3 ?(cout = !cvc3out) t = ()(*
  let id = ref 0 in
  let fm = Format.formatter_of_out_channel cout in
  let declss = get_decls t in
  let print_kind fm = function
      KVariant ctypss ->
        let rec aux fm = function
            [] -> ()
          | (c,typs)::ctypss ->
              let bar = if ctypss = [] then "" else " | " in
                if typs = []
                then Format.fprintf fm "%s%s%a" c bar aux ctypss
                else
                  let rec aux' fm = function
                      [] -> assert false
                    | [typ] -> Format.fprintf fm "sel%d:%s" !id (string_of_typ typ); incr id
                    | typ::typs -> Format.fprintf fm "%a,%a" aux' [typ] aux' typs
                  in
                    Format.fprintf fm "%s(%a)%s%a" c aux' typs bar aux ctypss
        in
          aux fm ctypss
    | KRecord _ -> ()
  in
  let print_decls decls =
    let rec aux fm = function
        [] -> ()
      | (x,(_,kind))::decls ->
          match kind with
              KVariant ctypss ->
                let punc = if decls=[] then "" else "," in
                  Format.fprintf fm "%s = %a%s %a"
                    x print_kind kind punc aux decls
            | KRecord _ -> ()
    in
      Format.fprintf fm "DATATYPE %a END;@?" aux decls
  in
  let declss' = List.filter (List.exists (function (_,(_,KVariant _)) -> true | (_,(_,KRecord _)) -> false)) declss in
    List.iter print_decls declss';
    output_string cout "DATATYPE List = nil | cons (cdr: List) END;"
*)


exception Satisfiable



let string_of_var env x =
  let post =
    match List.assoc x env with
        TBase(TUnit,_) -> "_u"
      | TBase(TInt,_) -> "_i"
      | TBase(TBool,_) -> "_b"
      | _ -> Format.printf "%s@." x; assert false
  in
    x ^ post

let string_of_typ env x =
  match List.assoc x env with
      TBase(TUnit,_) -> "INT"
    | TBase(TInt,_) -> "INT"
    | TBase(TBool,_) -> "BOOLEAN"
    | _ -> assert false


let rec string_of_term env = function
    Const True -> "TRUE"
  | Const False -> "FALSE"
  | Const (Int n) -> string_of_int n
  | Var x -> string_of_var env x
  | App(App(Const And, t1), t2) -> "(" ^ string_of_term env t1 ^ " AND " ^ string_of_term env t2 ^ ")"
  | App(App(Const Or, t1), t2) -> "(" ^ string_of_term env t1 ^ " OR " ^ string_of_term env t2 ^ ")"
  | App(Const Not, t) -> "(NOT " ^ string_of_term env t ^ ")"
  | App(App(Const Lt, t1), t2) -> "(" ^ string_of_term env t1 ^ " < " ^ string_of_term env t2 ^ ")"
  | App(App(Const Gt, t1), t2) -> "(" ^ string_of_term env t1 ^ " > " ^ string_of_term env t2 ^ ")"
  | App(App(Const Leq, t1), t2) -> "(" ^ string_of_term env t1 ^ " <= " ^ string_of_term env t2 ^ ")"
  | App(App(Const Geq, t1), t2) -> "(" ^ string_of_term env t1 ^ " >= " ^ string_of_term env t2 ^ ")"
  | App(App(Const EqUnit, t1), t2) -> "TRUE"
  | App(App(Const EqBool, t1), t2) -> "(" ^ string_of_term env t1 ^ "<=>" ^ string_of_term env t2 ^ ")"
  | App(App(Const EqInt, t1), t2) ->  "(" ^ string_of_term env t1 ^ "=" ^ string_of_term env t2 ^ ")"
  | App(App(Const Add, t1), t2) -> "(" ^ string_of_term env t1 ^ " + " ^ string_of_term env t2 ^ ")"
  | App(App(Const Sub, t1), t2) -> "(" ^ string_of_term env t1 ^ " - " ^ string_of_term env t2 ^ ")"
  | App(App(Const Mul, t1), t2) -> "(" ^ string_of_term env t1 ^ " * " ^ string_of_term env t2 ^ ")"
  | t -> Format.printf "string_of_term: %a@." CEGAR_print.term t; assert false

let rec init_rand_int = function
    Const c -> [], Const c
  | Var x -> [], Var x
  | App(Const RandInt, _) ->
      let x = new_id "n" in
        [x,TBase(TInt,fun _ -> [])], Var x
  | App(t1,t2) ->
      let env1,t1' = init_rand_int t1 in
      let env2,t2' = init_rand_int t2 in
        env1@@env2, App(t1,t2)
  | Fun _ -> assert false
  | Let _ -> assert false


let string_of_env env =
  let aux str (x,typ) =
    match typ with
        TBase _ -> str ^ string_of_var env x ^ ":" ^ string_of_typ env x ^ "; "
      | _ -> str
  in
    List.fold_left aux "" env

let check env pre p =
  let cin = !cvc3in in
  let cout = !cvc3out in
    (**)
  let fm = Format.formatter_of_out_channel cout in
    (**)
  let types = string_of_env env in
  let assertion = List.fold_left (fun str p -> str ^ "ASSERT " ^ (string_of_term env p) ^ "; ") "" pre in
  let query = "QUERY " ^ string_of_term env p ^ ";" in
  let q = "PUSH;"^types^assertion^query^"\nPOP;" in
  let _ = if Flag.debug && Flag.print_cvc3 then Format.fprintf Format.std_formatter "check: %s@." q in
  let _ = Format.fprintf fm "%s@?" q in
  let s = input_line cin in
  let r = Str.string_match (Str.regexp ".*Valid") s 0 in
    if not (r || Str.string_match (Str.regexp ".*Invalid") s 0)
    then (print_string s; assert false)
    else r



let checksat env p =
  let cin = !cvc3in in
  let cout = !cvc3out in
  let fm = Format.formatter_of_out_channel cout in

  let types = string_of_env env in
  let query = "CHECKSAT " ^ string_of_term env p ^ ";" in

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



let equiv env cond t1 t2 =
  check env (t1::cond) t2 && check env (t2::cond) t1



let interpolation env ts1 ts2 =
  let bool_theory =
    [CsisatAst.Not(CsisatAst.Eq(CsisatAst.Variable "tru", CsisatAst.Variable "fls"));
     CsisatAst.Eq(CsisatAst.Application("neg", [CsisatAst.Variable "tru"]), CsisatAst.Variable "fls");
     CsisatAst.Eq(CsisatAst.Application("neg", [CsisatAst.Variable "fls"]), CsisatAst.Variable "tru")]
  in
  let ts1' = List.map (to_pred env) ts1 in
  let ts2' = List.map (to_pred env) ts2 in
  let t1 = CsisatAstUtil.simplify (CsisatAst.And (bool_theory@@ts1')) in
  let t2 = CsisatAstUtil.simplify (CsisatAst.And ts2') in
  let () = if Flag.debug && Flag.print_interpolant then Format.printf "  t1: %s@." (CsisatAstUtil.print_pred t1) in
  let () = if Flag.debug && Flag.print_interpolant then Format.printf "  t2: %s@." (CsisatAstUtil.print_pred t2) in

  let pred = try
    CsisatInterpolate.interpolate_with_proof t1 t2
  with CsisatAst.SAT_FORMULA(pred) -> begin
    (*if Flag.debug then print_string ("satisfiable: \n" ^ (CsisatAstUtil.print_pred pred) ^ "\n");*)
    if not Flag.check_sat || checksat env (List.fold_left make_and (Const True) (ts1@@ts2))
    then raise Satisfiable
    else
      let ts1' = List.map (to_pred env) ts1 in
      let ts2' = List.map (to_pred env) ts2 in
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
    from_pred env (CsisatAstUtil.dnf pred')



let get_solution env p =
  (**)
  let cin,cout = Unix.open_process !Flag.cvc3 in
    (**)

(*
  let () = set_datatype_cvc3 ~cout:cout t in
*)

  let fm = Format.formatter_of_out_channel cout in

  let types = string_of_env env in
  let query = "CHECKSAT " ^ string_of_term env p ^ "; COUNTERMODEL;" in

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
  let ts' = List.sort compare (List.filter aux ts) in
  let aux t =
    let b = Str.string_match (Str.regexp "^.+ = \(-?[0-9]+\)$") t 0 in
    let n = String.sub t (Str.group_beginning 1) (Str.group_end 1 - Str.group_beginning 1) in
      assert b;
      int_of_string n
  in
    List.map aux ts'
