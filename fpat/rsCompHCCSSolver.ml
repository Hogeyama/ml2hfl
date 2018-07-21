open Util
open Combinator
open Rsdefs

(** A complete HCCS solver based on relaxed stratification *)

let formals_of pv =
  pv
  |> PredVar.args_of
  |> List.map (fun (x, _) -> { var_name = Idnt.serialize x })

let of_pvar pv =
  { pv_name = Idnt.serialize (PredVar.idnt_of pv);
    pv_formals = formals_of pv;
    pv_ufs = [] }

let pvar_of pv =
  PredVar.make
    (pv.pv_name |> Idnt.deserialize)
    (List.map
       (fun x ->
          x.var_name |> Idnt.deserialize,
          Type.mk_int)
       pv.pv_formals)

let of_term =
  CunTerm.fold
    (object
      method fvar x [] = VAR({ var_name = Idnt.serialize x })
      method funit () = assert false
      method ftrue () = assert false
      method ffalse () = assert false
      method fint n = NUM(n, 1)
      method fbigint n =
        try
          NUM(Big_int_Z.int_of_big_int n, 1)
        with Failure("int_of_big_int") ->
          assert false
      method frational _ _ = assert false
      method freal _ = assert false
      method fstring _ = assert false
      method fneg _ r = LINTIMES((-1, 1), r)
      method fadd _ r1 r2 = PLUS([r1; r2])
      method fsub _ r1 r2 = PLUS([r1; LINTIMES((-1, 1), r2)])
      method fmul _ r1 r2 =
        match r1, r2 with
        | NUM(n1, n2), r
        | r, NUM(n1, n2) ->
          LINTIMES((n1, n2), r)
        | _, _ ->
          NONLINTIMES(r1, r2)
      method fdiv _ r1 r2 = assert false
      method fmax _ r1 r2 = assert false
      method fmin _ r1 r2 = assert false
      method fmod r1 r2 = assert false
      method ftuple _ _ = assert false
      method fproj _ _ _ = assert false
      method fkon _ _ _ = assert false
      method faccessor _ _ _ _ = assert false
      method fufun _ _ _ = assert false
      method fformula _ = assert false
    end)

let of_atom =
  CunAtom.fold
    (object
      method fvar x _ =
        Logger.debug_assert_false ()
      method feq ty t1 t2 =
        EQUAL(of_term t1, of_term t2)
      method fneq ty t1 t2 =
        NOT(EQUAL(of_term t1, of_term t2))
      method flt ty t1 t2 =
        LT(of_term t1, of_term t2)
      method fgt ty t1 t2 =
        LT(of_term t2, of_term t1)
      method fleq ty t1 t2 =
        LEQ(of_term t1, of_term t2)
      method fgeq ty t1 t2 =
        LEQ(of_term t2, of_term t1)
      method fdiv t1 t2 =
        Logger.debug_assert_false ()
      method frecognizer ty x t1 =
        Logger.debug_assert_false ()
    end)

let of_formula =
  Formula.fold
    (object
      method fatom t = t |> of_atom
      method ftrue () = TRUE
      method ffalse () = FALSE
      method fnot s = NOT s
      method fand s1 s2 = AND [s1; s2]
      method for_ s1 s2 = OR [s1; s2]
      method fimply s1 s2 =
        Logger.debug_assert_false ()
      method fiff s1 s2 =
        Logger.debug_assert_false ()
      method fforall _ _ =
        Logger.debug_assert_false ()
      method fexists _ _ =
        Logger.debug_assert_false ()
    end)

let of_pva pva =
  let pv, phi = Pva.pvar_of pva in
  PVAR(pv |> PredVar.normalize_args |> of_pvar, formals_of pv) ::
  [of_formula phi]

let of_hc hc =
  HornClause.fold
    (fun pvas phi ->
       AND(of_formula phi :: List.concat_map of_pva pvas),
       FALSE)
    (fun pv pvas phi ->
       let [f1; f2] = of_pva (Pva.of_pvar pv) in
       AND(of_formula phi :: f2 :: List.concat_map of_pva pvas),
       f1)
    hc

let of_hcs hcs =
  hcs
  |> List.map of_hc
  |> List.fold_left (fun t e -> HconstS.add e t) HconstS.empty

let rec term_of = function
  | NUM(n, 1) ->
    IntTerm.make n
  | VAR(x) ->
    x.var_name |> Idnt.deserialize |> Term.mk_var
  | PLUS(ts) ->
    IntTerm.sum (List.map term_of ts)
  | LINTIMES((n, 1), t) ->
    IntTerm.mul (IntTerm.make n) (term_of t)
  | APP(_, _) -> assert false
  | NONLINTIMES(t1, t2) ->
    IntTerm.mul (term_of t1) (term_of t2)
  | COEFF(_) -> assert false

let rec formula_of = function
  | EQUAL(t1, t2) ->
    IntFormula.eq (term_of t1) (term_of t2)
  | LEQ(t1, t2) ->
    IntFormula.leq (term_of t1) (term_of t2)
  | LT(t1, t2) ->
    IntFormula.lt (term_of t1) (term_of t2)
  | AND(fs) ->
    Formula.band (List.map formula_of fs)
  | OR(fs) ->
    Formula.bor (List.map formula_of fs)
  | NOT(f) ->
    Formula.bnot (formula_of f)
  | PVAR(pv, vs) ->
    (* not reachable here *)
    Pva.to_formula
      (Pva.make
         (Idnt.make pv.pv_name)
         (List.map
            (fun v ->
               Term.var_of_string v.var_name,
               Type.mk_int)
            vs))
  | TRUE ->
    Formula.mk_true
  | FALSE ->
    Formula.mk_false

let sol_of =
  function
  | None ->
    raise HCCSSolver.NoSolution
  | Some(sol) ->
    sol
    |> PVM.bindings
    |> List.map
      (fun (x, y) ->
         PredSubst.elem_of_pvar (pvar_of x) (formula_of y))

let pginfo_of_hcs hcs =
  hcs
  |> HCCS.tenv
  |> List.map
    (fun (p, ty) ->
       let pv = PredVar.of_type p ty |> PredVar.normalize_args in
       of_pvar pv,
       of_pvar (PredVar.reset_uid pv))
  |> List.fold_left
    (fun t (e1, e2) -> PVM.add e1 e2 t)
    PVM.empty

let solve ?(pred_lang = None) cut_len hcs =
  let s = of_hcs hcs in
  let pginfo = pginfo_of_hcs hcs in
  let pl =
    match pred_lang with
    | None -> (fst !Rsrefine.curpredlang, cut_len)
    | Some(pl) -> (fst pl, cut_len)
  in
  Rsrefine.curpredlang := pl;
  Logger.log
    (fun () ->
       Format.printf "pred lang before:@,";
       !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  Rsdagsolver.solver_type := 0;
  Logger.printf "begin default solver with cut_len = %a@," Integer.pr cut_len;
  let sol = Rsrefine.solve s pginfo |> sol_of in
  Logger.printf "end default solver with cut_len = %a@,@," Integer.pr cut_len;
  Logger.log
    (fun () ->
       Format.printf "pred lang after:@,";
       if !Rsrefine.curpredlang = pl then
         Format.printf "  not changed@,"
       else
         !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  sol

let solve_itm ?(pred_lang = None) hcs=
  let s = of_hcs hcs in
  let pginfo = pginfo_of_hcs hcs in
  let pl =
    match pred_lang with
    | None -> !Rsrefine.curpredlang
    | Some(pl) -> pl
  in
  Rsrefine.curpredlang := pl;
  Logger.log
    (fun () ->
       Format.printf "pred lang before:@,";
       !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  Rsdagsolver.solver_type := 1;
  Logger.printf0 "begin unrestricted solver@,";
  let sol = Rsrefine.solve s pginfo |> sol_of in
  Logger.printf0 "end unrestricted solver@,@,";
  Logger.log
    (fun () ->
       Format.printf "pred lang after:@,";
       if !Rsrefine.curpredlang = pl then
         Format.printf "  not changed@,"
       else
         !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  sol

let solve_exl ?(pred_lang = None) hcs=
  let s = of_hcs hcs in
  let pginfo = pginfo_of_hcs hcs in
  let pl =
    match pred_lang with
    | None -> !Rsrefine.curpredlang
    | Some(pl) -> pl
  in
  Rsrefine.curpredlang := pl;
  Logger.log
    (fun () ->
       Format.printf "pred lang before:@,";
       !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  Rsdagsolver.solver_type := 2;
  Logger.printf0 "begin exact solver@,";
  let sol = Rsrefine.solve s pginfo |> sol_of in
  Logger.printf0 "end exact solver@,@,";
  Logger.log
    (fun () ->
       Format.printf "pred lang after:@,";
       if !Rsrefine.curpredlang = pl then
         Format.printf "  not changed@,"
       else
         !Rsrefine.curpredlang |> Rsrefine.print_attribs);
  sol

let solve_rse hcs =
  HCCS.save_smtlib
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ ".smt")
    hcs;
  HCCS.save_graphviz
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ ".dot")
    hcs;
  let hcs_expanded = HCCS.expand_dag hcs |> fst in
  HCCS.save_smtlib
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ "_expanded.smt")
    hcs_expanded;
  HCCS.save_graphviz
    (Filename.chop_extension !Global.target_filename
     ^ "_hccs" ^ string_of_int !Global.cegar_iterations ^ "_expanded.dot")
    hcs_expanded;
  Format.printf
    "@[<v># of constrs.: %a@,# of pred. vars.: %a@,@]"
    Integer.pr (List.length hcs_expanded)
    Integer.pr (HCCS.num_pvs hcs_expanded);

  let old_pl = !Rsrefine.curpredlang in
  let tm_rs4, sol =
    Timer.block
      (fun _ -> ())
      (fun _ -> ())
      (fun () -> solve 4 hcs)
  in
  let new_pl = !Rsrefine.curpredlang in
  let tm_itm, _ =
    try
      Timer.block
        ~timeout:100
        (fun _ -> ())
        (fun _ -> ())
        (fun () ->
           try
             solve_itm ~pred_lang:(Some old_pl) hcs
           with HCCSSolver.NoSolution ->
             [])
    with
    | Timer.Timeout -> 100.0, []
    | _ -> 100.0, [](*assert false*)
  in
  let tm_rs1, _ =
    try
      Timer.block
        ~timeout:100
        (fun _ -> ())
        (fun _ -> ())
        (fun () ->
           try
             solve ~pred_lang:(Some old_pl) 1 hcs
           with HCCSSolver.NoSolution ->
             [])
    with
    | Timer.Timeout -> 100.0, []
    | _ -> 100.0, [](*assert false*)
  in
  let tm_exl, _ =
    try
      Timer.block
        ~timeout:100
        (fun _ -> ())
        (fun _ -> ())
        (fun () ->
           try
             solve_exl ~pred_lang:(Some old_pl) hcs
           with HCCSSolver.NoSolution ->
             [])
    with
    | Timer.Timeout -> 100.0, []
    | _ -> 100.0, [](*assert false*)
  in
  Rsrefine.curpredlang := new_pl;

  let name =
    Filename.chop_extension (Filename.basename !Global.target_filename)
    ^ "_" ^ string_of_int !Global.cegar_iterations
  in
  let oc = open_out_gen [Open_append; Open_creat] 0o666 "exp_rscomp.csv" in 
  let ocf =
    Format.make_formatter
      (output oc)
      (fun () -> flush oc)
  in
  Format.fprintf ocf "@[<v>";
  Format.fprintf
    ocf
    "%s,%f,%f,%f,%f,%a@,"
    name tm_rs4 tm_itm tm_rs1 tm_exl
    Bool.pr_yn (hcs |> HCCS.conj_hccs_of |> HCCS.is_non_disjunctive |> not);
  Format.fprintf ocf "@]@?";
  close_out oc;

  sol

let init pvs =
  pvs
  |> List.map of_pvar
  |> List.fold_left (fun t e -> PVS.add e t) PVS.empty
  |> Option.return
  |> Rsrefine.init None

let solve cut_len =
  (HCCS.normalize2 >> HCCS.fresh >> HCCS.alpha >> solve cut_len)
  |> EncBoolHCCSSolver.solve false
let solve =
  Logger.log_block2
    "RsCompHCCSSolver.solve"
    ~after:
      (Logger.printf
         "solution:@,  %a@,"
         PredSubst.pr)
    solve

let solve_itm =
  (HCCS.normalize2 >> HCCS.fresh >> HCCS.alpha >> solve_itm)
  |> EncBoolHCCSSolver.solve false
let solve_itm =
  Logger.log_block1
    "RsCompHCCSSolver.solve_itm"
    ~after:
      (Logger.printf
         "solution:@,  %a@,"
         PredSubst.pr)
    solve_itm

let solve_exl =
  (HCCS.normalize2 >> HCCS.fresh >> HCCS.alpha >> solve_exl)
  |> EncBoolHCCSSolver.solve false
let solve_exl =
  Logger.log_block1
    "RsCompHCCSSolver.solve_exl"
    ~after:(Logger.printf "solution:@,  %a@," PredSubst.pr)
    solve_exl

let solve_rse =
  (HCCS.normalize2 >> HCCS.fresh >> HCCS.alpha >> solve_rse)
  |> EncBoolHCCSSolver.solve false
let solve_rse =
  Logger.log_block1
    "RsCompHCCSSolver.solve_rse"
    ~after:(Logger.printf "solution:@,  %a@," PredSubst.pr)
    solve_rse

let _ =
  HCCSSolver.ext_init_rsrefine := init;
  HCCSSolver.ext_solve_rsrefine_rs1 := solve 1;
  HCCSSolver.ext_solve_rsrefine_rs4 := solve 4;
  HCCSSolver.ext_solve_rsrefine_rs8 := solve 8;
  HCCSSolver.ext_solve_rsrefine_itm := solve_itm;
  HCCSSolver.ext_solve_rsrefine_exl := solve_exl;
  HCCSSolver.ext_solve_rsrefine_rse := solve_rse
