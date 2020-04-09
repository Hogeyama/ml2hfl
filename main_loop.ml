open Util
open Mochi_util
open Main_loop_util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let cegar_of_preprocessed ?fun_list spec results =
  let set_main = Option.map fst @@ List.assoc_option Preprocess.Set_main results in
  let main = Option.(set_main >>= return-|Problem.term >>= Trans.get_set_main)  in
  let problem = Preprocess.last_problem results in
  let fun_list' =
    match fun_list with
    | None ->
        begin
          try
            let t = Problem.term Preprocess.(take_result Decomp_pair_eq results) in
            Term_util.get_top_funs t
          with Not_found -> []
        end
    | Some fun_list' -> fun_list'
  in

  let prog,map,_,make_get_rtyp_trans = CEGAR_trans.trans_prog problem in
  let abst_cegar_env =
    Spec.get_abst_cegar_env spec prog
    |@> Verbose.printf "%a@." Spec.print_abst_cegar_env
  in
  let prog = CEGAR_trans.add_env abst_cegar_env prog in
  let make_get_rtyp =
    if !!Debug.check then
      let aux f (label,(_,g)) map x =
        Format.printf "BEGIN[%s]@." @@ Preprocess.string_of_label label;
        let r =
          try
            g (f map) x
          with e ->
            Format.printf "GET_RTYP ERROR[%s]: %s@." (Preprocess.string_of_label label) (Printexc.to_string e);
            assert false
        in
        Format.printf "END %s@." @@ Preprocess.string_of_label label;
        r
      in
      List.fold_left aux make_get_rtyp_trans results
    else
      List.fold_left (fun f (_,(_,g)) -> g -| f) make_get_rtyp_trans results
  in

  let info =
    let orig_fun_list =
      let aux x = List.assoc_option (CEGAR_trans.trans_var x) map in
      List.filter_map aux fun_list'
    in
    let inlined = List.map CEGAR_trans.trans_var spec.Spec.inlined in
    let fairness =
      if Flag.Method.(!mode = FairNonTermination) then
        Some spec.Spec.fairness
      else
        None
    in
    CEGAR_syntax.{prog.info with orig_fun_list; inlined; fairness}
  in
  CEGAR_syntax.{prog with info}, make_get_rtyp, set_main, main, problem.Problem.info


let run_preprocess ?make_pps spec problem =
  let pps' =
    match make_pps with
    | None -> Preprocess.all spec
    | Some make_pps' -> make_pps' spec
  in
  Preprocess.run_problem pps' problem

let check ?fun_list ?(exparam_sol=[]) spec pp =
    (* TODO 他の返り値は何 *)
    let preprocessed, make_get_rtyp, set_main, main, info = cegar_of_preprocessed ?fun_list spec pp in
    let cegar_prog = preprocessed in
    cegar_prog

let rec run ?make_pps ?fun_list ?exparam_sol spec problem =
  let preprocessed = run_preprocess ?make_pps spec problem in
  let is_singleton = Exception.not_raise Preprocess.get preprocessed in
  if not is_singleton then unsupported "multiple goal";
  let cegar_prog =
    let rec aux label acc r =
        match r with
        | Preprocess.Before p ->
            if not is_singleton then Verbose.printf "Start checking sub-problem.@.";
            check ?fun_list ?exparam_sol spec ((label,p)::acc)
        | Preprocess.After {Preprocess.label; problem; op; result=[r]} ->
            let acc' = (label,problem)::acc in
            aux label acc' r
        | _ -> assert false
    in aux Preprocess.Init [] preprocessed
  in
  let info =
    let open CEGAR_syntax in
    let non_rec =
      if !Flag.PredAbst.expand_non_rec then
        CEGAR_util.get_non_rec CEGAR_abst_CPS.beta_reduce_term @@
          snd @@ CEGAR_abst_util.add_label cegar_prog
      else
        cegar_prog.info.non_rec
    in { cegar_prog.info with non_rec }
  in
  let cegar_prog = { cegar_prog with info } in
  let cegar_prog = CEGAR_abst_CPS.expand_non_rec cegar_prog in
  let hes = HFLz.of_cegar cegar_prog in
  Format.printf "%a" HFLz.Print.hes hes

(* TODO CEGAR_syntax.prog -> HFL -> string *)
