open Util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

module F = Fpat

exception TimeOut

let rec fix_id_T x =
  match x with
  | F.Idnt.T(y, _, n) -> F.Idnt.T(fix_id_T y, 0, n)
  | _ -> x

let solve_file filename =
  let cmd =
    let open Flag.Refine in
    match !solver with
    | Hoice -> !hoice
    | Z3 -> !z3
    | Z3_spacer -> !z3_spacer
  in
  let sol = Filename.change_extension filename "sol" in
  let cmd' = Format.sprintf "(ulimit -t %d; %s %s > %s)" !Flag.Refine.solver_timelimit cmd filename sol in
  let r = Sys.command cmd' in
  if r = 128+9 then raise TimeOut;
  let s = IO.input_file sol in
  if r <> 0 || s = "" then fatal "solver aborted";
  Smtlib2_interface.parse_sexp s
  |@> Debug.printf "PARSED: %a@." (List.print Sexp.print)

let print_sol = Print.(list (pair string (pair (list (pair string CEGAR_print.typ)) CEGAR_print.term)))

let preprocess_rec_hccs hcs =
  let map =
    let rename x =
      x
      |> fix_id_T
      |> Format.asprintf "|%a|" F.Idnt.pr
      |> F.Idnt.make
    in
    F.HCCS.tenv hcs
    |> List.map fst
    |> List.map (Pair.add_right rename)
  in
  let rev_map = List.map (fun (x,y) -> F.Idnt.string_of y, x) map in
  let filename = Filename.change_extension !!Flag.mainfile "smt2" in
  hcs
  |> F.HCCS.rename map
  |@> Debug.printf "HCCS: %a@." F.HCCS.pr
  |> F.HCCS.save_smtlib2 filename;
  rev_map, filename

let unfold sol =
  let sol' = Hashtbl.create @@ List.length sol in
  List.iter (Fun.uncurry @@ Hashtbl.add sol') sol;
  let rec aux t =
    match CEGAR_syntax.decomp_app t with
    | _, [] -> t
    | CEGAR_syntax.Var ("exists"|"forall" as s), [args; t] -> CEGAR_syntax.make_app (CEGAR_syntax.Var s) [args; aux t]
    | CEGAR_syntax.Var f, ts ->
        let args,t' = Hashtbl.find sol' f in
        let xs = List.map fst args in
        let ts' = List.map aux ts in
        let t'' = update f args t' in
        CEGAR_util.subst_map (List.combine xs ts') t''
    | t1, ts -> CEGAR_syntax.make_app t1 (List.map aux ts)
  and update f args t =
    let t' = aux t in
    Hashtbl.replace sol' f (args,t');
    t'
  in
  Hashtbl.iter (fun f (args,t) -> ignore @@ update f args t) sol';
  Hashtbl.fold (fun f (xs,t) acc -> (f,(xs,t))::acc) sol' []

let solve hcs =
  let to_pred (xs,t) =
    List.map (Pair.map F.Idnt.make FpatInterface.conv_typ) xs, FpatInterface.conv_formula t
  in
  let rev_map,filename = preprocess_rec_hccs hcs in
  solve_file filename
  |> Smtlib2_interface.parse_model
  |@> Debug.printf "Sol: %a@." print_sol
  |> unfold
  |@> Debug.printf "Unfold: %a@." print_sol
  |> List.map (Pair.map_snd @@ Pair.map_snd QE.eliminate)
  |> List.map (fun (f,def) -> List.assoc f rev_map, to_pred def)

let check_sat hcs =
  let _,filename = preprocess_rec_hccs hcs in
  match solve_file filename with
  | [Sexp.A "sat"; _] -> true
  | _ -> false
