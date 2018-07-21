open Util
open Combinator

(** Knuth-Bendix completion algorithm *)

let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let critical_pairs (id1, (t1, t2)) (id2, (s1, s2)) =
  let s1, s2 =
    let fvs1 = Term.fvs t1 @ Term.fvs t2 in
    let fvs2 = Term.fvs s1 @ Term.fvs s2 in
    if Set_.intersects fvs1 fvs2 then
      let xs = List.unique fvs2 in
      let sub = List.map (fun x -> x, Term.new_var ()) xs in
      Term.subst sub s1, Term.subst sub s2
    else s1, s2
  in
  Term.find_all_mapc
    (fun ctx s ->
     try
       let sub = Term.unify [t1, s] in
       Some
         ([id1; id2],
          (Term.subst sub s2,
           Term.subst sub (ctx t2)))
     with AbsTerm.CannotUnify -> None)
    s1
  @ Term.find_all_mapc
    (fun ctx t ->
       try
         let sub = Term.unify [s1, t] in
         Some
           ([id1; id2],
            (Term.subst sub t2,
             Term.subst sub (ctx s2)))
       with AbsTerm.CannotUnify -> None)
    t1
let critical_pairs =
  Logger.log_block2
    "KnuthBendix.critical_pairs"
    critical_pairs
    ~before:(fun (_, tt) (_, ss) ->
        Logger.printf "input1:@,  %a@," RewriteSystem.pr_elem tt;
        Logger.printf "input2:@,  %a@," RewriteSystem.pr_elem ss)
    ~after:(fun cps ->
        Logger.printf
          "output:@,  @[<v>%a@]"
          (List.pr (Printer.coerce snd (Pair.pr Term.pr Term.pr)) "@,") cps)


let add_rule t1 t2 es rs =
  let es', rs' =
    List.fold_left
      (fun (es, rs) (id, (s1, s2)) ->
         let s1' = RewriteSystem.normalize ((t1, t2) :: List.map snd rs) s1 in
         if s1 = s1' then
           let s2' = RewriteSystem.normalize ((t1, t2) :: List.map snd rs) s2 in
           es, (id, (s1, s2')) :: rs
         else
           ([], (s1', s2))
           :: List.filter (fun (ids, _) -> not (List.mem id ids)) es, rs)
      (es, [])
      rs
  in
  let id = gen_id () in
  Logger.printf "added rule:@,  %a@," RewriteSystem.pr_elem (t1, t2);
  let rs'' = (id, (t1, t2)) :: rs' in
  let cps = List.concat_map (critical_pairs (id, (t1, t2))) rs'' in
  let es'' = es' @ cps in
  es'', rs''
let add_rule = Logger.log_block4 "KnuthBendix.add_rule" add_rule

let kb es (>>) =
  let rec aux es rs (>>) =
    if es = [] then List.map snd rs
    else
      let es1, (_, (t1, t2)), es2 =
        try List.pick (fun _ -> true) es with Not_found -> assert false
      in
      Logger.printf "picked: %a@," EquationTheory.pr_elem (t1, t2);
      let t1' = RewriteSystem.normalize (List.map snd rs) t1 in
      let t2' = RewriteSystem.normalize (List.map snd rs) t2 in
      if t1' = t2' then
        begin
          Logger.printf2
            "normalized to:@,  @[<v>%a = %a@]@,"
            Term.pr t1'
            Term.pr t2';
          aux (es1 @ es2) rs (>>)
        end
      else if t1' >> t2' then
        begin
          Logger.printf2
            "normalized to:@,  @[<v>%a > %a@]@,"
            Term.pr t1'
            Term.pr t2';
          let es', rs' = add_rule t1' t2' (es1 @ es2) rs in
          aux es' rs' (>>)
        end
      else if t2' >> t1' then
        begin
          Logger.printf2
            "normalized to:@,  @[<v>%a < %a@]@,"
            Term.pr t1'
            Term.pr t2';
          let es', rs' = add_rule t2' t1' (es1 @ es2) rs in
          aux es' rs' (>>)
        end
      else assert false
  in
  aux (List.map (fun e -> [], e) es) [] (>>)
let kb =
  Logger.log_block2
    "KnuthBendix.kb"
    ~before:(fun es _ -> Logger.printf "input:@,  @[<v>%a@]@," EquationTheory.pr es)
    ~after:(Logger.printf "output:@,  @[<v>%a@]" RewriteSystem.pr)
    kb

(** @test simple *)
let test () =
  Format.printf "Testing Knuth-Bendix completion algorithm@,";
  ignore (uncurry2 kb (RewriteSystem.group_theory ()))
