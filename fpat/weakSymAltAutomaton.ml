open Util
open Combinator

(** Weak symmetric alternating automata
    (see e.g. [Kupferman et al. JACM'00] for detals) *)

type modality = Nil | Box of string | Diamond of string
type ('a, 'b) trans_formula =
  | Literal of 'b
  (*| Not of ('a, 'b) trans_formula*)
  | And of ('a, 'b) trans_formula * ('a, 'b) trans_formula
  | Or of ('a, 'b) trans_formula * ('a, 'b) trans_formula
  | Box of string * 'a
  | Diamond of string * 'a

  | State of 'a (* not used in transition formulas *)

type ('a, 'b) t = 
  'a list (* a finite set of states Q *)
  (** 'b (* a finite set of alphabets \Sigma *)*)
  * 'a (* an initial state q_0 *)
  * ('a * ('a, 'b) trans_formula) list (* the transition function \rho *)
  * 'a list (* the recurrence set W (for defining the acceptance condition) *)
  * 'a list list (* a partition of Q (for defining the weak structure) *)
  * ('a * 'a) list (* a partial order on Q (for defining the weak structure) *)

let rec tr_of phi =
  (*Format.printf "tr_of: %a@." Formula.pr phi;*)
  Formula.visit
    (object
      method fatom atm = Literal(Formula.of_atom atm)
      method ftrue () = assert false
      method ffalse () = assert false
      method fnot phi1 = Literal(Formula.bnot phi1)
      method fand phi1 phi2 = And(tr_of phi1, tr_of phi2)
      method for_ phi1 phi2 = Or(tr_of phi1, tr_of phi2)
      method fimply phi1 phi2 = assert false
      method fiff phi1 phi2 = assert false
      method fforall xty phi1 = assert false
      method fexists xty phi1 = assert false
      method fbox idx phi1 = Box(idx, phi1)
      method fdiamond idx phi1 = Diamond(idx, phi1)
      method fmu x phi1 =
        let phi = Formula.mu x phi1 in (* @todo *)
        let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
        tr_of unfolded
      method fnu x phi1 =
        let phi = Formula.nu x phi1 in (* @todo *)
        let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
        tr_of unfolded
    end)
    phi

(** this makes ranking functions simple? *)
let rec rotate ec1 ec2 =
  match ec2 with
  | [] -> List.rev ec1
  | ec :: ec2' ->
    if Formula.is_diamond ec
    then ec :: ec2' @ List.rev ec1
    else rotate (ec :: ec1) ec2'

(** @require [phi] is alternation-free and guarded *)
let of_modal_mu phi =
  let phi = phi |> Formula.elim_imply_iff |> FormulaSimplifier.simplify in
  (* phis is now in the negation normal form *)
  let pre_cls = ModalMu.closure phi |> List.unique in
  let cls = PartOrd.reflexive_transitive_closure_of pre_cls in
  let states = List.map fst pre_cls @ List.map snd pre_cls |> List.unique in
  let scc = Graph_.scc pre_cls in (* assume that pre_cls has no duplication *)
  let ecs =
    scc
    |> List.classify (fun (_, c1) (_, c2) -> c1 = c2)
    |> List.map @@ List.map @@ fst
  in
  let ecs =
    List.map
      (List.sort
         ~cmp:(fun phi1 phi2 ->
             (List.findi (fun _ -> (=) phi1) states |> fst) -
             (List.findi (fun _ -> (=) phi2) states |> fst)))
      ecs
  in
  let ecs =
    List.sort ~cmp:(fun ec1 ec2 ->
        if List.mem (List.hd ec1, List.hd ec2) cls then -1
        else if List.mem (List.hd ec2, List.hd ec1) cls then 1
        else 0)
      ecs
  in
  (*let ecs = List.map (rotate []) ecs in*)
  (*let ecs =
    let [ec], ecs = List.partition (fun ec -> List.mem phi ec) ecs in
    ec :: ecs (** @todo necessary to set [ec] the initial component? *)
    in*)
  let trs = List.map tr_of states in
  let rset = List.filter (List.exists Formula.is_nu) ecs |> List.concat
  in
  let ord =
    Vector.multiply
      (fun ec1 ec2 ->
         let v1 = List.assocF (List.hd ec1) scc in
         let v2 = List.assocF (List.hd ec2) scc in
         if List.mem (v1, v2) cls then Some(ec1, ec2) else None)
      ecs ecs
    |> List.filter_map id
  in
  (*Format.printf
    "closure: %a@."
    (List.pr (Pair.pr Formula.pr Formula.pr) ";@,") cls;*)
  Format.printf "WAA constructed from a given AFMC formula:@.";
  Format.printf "  states:@.    @[<v>%a@]@." (List.pr Formula.pr ",@,") states;
  Format.printf "  initial state:@.    @[<v>%a@]@." Formula.pr phi;
  Format.printf "  states of recurrence set:@.    @[<v>%a@]@."
    (List.pr Formula.pr ",@,") rset;
  Format.printf "  components:@.    @[<v>%a@]@."
    (List.pr (List.pr ~header:"{" ~footer:"}" Formula.pr ",@,") ";@,") ecs;
  (states, phi, trs, rset, ecs, ord)
(* @todo compute reachable states for minimizing the WAA *)

(*
let rec subforms = function
  | Literal(l) -> [Literal(l)]
  | And(tr1, tr2) -> And(tr1, tr2) :: subforms tr1 @ subforms tr2
  | Or(tr1, tr2) -> Or(tr1, tr2) :: subforms tr1 @ subforms tr2
  | Box(idx, q) -> [Box(idx, q)]
  | Diamond(idx, q) -> [Diamond(idx, q)]

  | State(q) -> [State(q)]
*)
(*List.map (fun q -> State(q)) states
  @ (List.concat_map subforms trs |> List.unique)*)

(** returns a weak recurrence game
    assumes that the waa is constructed by [of_modal_mu] *)
let product ((*states*)_, (*phi*)_, (*trs*)_, rset, ecs, ord) (tenv, fdefs) =
  (* remove mu- and nu- formulas from rset, ecs, and ord *)
  let rset = List.map Formula.unfold rset |> List.unique in
  let ecs = List.map (List.map Formula.unfold >> List.unique) ecs in
  let ord = List.map (fun (ec1, ec2) ->
      List.map Formula.unfold ec1 |> List.unique,
      List.map Formula.unfold ec2 |> List.unique) ord
  in
  (* *)
  (*let main_t = "main" in*)
  let angel = Idnt.make "angel" in
  let demon = Idnt.make "demon" in
  let call_angel e = Term.mk_app (Term.mk_var angel) [e] in
  let call_demon e = Term.mk_app (Term.mk_var demon) [e] in
  let init_st = Idnt.new_var () in
  let [st_typ] = Type.args_of (TypEnv.lookup tenv angel) in
  let st_typ' = Type.mk_tuple [Type.mk_int; st_typ] in
  let st01 = Term.mk_var init_st in
  let st0 = TupTerm.mk_proj [Type.mk_int; st_typ] 0 st01 in
  let st1 = TupTerm.mk_proj [Type.mk_int; st_typ] 1 st01 in
  let next i = TupTerm.make [Type.mk_int; st_typ] [IntTerm.make i; st1] in
  let next_st i st =
    TupTerm.make
      [Type.mk_int; st_typ]
      [IntTerm.make i; Term.mk_var (Idnt.make st)]
  in
  let win = next 0 in
  let lose = next 1 in
  (* transition relation and substitution for angel *)
  let ra, suba =
    let angel_fdef =
      List.find (fun fdef -> Idnt.make fdef.Fdef.name = angel) fdefs
    in
    Fdef.apply angel_fdef [st1],
    Fdef.get_sub angel_fdef [st1]
  in
  (* transition relation and substitution for demon *)
  let rd, subd =
    let demon_fdef =
      List.find (fun fdef -> Idnt.make fdef.Fdef.name = demon) fdefs
    in
    Fdef.apply demon_fdef [st1],
    Fdef.get_sub demon_fdef [st1]
  in
  assert (suba = subd);
  (* mapping states to integers
     0: winning, 1: losing, ... *)
  let mapping = List.flatten ecs |> List.mapi (fun i q -> q, i + 2) in
  let tenv' =
    (List.filter (fun (x, _) -> x <> angel && x <> demon) tenv)
    @ [(*Idnt.make main_t, Type.mk_fun [st_typ'; Type.mk_unit];*)
       angel, Type.mk_fun [st_typ'; Type.mk_unit];
       demon, Type.mk_fun [st_typ'; Type.mk_unit]]
  in
  let cs =
    List.map
      (fun ec ->
         let b = List.assocF (List.hd ec) mapping in
         let e = List.assocF (List.last ec) mapping in
         let q = Idnt.make "q" in
         let s = Idnt.make "s" in
         Idnt.new_var (),
         if b = e then
           let pred =
             Pred.make [q, Type.mk_int; s, st_typ]
               (IntFormula.eq (Term.mk_var q) (IntTerm.make b))
           in
           pred, pred
         else
           Pred.make [q, Type.mk_int; s, st_typ]
             (Formula.band [IntFormula.leq (IntTerm.make b) (Term.mk_var q);
                            IntFormula.leq (Term.mk_var q) (IntTerm.make e)]),
           Pred.make [q, Type.mk_int; s, st_typ]
             (IntFormula.eq (Term.mk_var q) (IntTerm.make e)))
      ecs
  in
  let mapping_ecs = List.map2 (fun ec (cid, _) -> ec, cid) ecs cs in
  let cwin =
    let q = Idnt.make "q" in
    let s = Idnt.make "s" in
    Idnt.make "winning",
    let pred =
      Pred.make [q, Type.mk_int; s, st_typ]
        (IntFormula.eq (Term.mk_var q) IntTerm.zero)
    in
    pred, pred
  in
  let close =
    let q = Idnt.make "q" in
    let s = Idnt.make "s" in
    Idnt.make "losing",
    let pred =
      Pred.make [q, Type.mk_int; s, st_typ]
        (IntFormula.eq (Term.mk_var q) IntTerm.one)
    in
    pred, pred
  in
  let cs' = cs @ [cwin; close] in
  let rset' =
    fst cwin ::
    List.map
      (fun phi ->
         try
           List.find_map
             (fun (ec, cid) -> if List.mem phi ec then Some(cid) else None)
             mapping_ecs
         with Not_found -> assert false)
      rset
    |> List.unique
  in
  let ord' =
    List.map (fun (cid, _) -> fst cwin, cid) (cwin :: cs)
    @ List.map (fun (cid, _) -> fst close, cid) (close :: cs)
    @ List.map
      (fun (ec1, ec2) -> List.assocF ec1 mapping_ecs,
                         List.assocF ec2 mapping_ecs)
      ord
  in
  let is_demon phi =
    let phi = phi |> Formula.unfold in
    Formula.is_and phi || Formula.is_box phi
  in
  let mapping_demon, mapping_angel = List.partition (fst >> is_demon) mapping in
  let angel_body =
    MLExp.mk_cases Type.mk_unit
      ([IntFormula.eq st0 IntTerm.zero |> Formula.term_of,
        call_angel st01;
        IntFormula.eq st0 IntTerm.one |> Formula.term_of,
        call_angel st01]
       @ List.map
         (fun (phi, _) ->
            IntFormula.eq st0 (IntTerm.make (List.assocF phi mapping))
            |> Formula.term_of,
            Formula.visit
              (object
                method fatom atm =
                  MLExp.mk_if Type.mk_unit
                    (atm |> Atom.subst suba |> Atom.term_of)
                    (call_angel win)
                    (call_angel lose)
                method ftrue () = assert false
                method ffalse () = assert false
                method fnot phi1 =
                  MLExp.mk_if Type.mk_unit
                    (Formula.bnot phi1 |> Formula.subst suba |> Formula.term_of)
                    (call_angel win)
                    (call_angel lose)
                method fand phi1 phi2 = assert false
                method for_ phi1 phi2 =
                  let next1 = List.assocF (phi1 |> Formula.unfold) mapping in
                  let next2 = List.assocF (phi2 |> Formula.unfold) mapping in
                  MLExp.mk_let Type.mk_unit (Idnt.make "$nd", Type.mk_int)
                    MLExp.mk_rand_int
                    (MLExp.mk_if Type.mk_unit
                       (IntFormula.geq (Term.var_of_string "$nd") IntTerm.zero
                        |> Formula.term_of)
                       (if is_demon phi1
                        then call_demon (next next1)
                        else call_angel (next next1))
                       (if is_demon phi2
                        then call_demon (next next2)
                        else call_angel (next next2)))
                method fimply phi1 phi2 = assert false
                method fiff phi1 phi2 = assert false
                method fforall xty phi1 = assert false
                method fexists xty phi1 = assert false
                method fbox idx phi1 = assert false
                method fdiamond idx phi1 =
                  let next = List.assocF (phi1 |> Formula.unfold) mapping in
                  MLExp.mk_let Type.mk_unit (Idnt.make "$std", st_typ)
                    (if Idnt.make idx = angel then ra else rd)
                    (if is_demon phi1
                     then call_demon (next_st next "$std")
                     else call_angel (next_st next "$std"))
                method fmu x phi1 = assert false(*
                  let phi = Formula.mu x phi1 in (* @todo *)
                  let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
                  let next = List.assocF unfolded mapping in
                  let o =
                    if is_demon unfolded
                    then Term.mk_var demon
                    else Term.mk_var angel
                  in
                  let a =
                    TupTerm.make
                      [Type.mk_int; st_typ]
                      [IntTerm.make next; st1]
                  in
                  Term.mk_app o [a]*)
                method fnu x phi1 = assert false(*
                  let phi = Formula.nu x phi1 in (* @todo *)
                  let unfolded = Formula.subst [x, phi |> Formula.term_of] phi1 in
                  let next = List.assocF unfolded mapping in
                  let o =
                    if is_demon unfolded
                    then Term.mk_var demon
                    else Term.mk_var angel
                  in
                  let a =
                    TupTerm.make
                      [Type.mk_int; st_typ]
                      [IntTerm.make next; st1]
                  in
                  Term.mk_app o [a]*)
              end)
              phi)
         mapping_angel
         (*@ List.map
           (fun (phi, _) ->
            IntFormula.eq st0 (IntTerm.make (List.assocF phi mapping))
            |> Formula.term_of,
            call_demon st01)
           mapping_demon*))
      (call_angel win)
  in
  let demon_body =
    MLExp.mk_cases Type.mk_unit
      (List.map
         (fun (phi, _) ->
            IntFormula.eq st0 (IntTerm.make (List.assocF phi mapping))
            |> Formula.term_of,
            Formula.visit
              (object
                method fatom atm = assert false
                method ftrue () = assert false
                method ffalse () = assert false
                method fnot phi1 = assert false
                method fand phi1 phi2 =
                  let next1 = List.assocF (phi1 |> Formula.unfold) mapping in
                  let next2 = List.assocF (phi2 |> Formula.unfold) mapping in
(*
                  MLExp.mk_let Type.mk_unit (Idnt.make "$nd", Type.mk_int)
                    MLExp.mk_rand_int*)
                  (MLExp.mk_if Type.mk_unit
                     (MLExp.mk_rand_bool(*IntFormula.geq
                                            (Term.mk_var (Idnt.make "$nd"))
                                            IntTerm.zero
                                          |> Formula.term_of*))
                     (if is_demon phi1
                      then call_demon (next next1)
                      else call_angel (next next1))
                     (if is_demon phi2
                      then call_demon (next next2)
                      else call_angel (next next2)))
                method for_ phi1 phi2 = assert false
                method fimply phi1 phi2 = assert false
                method fiff phi1 phi2 = assert false
                method fforall xty phi1 = assert false
                method fexists xty phi1 = assert false
                method fbox idx phi1 =
                  let next = List.assocF (phi1 |> Formula.unfold) mapping in
                  MLExp.mk_let Type.mk_unit (Idnt.make "$stb", st_typ)
                    (if Idnt.make idx = angel then ra else rd)
                    (if is_demon phi1
                     then call_demon (next_st next "$stb")
                     else call_angel (next_st next "$stb"))
                method fdiamond idx phi1 = assert false
                method fmu x phi1 = assert false
                method fnu x phi1 = assert false
              end)
              phi)
         mapping_demon
         (*@ List.map
           (fun (phi, _) ->
              IntFormula.eq st0 (IntTerm.make (List.assocF phi mapping))
              |> Formula.term_of,
              call_angel st01)
           mapping_angel*))
      (call_angel win)
  in
  let fdefs' =
    (*let fdef_main =
      Fdef.make
        main_t [Pattern.mk_var init_st]
        Formula.mk_true
        (if mapping_demon = [] then
           call_angel st01
         else
           MLExp.mk_if Type.mk_unit
             (List.map
                (fun (phi, _) ->
                   IntFormula.eq
                     st0
                     (IntTerm.make (List.assocF phi mapping)))
                mapping_demon
              |> Formula.bor
              |> Formula.term_of)
             (call_demon st01)
             (call_angel st01))
    in*)
    let fdef_angel =
      Fdef.make
        (Idnt.string_of angel) [Pattern.mk_var init_st]
        Formula.mk_true
        angel_body
    in
    let fdef_demon =
      Fdef.make
        (Idnt.string_of demon) [Pattern.mk_var init_st]
        Formula.mk_true
        demon_body
    in
    List.filter
      (fun fdef -> let x = Idnt.make fdef.Fdef.name in x <> angel && x <> demon)
      fdefs
    @ [(*fdef_main;*) fdef_angel; fdef_demon]
  in
  let pr_aux ppf (x1, x2) =
    Format.fprintf ppf "(%a: %a)" Integer.pr x1 Formula.pr x2
  in
  Format.printf_force
    "mapping between WAA states and integers:@.  @[<v>%a@]@."
    (List.pr pr_aux ",@,")
    ([0, Formula.mk_var (Idnt.make "winning") [];
      1, Formula.mk_var (Idnt.make "losing") []] @ List.map Pair.flip mapping);
  Format.printf_force
    "product program:@.  %a@."
    Prog.pr (Prog.make fdefs' tenv' "");
  fdefs', tenv', Game.WeakRecurrence(true, cs', rset', ord', true)
