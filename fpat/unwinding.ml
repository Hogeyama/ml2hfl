open Util
open Combinator

(** unwinding *)

(** k is a parameter of iteration.
    d is definition of predicates.
    b is a flag of over/under-approximation.
    p is proposition.
*)

let rec get_ufun_def hash k =
  Formula.fold_set
    (fun atm ->
     match Atom.term_of atm |> Term.fun_args with
     | Term.Const (Const.Leq _), [Term.Const (Const.Int 0); t2] ->
        begin 
          match Term.fun_args t2 with
          | Term.Const (Const.UFun (ty, x)), args
               when String.starts_with (Idnt.string_of x) "ufun_" ->
             begin
               let x =
                 Idnt.make
                   (String.sub (Idnt.string_of x) 5
                               (String.length (Idnt.string_of x) - 5))
               in
               let pva =
                 Pva.make
                   x
                   (List.map (flip Pair.make Type.mk_unknown) args)
               in
               try
                 let ans = Hashtbl.find hash pva in
                 Formula.mk_iff (Formula.of_atom atm) ans
                 :: if k = 0 then []
                    else get_ufun_def hash (k-1) ans
               with Not_found -> 
                 (*Format.printf "%a not found!!@." Pva.pr pva;*)
                 []
             end
          | _ -> []
        end
     | _ -> [])



let rec ufun_atom2 atm =
  CunAtom.fold_brel
    (object
        method fvar x args =
          let pva =
            args
            |> List.map (flip Pair.make Type.mk_unknown)
            |> Pva.make x
          in
          IntFormula.leq
            IntTerm.zero
            (CunTerm.mk_ufun
               (Idnt.make ("ufun_" ^ Idnt.string_of x),
                Type.mk_fun_args_ret
                  (List.map (const Type.mk_unknown) args)
                  Type.mk_int)
               args)
        method fbrel c t1 t2 =
          Formula.mk_brel c t1 t2
        method fdivides n t =
          IntFormula.divides n t
        method frecognizer ty id t =
          ADTFormula.mk_recognizer ty id t
        method fsmem ty e t =
          SetFormula.mk_mem ty e t 
        method fssubset ty t1 t2 =
          SetFormula.mk_subset ty t1 t2
        method fterm c ts = assert false
      end)
    atm
and ufun2 phi = Formula.map_atom ufun_atom2 phi



let rec ufun_atom hash hcs k atm =
  CunAtom.fold_brel
    (object
        method fvar x args =
          let pva =
            args
            |> List.map (flip Pair.make Type.mk_unknown)
            |> Pva.make x
          in
          let ans =
            IntFormula.leq
              IntTerm.zero
              (CunTerm.mk_ufun
                 (Idnt.make ("ufun_" ^ Idnt.string_of x),
                  Type.mk_fun_args_ret
                    (List.map (const Type.mk_unknown) args)
                    Type.mk_int)
                 args)
          in
          if k = 0 || Hashtbl.mem hash pva then
            ans
          else if k > 0 && (Hashtbl.mem hash pva |> not) then
            begin
              let phi =
                try
                  pva
                  |> HCCS.lookup_eq hcs
                  |> HCCS.formula_of_bodies_eq
                  |> Formula.remove_annot
                  |> ufun hash hcs (k-1)
                with Not_found ->
                  Format.printf "error: %a is not found@," Pva.pr pva;
                  assert false
              in
              (*Format.printf "adding %a = %a@." Pva.pr pva Formula.pr phi;*)
              Hashtbl.add hash pva phi;
              ans
            end
          else assert false
        method fbrel c t1 t2 =
          Formula.mk_brel c t1 t2
        method fdivides n t =
          IntFormula.divides n t
        method frecognizer ty id t =
          ADTFormula.mk_recognizer ty id t
        method fsmem ty e t =
          SetFormula.mk_mem ty e t 
        method fssubset ty t1 t2 =
          SetFormula.mk_subset ty t1 t2
        method fterm c ts = assert false
      end)
    atm
and ufun hash hcs k phi = Formula.map_atom (ufun_atom hash hcs k) phi

let rec unwind_atom h k d b atm =
  CunAtom.fold_brel
    (object
        method fvar x args =
          let org = (k,b,Term.mk_app (Term.mk_var x) args) in
          if k = 0 then
            let _ = Hashtbl.add h org b in b
          else if k > 0 && Hashtbl.mem h org  then
            Hashtbl.find h org 
          else if k > 0 && (Hashtbl.mem h org |> not) then
            begin
              let pva =
                args
                |> List.map (flip Pair.make Type.mk_unknown)
                |> Pva.make x
              in
              let ans =
                try
                  pva
                  |> HCCS.lookup_eq d
                  |> HCCS.formula_of_bodies_eq
                  |> unwind h (k-1) d b
                with Not_found ->
                  Format.printf "error: %a is not found@," Pva.pr pva;
                  assert false
              in
              Hashtbl.add h org ans;
              ans
            end
          else
            assert false
        method fbrel c t1 t2 =
          Formula.mk_brel c t1 t2
        method fdivides n t =
          IntFormula.divides n t
        method frecognizer ty id t =
          ADTFormula.mk_recognizer ty id t
        method fsmem ty e t =
          SetFormula.mk_mem ty e t 
        method fssubset ty t1 t2 =
          SetFormula.mk_subset ty t1 t2
        method fterm c ts = assert false
      end)
    atm
and unwind h k d b phi =
  Formula.fold
    (object
      method fatom atm = fun b ->
        unwind_atom h k d b atm
      method ftrue _ = fun b ->
        Formula.mk_true
      method ffalse _ = fun b ->
        Formula.mk_false      
      method fnot r = fun b ->
        Formula.bnot @@ r (Formula.bnot b)
      method fand r1 r2 = fun b ->
        Formula.band [r1 b; r2 b]
      method for_ r1 r2 =  fun b ->
        Formula.bor [r1 b; r2 b]
      method fimply r1 r2 = fun b ->
        Formula.imply (r1 (Formula.bnot b)) (r2 b)
      method fiff r1 r2 = fun b ->
        (*Formula.mk_iff [r1 b; r2 b]*)
        assert false
      method fforall xty r1 = fun b ->
        Formula.forall [xty] (r1 b)
      method fexists xty r1 = fun b ->
        Formula.exists [xty] (r1 b)
      method fbox idx r1 = fun b -> assert false
      method fdiamond idx r1 = fun b -> assert false
      method fmu x r1 = fun b -> assert false
      method fnu x r1 = fun b -> assert false
    end)
    phi
    b



let mk_complete_unint p d =
  List.map
    (fun x -> 
     let Some(hd) = HornClause.pv_of_head (HornClause.head_of x) in
     (hd, HornClause.bpvas_of x, HornClause.bphi_of x))
    (d:HornClause.t list)
  |> List.sort
  |> List.fold_left
       (fun w (x,y,z) ->
        match w with
        | [] -> [x, [Formula.band 
                       (z::List.map Pva.to_formula y)]]
        | (x',bd)::rest ->
           begin
             try
               let subst = PredVar.pat_match x' x in
               (x',
                (Formula.band 
                   (z::List.map Pva.to_formula y)
                 |> Formula.subst subst) :: bd)::rest
             with
             | _ ->
                (x, [Formula.band 
                       (z::List.map Pva.to_formula y)])::w
           end) []
  |> List.map
       (fun (hd,bd) -> 
        let bd' = Formula.bor bd in
        let vs = 
          Formula.fvs bd'
          |> List.unique
        in
        let pv = Formula.fpvs bd' in
        let bvs =
          List.fold_left List.remove_all vs pv
          |> List.map (fun x -> (x,Type.mk_unknown))
        in
        Formula.forall bvs @@ 
          Formula.mk_iff 
            ((Pva.of_pvar >> Pva.to_formula) hd)
            bd')
  |> Formula.band
  |> ufun2


let rec unwinding_rec ctenv h1 h2 p d n max =
  Logger.printf "%a unwinding@," Ordinal.pr (Ordinal.make n);
  if n > max then
    begin
      Logger.printf0 "recusion reached the limit";
      raise SMTProver.Unknown
    end
  else
    begin
      let un = ufun h2 d n p in
      try
        un :: get_ufun_def h2 n un
        |> Formula.band
        |> Logger.pprintf "unsatisfiability check1: %a@." Formula.pr
        |> SMTProver.solve_dyn ~tenv:ctenv;
        let f = Idnt.new_var () in
        let unp = unwind h1 n d (Formula.mk_var f []) p in
        try
          Formula.subst [f, Formula.term_of Formula.mk_true] unp
          |> Logger.pprintf "unsatisfiablity check2: %a@." Formula.pr
          |> SMTProver.solve_dyn ~tenv:ctenv;
          begin
            try
              let oap =
                Formula.subst [f, Formula.term_of Formula.mk_false] unp
              in
              let ans =
                oap
                |> Logger.pprintf "satisfiability check %a@." Formula.pr
                |> SMTProver.solve_dyn ~tenv:ctenv
              in
              let ce = 
                try
                  p
                  |> flip Formula.let_not id
                  |> flip Formula.let_imply (fun x y -> y)
                with
                  _ -> Formula.mk_false
              in
              let vc = 
                try p |> flip Formula.let_not id with
                  _ -> Formula.bnot p
              in
              Format.printf
                "Verification condition:@,  %a@,"
                Formula.pr vc;
              Format.printf "Counter example:@,";
              let fvs = Formula.fvs oap |> List.unique in
              let rec loop l1 l2 =
                let nvs = 
                  List.concat_map
                    (fun (x,t) -> if List.mem x l2 then Term.fvs t else [])
                    l1
                in
                let l2' = nvs@l2 |> List.unique in
                if List.length l2 = List.length l2'
                then l2'
                else loop l1 l2'
              in
              let vl = loop ans fvs in
              List.iter
                (fun (x,sol) -> 
                 if List.mem x vl then 
                   Format.printf
                     "  %a = %a@,"
                     Idnt.pr x
                     Term.pr sol)
                ans;
              Format.printf
                "Core:@,  %a@,"
                Formula.pr (Formula.subst ans ce);
              true
            with
            | SMTProver.Unsat ->
               unwinding_rec ctenv h1 h2 p d (n+1) max
            | SMTProver.Unknown ->
               Format.printf "unknown@,";
               assert false
            | e ->
               Format.printf "Z3 error@,";
               raise e
          end
        with
        | SMTProver.Unsat ->
           false
        | SMTProver.Unknown ->
           Format.printf "unknown@,";
           assert false
        | e ->
           Format.printf "Z3 error@,";
           raise e
      with
      | SMTProver.Unsat ->
         false
      | SMTProver.Unknown ->
         Format.printf "unknown@,";
         assert false
      | e ->
         Format.printf "Z3 error@,";
         raise e
    end

let is_sat ctenv hcs max phi = 
  let h1 = Hashtbl.create max in
  let h2 = Hashtbl.create max in
  unwinding_rec
    ctenv
    h1 h2
    (phi |> Formula.remove_annot |> TupFormula.elim_proj)
    (TupHCCSSolver.elim_proj hcs)
    1 max

let is_valid ctenv hcs max =
  Formula.bnot >> is_sat ctenv hcs max >> not

let ctenv0 = ref []

let solve ctenv hcs =
  let ctenv = ctenv @ !ctenv0 in
  let phi, hcs_def =
    hcs
    |> List.partition HornClause.is_root
    |> Pair.map
         (List.map HornClause.formula_of
          >> Formula.band)
         id
  in
  try
    if is_valid ctenv hcs_def 100 phi then
      begin
        Format.printf "the HCCS has a solution@,";
        []
      end
    else
      raise HCCSSolver.NoSolution
  with SMTProver.Unknown ->
    raise HCCSSolver.Unknown
let solve =
  Logger.log_block2
    "Unwinding.solve"
    solve

(*
let rec flattening a = 
  a |>
    CunAtom.fold_brel
      (object
          method fvar x args = 
            Term.mk_app (Term.mk_var x) args
            |> Formula.of_term
          method ftrue () = Formula.mk_true
          method ffalse () = Formula.mk_false
          method fbrel c t1 t2 =
            let is_eq = function | Const.Eq _ | Const.Neq _ -> true | _ -> false in
            let f,args = Term.fun_args t1 in
            match f with
            | Term.Const (Const.Con (t, s)) -> 
               let nvs = List.map (fun _ -> Term.new_var ()) args in
               let a1 = 
                 List.map2 (fun x y -> Formula.eq t x y |> Formula.atom_of) args nvs 
                 |> List.map flattening 
               in
               let a2 = 
                 Formula.eq t t2 (Term.mk_app f nvs) 
                 |> Formula.atom_of 
                 |> flattening 
               in
               Formula.band @@ a2::a1
            | Term.Const (Const.Tuple tl) 
                 when Term.is_var t2 && is_eq c && 
                        List.exists (fun x -> not(Term.is_var x)) args -> 
               let rec getnvt n = function
                 | [] -> []
                 | x::xl when Term.is_var x -> getnvt (n+1) xl
                 | x::xl -> (n,x)::getnvt (n+1) xl
               in
               let nvts = getnvt 0 args in
               let nvs = List.map (fun _ -> Idnt.new_var ()) nvts in
               let theta = List.combine nvs (snd (List.split nvts)) in
               let t1' = List.map (fun x -> Term.subst theta x) args in
               let a1 = 
                 List.map2 
                   (fun (n,x) y -> 
                    Formula.eq (List.nth tl n) x (Term.mk_var y) 
                    |> Formula.atom_of) nvts nvs 
                 |> List.map flattening 
               in
               let a2 = 
                 Formula.mk_brel c t2 (Term.mk_app f t1') 
                 |> Formula.atom_of 
                 |> flattening in
               Formula.band @@ a2::a1          
            | Term.Const (Const.Tuple tl) 
                 when Term.is_var t2 && is_eq c && 
                        List.for_all Term.is_var args-> 
               Formula.mk_brel c t1 t2
            | Term.Const (Const.Proj (tl, n)) -> 
               let nv = Term.new_var () in
               let a1 = 
                 Formula.eq (List.nth tl n) nv (List.hd args) 
                 |> Formula.atom_of 
                 |> flattening 
               in
               let a2 = 
                 Formula.mk_brel c t2 (Term.mk_app f [nv]) 
                 |> Formula.atom_of 
                 |> flattening in
               Formula.band [a1;a2]
            | _ when Term.is_var t1 && Term.is_var t2 ->
               Formula.mk_brel c t1 t2
            | _ when Term.is_var t1 && is_eq c -> 
               flattening @@ Formula.atom_of (Formula.mk_brel c t2 t1)
            | _ -> Formula.mk_brel c t1 t2
        end)*)



let test () =
  try
    SMTProver.solve_dyn
      (Formula.band
         [IntFormula.eq 
            (Term.mk_var (Idnt.make "x"))
            IntTerm.zero;
          Formula.forall
            [Idnt.make "y", Type.mk_int]
            (IntFormula.eq
               (Term.mk_var (Idnt.make "x"))
               (Term.mk_var (Idnt.make "y")))]);
    Format.printf "sat!@,"
  with
  | SMTProver.Unsat ->
     Format.printf "unsat!@,"
  | SMTProver.Unknown ->
     assert false
