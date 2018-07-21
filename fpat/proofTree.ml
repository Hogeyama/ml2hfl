open Util
open Combinator

type h = Bot | P of Pva.t
type t =
    Node of
      (HCCS.t
       * (PredVar.t list * (PredVar.t * int) option * Formula.t * h) list
       * (Pva.t * int list) list * Formula.t * h)
      * t
  | Fold of (Pva.t * int list) list * t
  | Unfold of (Pva.t * int list)
              * (PredVar.t list * (PredVar.t * int) option * Formula.t * h)
              * t list
  | Apply_Bot of Formula.t * t
  | Apply_P of (Pva.t * int list) list * t
  | Valid_Bot of Formula.t
  | Valid_P of (Pva.t * int list) list * Formula.t * h
  | InValid of SMTProver.model

let mk_hP pva = P pva
let mk_hB = Bot

let rec is_valid = function
  | Node(_, t) -> is_valid t
  | Unfold(_, _, ts) -> List.for_all is_valid ts
  | Fold(_, t) -> is_valid t
  | Apply_Bot(_, t) -> is_valid t
  | Apply_P(_, t) -> is_valid t
  | Valid_Bot _ | Valid_P _ -> true
  | InValid _ -> false

(** printer *)
let print_pva_with_mark (pva, m) =
  Format.printf "(%a * " Pva.pr pva;
  Format.printf "{";
  List.iter (Format.printf "_a%a, " Integer.pr) m;
  Format.printf "})"

let print_pva_with_mark_tex (pva, m) =
  Format.printf "\\left(%a * " Pva.pr_tex pva;
  Format.printf "\\{";
  List.iter (Format.printf "\\_a%a, " Integer.pr) m;
  Format.printf "} \\right)"

let print_pv_with_mark (pv, m) =
  Format.printf "(%a * " PredVar.pr pv;
  Format.printf "{";
  List.iter (Format.printf "_a%a, " Integer.pr) m;
  Format.printf "})"

let rec print_la = function
  | [] -> Format.printf "@."
  | a::rest ->
    print_pva_with_mark a;
    Format.printf ";@.";
    print_la rest

let rec print_la_tex = function
  | [] -> Format.printf "\\\\@."
  | a::rest ->
    print_pva_with_mark_tex a;
    Format.printf ";\\\\@.";
    print_la_tex rest

let print_phi phi =
  Format.printf "%a@." Formula.pr phi

let print_phi_tex phi =
  Format.printf "%a\\\\@." Formula.pr_tex phi

let print_h = function
  | Bot -> Format.printf "false@."
  | P pva -> Format.printf "%a@." Pva.pr pva

let print_h_tex = function
  | Bot -> Format.printf "\\bot@."
  | P pva -> Format.printf "%a\\\\@." Pva.pr_tex pva  

let print_g = function
  | None -> ()
  | Some (pv, m) ->
    Format.printf "(_a%a * " Integer.pr m;
    Format.printf "%a)" PredVar.pr pv

let print_g_tex = function
  | None -> ()
  | Some (pv, m) ->
    Format.printf "\\left(\\_a%a * " Integer.pr m;
    Format.printf "%a\\right)" PredVar.pr_tex pv

let print_gam (la, g, phi, h) =
  begin
    match g with
    | None ->
      begin
        match la with
        | [] -> ()
        | [pv] -> Format.printf "@,%a@," PredVar.pr pv
        | _ -> List.iter (Format.printf "@,%a && @, " PredVar.pr) la
      end
    | Some (pv, m) ->
      print_g g;
      match la with
      | [] -> ()
      | _ -> List.iter (Format.printf "@,&& %a@, " PredVar.pr) la
  end;
  begin
    if Formula.is_true phi then ()
    else (Format.printf "&& "; print_phi phi)
  end;
  Format.printf " => ";
  print_h h

let print_gam_tex (la, g, phi, h) =
  begin
    match g with
    | None ->
      begin
        match la with
        | [] -> ()
        | [pv] -> Format.printf "@,%a@," PredVar.pr_tex pv
        | _ -> List.iter (Format.printf "@,%a \\land @, " PredVar.pr_tex) la
      end
    | Some (pv, m) ->
      print_g_tex g;
      match la with
      | [] -> ()
      | _ -> List.iter (Format.printf "@, \\land %a@, " PredVar.pr_tex) la
  end;
  begin
    if Formula.is_true phi then ()
    else (Format.printf "\\land "; print_phi_tex phi)
  end;
  Format.printf " \\Rightarrow ";
  print_h_tex h

let rec print_gamma = function
  | [] -> Format.printf "[]@.@."
  | [gam] -> print_gam gam; Format.printf "@."
  | gam::rest -> print_gam gam; print_gamma rest

let rec print_gamma_tex = function
  | [] -> Format.printf "\\left[\\right]@.@."
  | [gam] -> print_gam_tex gam; Format.printf "\\\\@."
  | gam::rest -> print_gam_tex gam; print_gamma_tex rest

let print_tuple (ld, gamma, la, phi, h) =
  Format.printf "----------------------------@.";
  Format.printf "[D]: @,%a@." HCCS.pr ld;
  Format.printf "@.[Gamma]:  @,";
  print_gamma gamma;
  Format.printf "[A]: @,";
  print_la la;
  Format.printf "[phi]: @,";
  print_phi phi;
  Format.printf "@.[h]: @,";
  print_h h;
  Format.printf "----------------------------@."

let rec print_depth = function
  | [] -> ()
  | [i] -> Format.printf "%a" Integer.pr i
  | i::rest -> print_depth rest; Format.printf "-%a" Integer.pr i

let rec prettyprint gc depth = function
  | Node(tuple, ptree) ->
    print_tuple tuple;
    prettyprint gc depth ptree
  | Unfold(pvam, gam, ptrees) ->
    Format.printf "<rule(unfold)>:@.";
    Format.printf "Unfolded pva: ";
    print_pva_with_mark pvam;
    Format.printf "@.added hypothesis: @,";
    print_gam gam;
    Format.printf "@.";
    List.iteri
      (fun i pt ->
	     let depth' = i::depth in
	     Format.printf "Unfold(Case";
	     print_depth depth';
	     Format.printf "): ";
	     print_pva_with_mark pvam;
	     Format.printf "@.";
	     prettyprint gc depth' pt)
      ptrees
  | Fold(newla, ptree) ->
    Format.printf "<rule(Fold)>:@.";
    Format.printf "added A:@.";
    print_la newla;
    prettyprint gc depth ptree
  | Apply_Bot (newphi, ptree) ->
    Format.printf "<rule(apply_Bot)>:@.";
    Format.printf "newphi:@.";
    print_phi newphi;
    prettyprint gc depth ptree
  | Apply_P (newla, ptree) ->
    Format.printf "<rule(apply_P)>:@.";
    Format.printf "added A:@.";
    print_la newla;
    prettyprint gc depth ptree
  | Valid_Bot nphi ->
    Format.printf "<rule(valid_Bot)>:@.";
    Format.printf "%a@." Formula.pr nphi;
    Format.printf "is Valid!(end)@,@."
  | Valid_P (la, phi, h) ->
    Format.printf "<rule(valid_P)>:@.";
    Format.printf "Valid!(end)@,@."
  | InValid m ->
    Format.printf
      "!! Invalid !!@.invalid goal: %a@.counterexample: %a@.substituted: %a@."
      HornClause.pr gc
      TermSubst.pr m
      HornClause.pr (HornClause.subst_varsB m gc)
let prettyprint gc = prettyprint gc []

let prettyprints gcs =
  List.iteri
    (fun i pt ->
       Format.printf "@.[Theorem proving(No.%a)]@." Integer.pr i;
       prettyprint (List.nth gcs i) pt;
       Format.printf "---------QED(No.%a)----------@." Integer.pr i)

let print_tuple_tree (ld, gamma, la, phi, h) =
  Format.printf "@.\\begin{matrix}@.";
  Format.printf "@.\\left[Gamma\\right]:\\\\  @,";
  print_gamma_tex gamma;
  Format.printf "@.\\end{matrix}@.";
  Format.printf "@.\\begin{matrix}@.";
  Format.printf "\\left[A\\right]:\\\\ @,";
  print_la_tex la;
  Format.printf "@.\\end{matrix}@.";
  Format.printf "@.\\begin{matrix}@.";
  Format.printf "\\left[phi\\right]:\\\\ @,";
  print_phi_tex phi;
  Format.printf "@.\\end{matrix}@.";
  Format.printf "@.\\begin{matrix}@.";
  Format.printf "@.\\left[h\\right]:\\\\ @,";
  print_h_tex h;
  Format.printf "@.\\end{matrix}@."
let buff_lems = ref []
let treeprint gc =
  let name_count = ref 0 in
  let fresh_name () = 
    name_count:=(!name_count + 1);
    !name_count
  in
  (* inner : t -> string list *)
  let rec inner gc depth = function
    | Node(tuple, ptree) ->
       let rule_name = 
         begin 
           match ptree with 
           | Node      _ -> ""
           | Unfold    _ -> "unfold"
           | Fold      _ -> "fold"
           | Apply_Bot _ -> "apply\\_Bot"
           | Apply_P   _ -> "apply\\_P"
           | Valid_Bot _ -> "valid\\_Bot"
           | Valid_P   _ -> "valid\\_P"
           | InValid   _ -> "invalid"
         end
       in
       let p_name = Printf.sprintf "P%d" (fresh_name())
       in
       Format.printf "\\infer[%s]{%s}@." rule_name p_name;
       buff_lems := (p_name, Node(tuple, ptree))::(!buff_lems);
       (* print_tuple_tree tuple;*)
       inner gc depth ptree
    | Unfold(pvam, gam, ptrees) ->
       Format.printf "{@.";
       List.iteri
         (fun i pt ->
	   let depth' = i::depth in
	   Format.printf "@.";
	   inner gc depth' pt)
         ptrees;
       Format.printf "}@."
    | Fold(newla, ptree) ->
       Format.printf "{@.";
       inner gc depth ptree;
       Format.printf "}@."
    | Apply_Bot (newphi, ptree) ->
       Format.printf "{@.";
       inner gc depth ptree;
       Format.printf "}@."
    | Apply_P (newla, ptree) ->
       Format.printf "{@.";
       inner gc depth ptree;
       Format.printf "}@."
    | Valid_Bot nphi ->
       Format.printf "{@.";
       (*Format.printf "%a@." Formula.pr_tex nphi;*)
       (*Format.printf "is Valid!(end)@,@.";*)
       Format.printf "}@."
    | Valid_P (la, phi, h) ->
       Format.printf "{@.";
       (*Format.printf "Valid!(end)@,@.";*)
       Format.printf "}@."
    | InValid m ->
       Format.printf
         "!! Invalid !!@.invalid goal: %a@.counterexample: %a@.substituted: %a@."
         HornClause.pr_tex gc
         TermSubst.pr m
         HornClause.pr_tex (HornClause.subst_varsB m gc)
  in
  Format.printf "@.$$@.";
  inner gc []

let print_proof_tree = ref false
let output_file_name = ref ""

let treeprints gcs =
  List.iteri
    (fun i pt ->
      Format.printf "@.Theorem proving(No.%a)@." Integer.pr i;
      treeprint (List.nth gcs i) pt;
      Format.printf "QED(No.%a)@." Integer.pr i;
      Format.printf "where@.";
      buff_lems := List.rev (!buff_lems);
      List.iter 
        (fun (name, Node (tuple, _)) ->
          Format.printf "\\section{%s}@." name;
          print_tuple_tree tuple
        ) !buff_lems
    )
