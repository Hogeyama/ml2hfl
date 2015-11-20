open HORS_syntax
open Util
open CEGAR_util

let leaf () =
  Rose_tree.leaf "end"
let node label t =
  Rose_tree.Node (label, [t])
let branch label t1 t2 =
  Rose_tree.Node (label, [t1;t2])

let rec subst (x, ex1) ex2 =
  match ex1 with
  | Var s ->
     if s = x then
       ex2
     else
       Var s
  | Apply (e1, e2) ->
     Apply (subst (x, e1) ex2, subst (x, e2) ex2)
  | Abst (s, e1) ->
     Abst (s, subst (x, e1) ex2)

(** generate counter-example tree *)
let rec expand_tree rules n expr =
  let get_fun f =
    try
      Some (List.assoc f rules)
    with Not_found ->
      None in

  (** eval expression that does not generate tree *)
  let rec eval = function
    | Var s ->
       begin match get_fun s with
       | None -> Var s
       | Some e -> eval e
       end
    | Apply (e1, e2) ->
       begin match eval e1 with
       | Abst (x, e1') ->
          eval (subst(x, e1') e2)
       | Var s ->
          Apply(Var s, e2)
       | _ ->
          assert false
       end
    | Abst (x, e) -> Abst (x, e) in

  if n <= 0 then
    leaf ()
  else match expr with
  | Var s ->
     begin match get_fun s with
     | None ->
        leaf ()
     | Some e ->
        expand_tree rules (n - 1) e
     end
  | Apply (e1, e2) ->
     begin match eval e1 with
       | Var s ->
          let t = expand_tree rules (n - 1) e2 in
          node s t
       | Abst (x, e) ->
          expand_tree rules n (subst (x, e) e2)
       | Apply(Var s, e) when s = "br_exists" ->
          begin
            let t1 = expand_tree rules (n/2) e in
            let t2 = expand_tree rules (n/2) e2 in
            branch "br_exists" t1 t2
          end
       | e ->
          begin
            Format.printf "exp:%a@." pp_expr e;
            assert false
          end
     end
  | Abst _ ->
     leaf ()

let cegar prog0 labeled info is_cp ce_rules prog =
  let ce_tree = expand_tree ce_rules 500 (Var "Main") in
  (*feasiblity check and refinement is common with non-termination*)
  CEGAR_non_term.cegar prog0 labeled info is_cp ce_tree prog
