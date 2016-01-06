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

  let is_dummy = function
    | Var "_" -> true
    | _ -> false in

  match expr with
  | Var s ->
     begin match get_fun s with
     | Some e when n > 0 ->
        expand_tree rules (n - 1) e
     | _ ->
        leaf ()
     end
  | Abst _ ->
     leaf ()
  | Apply (e1, e2) ->
     begin match eval e1, n > 0 with
     | Var s, false when s = "tt" || s = "ff" ->  (* expansion is continued when label is "tt" or "ff" *)
        let t = expand_tree rules (n - 1) e2 in
        node s t
     | _, false ->
        leaf ()
     | Var s, _ ->
        let t = expand_tree rules (n - 1) e2 in
        node s t
     | Abst (x, e), _ ->
        expand_tree rules n (subst (x, e) e2)
     | Apply(Var s, e), _ when s = "br_exists" ->
        let t1 = expand_tree rules (n/2) e in
        let t2 = expand_tree rules (n/2) e2 in
        branch "br_exists" t1 t2
     | Apply(Var s, e), _ when s = "br_forall" ->
        assert (is_dummy e || is_dummy e2);
        let e' = if is_dummy e then e2 else e in
        let t = expand_tree rules (n - 1) e' in
        node "br_forall" t
     | Apply(v, _), _ when is_dummy v ->
        leaf ()
     | e, _ ->
        Format.printf "exp:%a@." pp_expr e;
        assert false
     end

let cegar prog0 labeled info is_cp ce_rules prog =
  let start_symbol = fst @@ List.hd ce_rules in
  let ce_tree = expand_tree ce_rules 50 (Var start_symbol) in
  Format.printf "tree: %a@." (Rose_tree.print Format.pp_print_string) ce_tree;

  (*feasiblity check and refinement is common with that of non-termination*)
  CEGAR_non_term.cegar prog0 labeled info is_cp ce_tree prog
