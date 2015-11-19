open HORS_syntax
open Util
open CEGAR_util

(* counter-example tree *)
type tree =
  | Node   of string * tree
  | Br_e   of tree * tree (* branch_exists *)
  | Leaf

let rec pp_tree fm = function
  | Node (l, t) ->
     Format.fprintf fm "(%s %a)" l pp_tree t
  | Br_e (t1, t2) ->
     Format.fprintf fm "(br_exists %a %a)" pp_tree t1 pp_tree t2
  | Leaf ->
     Format.fprintf fm "()"

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

(* generate counter-example tree *)
let rec expand_tree rules n expr =
  let get_fun f =
    try
      Some (List.assoc f rules)
    with Not_found ->
      None in

  (* eval expression that does not generate tree *)
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
    Leaf
  else match expr with
  | Var s ->
     begin match get_fun s with
     | None ->
        Leaf
     | Some e ->
        expand_tree rules (n - 1) e
     end
  | Apply (e1, e2) ->
     begin match eval e1 with
       | Var s ->
          let t = expand_tree rules (n - 1) e2 in
          Node (s, t)
       | Abst (x, e) ->
          expand_tree rules n (subst (x, e) e2)
       | Apply(Var s, e) when s = "br_exists" ->
          begin
            let t1 = expand_tree rules (n/2) e in
            let t2 = expand_tree rules (n/2) e2 in
            Br_e (t1, t2)
          end
       | e ->
          begin
            Format.printf "exp:%a@." pp_expr e;
            assert false
          end
     end
  | Abst _ ->
     Leaf

(* HorsatPInterfaceに書くべき?? *)
type path =
    int list (* branch info. *)
    * bool list (* partially constructed external input info. *)
    * ((int * (bool list)) list) (* external input info. *)
let add_next_rand_info r (branch,bs,ext) = (branch, [], (CEGAR_syntax.decomp_randint_label r, bs) :: ext)
let add_tf_info b (branch,bs,ext) = (branch,b::bs,ext)
let add_branch_info b (branch,bs,ext) = (b::branch,bs,ext)

(* gather error paths *)
let rec error_trace_aux : tree -> path list = function
  | Br_e (t1, t2) ->
     begin
       (*Format.printf "[%a %a]@." pp_tree t1 pp_tree t2;*)
       error_trace_aux t1 @ error_trace_aux t2
     end
  | Node ("l0", t) -> List.map (add_branch_info 0) @@ error_trace_aux t
  | Node ("l1", t) -> List.map (add_branch_info 1) @@ error_trace_aux t
  | Node ("tt", t) -> List.map (add_tf_info true)  @@ error_trace_aux t
  | Node ("ff", t) -> List.map (add_tf_info false) @@ error_trace_aux t
  | Node (r, t) when CEGAR_syntax.is_randint_label r -> List.map (add_next_rand_info r) @@ error_trace_aux t
  | Node (_, t) -> error_trace_aux t
  | Leaf -> [([], [], [])]

let error_trace tree =
  List.fold_left (fun (xs,ys) (x,_,y) -> (x::xs, y::ys)) ([],[]) @@ error_trace_aux tree


let show_pos fname filebuf =
  let pos = filebuf.Lexing.lex_start_p in
  Printf.eprintf "File \"%s\", line %d, character %d:\n"
    fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)


let cegar prog0 labeled info is_cp ce_file prog =
  let path_counter = ref 0 in
  let inchan = open_in ce_file in
  let filebuf = Lexing.from_channel inchan in
  try
    let rules = HORS_parser.top HORS_lexer.token filebuf in
    Format.printf "%a@." (Format.pp_print_list HORS_syntax.pp_rule) rules;
    let ce_tree = expand_tree rules 100 (Var "Main") in
    Format.printf "CounterExample: %a@." pp_tree ce_tree;
    let (cexs, ext_cexs) = error_trace ce_tree in
    Format.printf "Trace: (%a,@." (List.print (List.print Format.pp_print_int)) @@ cexs;
    Format.printf "        %a)@." (List.print (List.print (fun fm (a, bs)-> Format.fprintf fm "(%a, %a)" Format.pp_print_int a (List.print Format.pp_print_bool) bs))) @@ ext_cexs;
    let map_randint_to_preds = make_map_randint_to_preds prog0 in
    Format.printf "prog: %a@." CEGAR_print.prog prog;
    let paths =
    List.filter_map2
      (fun orig_ce ext_ce ->
        path_counter := !path_counter + 1;
        let ce = CEGAR_trans.trans_ce labeled prog orig_ce in
        if !Flag.print_progress then Feasibility.print_ce_reduction ~map_randint_to_preds ~ext_ce ce prog;
        let ext_path = ext_ce |> arrange_ext_preds_sequence |> conv_path in
        (* let ext_preds = ext_path |> List.map (FpatInterface.trans_ext renv map_randint_to_preds) in *)
        let path = Feasibility.check_non_term ~map_randint_to_preds ~ext_ce ce prog in
        match path with
          | Feasibility.Feasible _ -> assert false
          | Feasibility.FeasibleNonTerm (true, env, sol) ->
             None (* do not use a useless (i.e. already-used-in-CEGAR) error-path *)
          | Feasibility.FeasibleNonTerm (false, env, sol) ->
            Some (path, orig_ce, ce, ext_path)
          | Feasibility.Infeasible prefix ->
             Some (path, orig_ce, ce, ext_path))
      cexs ext_cexs
    in
    ()
  with
  | HORS_lexer.LexerError msg ->
     (show_pos ce_file filebuf;
      Format.eprintf "error: lexer: %s" msg;
     exit 1)
  | Failure "parse error" ->
     begin
       show_pos ce_file filebuf;
       let s = Lexing.lexeme filebuf in
       Format.eprintf "error: parser : syntax error near '%s'@." s;
       if s = "->" then
         Format.eprintf "Did you forget '.' at end of line?@.";
       exit 1
     end
