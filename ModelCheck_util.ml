
open Format
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type node = BrNode | FailNode | LineNode of int | EventNode of string



let parse_node = function
    "br" -> BrNode
  | "event_fail" -> FailNode
  | s when s.[0] = 'l' -> LineNode (int_of_string (String.sub s 1 (String.length s - 1)))
  | s -> EventNode s
      

let get_pair s =
  let n1 = String.index s ',' in
  let n2 = String.index s ')' in
  let node = parse_node (String.sub s 1 (n1-1)) in
  let n = int_of_string (String.sub s (n1+1) (n2-n1-1)) in
  let s' = String.sub s (n2+1) (String.length s-n2-1) in
    node, n, s'

let rec parse_trace s =
  match s.[0] with
      '.' -> []
    | ' ' -> parse_trace (String.sub s 1 (String.length s - 1))
    | '(' ->
      let node,n,s' = get_pair s in
        (node,n) :: parse_trace s'
    | _ -> assert false

let print_node fm = function
    BrNode -> Format.fprintf fm "br"
  | FailNode -> Format.fprintf fm "fail"
  | LineNode n -> Format.fprintf fm "#%d" n
  | EventNode s -> Format.fprintf fm "%s" s

let print_const fm = function
    Event s -> Format.fprintf fm "event_%s" s
  | Label n -> Format.fprintf fm "l%d" n
  | Unit -> Format.fprintf fm "unit"
  | True -> Format.fprintf fm "0"
  | False -> Format.fprintf fm "1"
  | If -> Format.fprintf fm "_case 2"
  | Bottom -> Format.fprintf fm "Bottom"
  | c -> Format.printf "print_const: %a@." CEGAR_print.print_term (Const c); assert false

let print_var = Format.pp_print_string

let rec print_term fm = function
    Const c -> print_const fm c
  | Var x -> print_var fm x
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      Format.fprintf fm "br (%a) (%a)" print_term t2 print_term t3
  | App _ as t ->
      let t,ts = decomp_app t in
      let aux fm t =
        match t with
            App _ -> Format.fprintf fm "(%a)" print_term t
          | _ -> Format.fprintf fm "%a" print_term t
      in
        print_list aux " " false fm (t::ts)

let rec print_fun_def fm (f,xs,t1,t2) =
  assert (t1 = Const True);
  Format.fprintf fm "%a -> %a.@." (print_list print_var " " false) (f::xs) print_term t2

let print_hors fm (defs, spec) =
    fprintf fm "%%BEGING\n";
    List.iter (print_fun_def fm) defs;
    fprintf fm "%%ENDG\n\n";
    fprintf fm "%%BEGINA\n";
    Automata.print_buchi fm spec;
    fprintf fm "%%ENDA\n\n"




let print_hors_to_file hors =
  let cout = open_out Flag.trecs_log in
  let fm = formatter_of_out_channel cout in
    print_hors fm hors;
    pp_print_flush fm ();
    close_out cout





let model_check_aux ((env,defs,main),spec) =
  let cin,cout = Unix.open_process Flag.trecs in
  let fm = formatter_of_out_channel cout in
  let () = print_hors fm (defs, spec) in
  let () = print_hors_to_file (defs, spec) in
  let () = pp_print_flush fm () in
  let () = close_out cout in
  let s1 = ignore (input_line cin); ignore (input_line cin); input_line cin in
  let s2 = ignore (input_line cin); input_line cin in
  let _ = close_in cin in
  let () = pp_print_flush std_formatter () in
  let () =
    match Unix.close_process (cin, cout) with
        Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> ()
  in
    if s1 = "The property is not satisfied."
    then
      let () =
        if Flag.print_trecs_output
        then printf "TRecS output: %s@.@." s2
      in
      let ce = parse_trace s2 in
      let () = if true then List.iter (fun (node,i) -> Format.printf "(%a,%d) " print_node node i) ce in
      let ce' = List.flatten (List.map (function (LineNode i,_) -> [i] | _ -> []) ce) in
        Some ce'
    else
      begin
        assert (String.sub s1 (String.length s1 - 3) 3 = " : ");
        None
      end




let make_line_spec n q =
  let rec aux i spec =
    if i < 0
    then spec
    else aux (i-1) ((q, "l" ^ string_of_int i, [q])::spec)
  in
    aux n []

let make_file_spec () =
  [0, "event_newr", [1];
   1, "event_read", [1];
   1, "event_close", [4];
   0, "event_neww", [2];
   2, "event_write", [2];
   2, "event_close", [4];
   2, "event_newr", [3];
   1, "event_neww", [3];
   3, "event_read", [3];
   3, "event_write", [3];
   3, "event_close", [3];]


let make_base_spec n q = (q, "br", [q;q])::(q, "unit", [])::make_line_spec (n+2) q

let make_spec n =
  let spec =
    match !Flag.mode with
        Flag.Reachability -> make_base_spec n 0
      | Flag.FileAccess ->
          let spec = make_file_spec () in
          let qm = List.fold_left (fun acc (n,_,_) -> max acc n) 0 spec in
          let spec' = rev_flatten_map (fun i -> make_base_spec n i) (Array.to_list (Array.init qm (fun i -> i))) in
            spec @@ spec'
  in
    List.sort compare spec


let capitalize_var = String.capitalize

let capitalize (env,defs,main) =
  let env' = List.map (fun (f,typ) -> capitalize_var f, typ) env in
  let map = List.map (fun (f,_) -> f, Var (capitalize_var f)) env in
  let aux (f,xs,t1,t2) = capitalize_var f, xs, subst_map map t1, subst_map map t2 in
  let defs' = List.map aux defs in
  let main' = capitalize_var main in
    env', defs', main'


(*
let move_main (env,defs,main) =
  let rec aux acc = function
      [] -> List.rev acc
    | (f,xs,t1,t2)::defs when f = main -> aux (acc@[(f,xs,t1,t2)]) defs
    | def::defs -> aux (def::acc) defs
  in
    env, aux defs, main
*)

let elim_non_det (env,defs,main) =
  let check f (g,_,_,_) = f = g in
  let mem f defs = List.exists (check f) defs in
  let rec aux = function
      [] -> []
    | (f,xs,t1,t2)::defs when mem f defs ->
        let f' = rename_id f in
        let defs1,defs2 = List.partition (check f) defs in
        let defs1' = List.map (fun (f,xs,t1,t2) -> rename_id f,xs,t1,t2) defs1 in
        let ts = List.map (fun x -> Var x) xs in
        let t = List.fold_left (fun t (f,_,_,_) -> make_br (make_app (Var f) ts) t) (make_app (Var f') ts) defs1' in
          (f,xs,Const True,t)::(f',xs,t1,t2)::defs1' @ aux defs2
    | def::defs -> def :: aux defs
  in
    Typing.infer ([], aux defs, main)


let make_bottom (env,defs,main) =
  let bottoms = ref [] in
  let aux_def (f,xs,t1,t2) =
    let env' = get_env (List.assoc f env) xs @@ env in
    let make_bottom n =
      let x = "Bottom" ^ string_of_int n in
        bottoms := (x,n)::!bottoms;
        Var x
    in
    let rec aux_term = function
        Const Bottom, typ ->
          let n = get_arg_num typ in
            make_bottom n
      | Const c, _ -> Const c
      | Var x, _ -> Var x
      | App(App(App(Const If, t1), t2), t3), typ ->
          let t1' = aux_term (t1,TBase(TBool,fun _ -> [])) in
          let t2' = aux_term (t2,typ) in
          let t3' = aux_term (t3,typ) in
            App(App(App(Const If, t1'), t2'), t3')
      | App(Const (Label n), t), typ -> App(Const (Label n), aux_term (t,typ))
      | App(t1,t2), _ ->
          let typ = get_typ env' t1 in
          let typ' =
            match typ with
                TFun typ -> fst (typ (Const Unit))
              | _ -> assert false
          in
            App(aux_term (t1,typ), aux_term (t2,typ'))
    in
    let t2' =
      try
        aux_term (t2, get_typ env' t2)
      with TypeBottom -> make_bottom 0
    in
      f, xs, t1, t2'
  in
  let make (x,n) =
    let xs = Array.to_list (Array.init n (fun _ -> "x")) in
      x, xs, Const True, make_app (Var x) (List.map (fun x -> Var x) xs)
  in
  let defs' = List.map aux_def defs in
  let bottom_defs = List.map make (uniq compare !bottoms) in
    env, bottom_defs@@defs', main


let rec eta_expand_term env = function
    Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      let typ = get_typ env t2 in
      let xs = Array.to_list (Array.init (arg_num typ) (fun _ -> new_id "x")) in
      let aux t = List.fold_left (fun t x -> App(t, Var x)) t xs in
      let t = make_if (Const RandBool) (aux t2) (aux t3) in
        List.fold_right (fun x t -> Fun(x,t)) xs t
  | App(t1, t2) -> App(eta_expand_term env t1, eta_expand_term env t2)


let eta_expand_def env (f,xs,t1,t2) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = eta_expand_term (get_env (List.assoc f env) xs @@ env) t2 in
  let t2'' = List.fold_left (fun t x -> App(t, Var x)) t2' ys in
    f, xs@ys, t1, put_into_term t2''

let eta_expand ((env,defs,main) : prog) : prog=
  lift2 (env, List.map (eta_expand_def env) defs, main)


