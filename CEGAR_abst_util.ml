
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util



let hd xs =
  match xs with
      [x] -> x
    | _ -> assert false


let check env cond pbs p =
  let ps,_ = List.split pbs in
    Wrapper2.check env (cond@@ps) p

let make_conj pbs =
  match pbs with
      [] -> Const True
    | (_,b)::pbs -> List.fold_left (fun t (_,b) -> make_and t b) b pbs

let make_dnf pbss =
  match pbss with
      [] -> Const False
    | pbs::pbss' -> List.fold_left (fun t pbs -> make_or t (make_conj pbs)) (make_conj pbs) pbss'


let mapi f xs =
  let rec aux i xs =
    match xs with
        [] -> []
      | x::xs' -> (f i x) ::(aux (i + 1) xs')
  in
    aux 1 xs


let weakest env (cond:CEGAR_syntax.t list) ds p =
  if check env cond [] p then (*???*)
    Const True, Const False
  else if check env cond [] (make_not p) then (*???*)
    Const False, Const True
  else
    let fvp = get_fv p in
    let ts = ((cond @@ List.map fst ds):CEGAR_syntax.t list) in
    let ds =
      let rec fixp xs =
        let xs' =
          uniq compare
            (xs @
               (List.flatten
                  (List.map
                     (fun p ->
                        let fv = get_fv p in
                          if inter fv xs = []
                          then []
                          else fv)
                     ts)))
        in
          if List.length xs = List.length xs'
          then xs
          else fixp xs'
      in
      let fv = fixp fvp in
        List.filter (fun (p, _) -> subset (get_fv p) fv) ds
    in
    let nds = List.map (fun (p, b) -> make_not p, make_not b) ds in

    let f pbs =
      List.map
        (fun i ->
           if i > 0 then
             List.nth ds (i - 1)
           else
             List.nth nds (-i - 1))
        pbs
    in
    let pbss =
      if Flag.use_neg_pred
      then mapi (fun i _ -> [i]) ds @ mapi (fun i _ -> [-i]) ds
      else mapi (fun i _ -> [i]) ds
    in
    let rec loop xs' nxs' ys' pbss =
      let xs, qs = List.partition
        (fun pbs ->
           let pbs = f pbs in
           let fvs =
             List.flatten
               (List.map
                  (fun (p, _) ->
                     get_fv p)
                  pbs)
           in
             if inter fvp fvs = [] && inter (rev_map_flatten get_fv cond) fvs = [] then
               false
             else
               check env cond pbs p)
        pbss
      in
      let nxs, ys = List.partition
        (fun pbs ->
           let pbs = f pbs in
           let fvs =
             List.flatten
               (List.map
                  (fun (p, _) ->
                     get_fv p)
                  pbs)
           in
             if inter fvp fvs = [] && inter (rev_map_flatten get_fv cond) fvs = [] then
               false
             else
               check env cond pbs (make_not p))
        pbss
      in
      let xs = uniq compare (xs' @ xs) in
      let nxs = uniq compare (nxs' @ nxs) in
      let ys = uniq compare (ys' @ ys) in
      let ws = 
        uniq compare
          (List.flatten
             (List.map
                (fun y1 ->
                   List.map
                     (fun y2 ->
                        List.sort compare
                          (uniq compare (y1 @ y2)))
                     ys)
                ys))
      in
      let ws =
        let ok w =
          if List.length w > !Flag.wp_max_num then
            false
          else
            let rec ok w =
              match w with
                  [] -> true
                | a::b -> not (List.mem (-a) b) && ok b
            in
              ok w
        in
          List.filter ok ws
      in
      let ws =
        if false then
          diff ws ys
        else
          List.filter (fun w -> not (List.exists (fun x -> diff w x = []) ys)) ws
      in
      let ws = List.filter
        (fun w -> not (List.exists (fun x -> diff x w = []) xs) &&
           not (List.exists (fun x -> diff x w = []) nxs)) ws in
      let ws =
        let rec aux xs =
          match xs with
              [] -> []
            | x::xs' ->
                if List.exists (fun y -> diff y x = []) xs' then aux xs' else x::(aux xs')
        in
          aux ws
      in
        if ws = [] then xs, nxs else loop xs nxs ys ws
    in
    let xs, nxs = loop [] [] [] pbss in
    let pbss = List.map f xs in
    let pbss = List.filter (fun pbs -> not (check env cond pbs (Const False))) pbss in
    let npbss = List.map f nxs in
    let npbss = List.filter (fun pbs -> not (check env cond pbs (Const False))) npbss in
      make_dnf pbss, make_dnf npbss



let abst env cond pbs p =
  let tt, ff = weakest env cond pbs p in
    make_if tt (Const True) (make_if ff (Const False) (make_br (Const True) (Const False)))



let assume env cond pbs t1 t2 =
  if t1 = Const True
  then t2
  else
    let _,ff = weakest env cond pbs t1 in
      make_if ff (Const Bottom) t2

(* TODO: equiv *)
let rec congruent env cond typ1 typ2 =
  match typ1,typ2 with
      TBase(TBottom,_), _ -> true
    | TBase(b1,ps1), TBase(b2,ps2) ->
        assert (b1=b2);
        let x = new_id "x" in
(*
          List.for_all2 (equiv env cond) (ps1 (Var x)) (ps2 (Var x))
*)
        let ps1' = ps1 (Var x) in
        let ps2' = ps2 (Var x) in
          List.length ps1' = List.length ps2' && List.for_all2 (=) ps1' ps2'
    | TFun typ1, TFun typ2 ->
        let x = new_id "x" in
        let typ11,typ12 = typ1 (Var x) in
        let typ21,typ22 = typ2 (Var x) in
          congruent env cond typ11 typ21 && congruent env cond typ12 typ22


let rec is_base_term env = function
    Const (Unit | True | False | Int _ | RandInt) -> true
  | Const _ -> false
  | Var x ->
      begin
        match List.assoc x env with
            TBase _ -> true
          | _ -> false
      end
  | App(App(Const (And|Or|Lt|Gt|Leq|Geq|Eq|Add|Sub|Mul),t1),t2) ->
      assert (is_base_term env t1);
      assert (is_base_term env t2);
      true
  | App(Const Not,t) -> assert (is_base_term env t); true
  | App _ -> false
  | Let _ -> false



let rec make_arg_let = function
    Const c -> Const c
  | Var x -> Var x
  | App(Const (Event s), t) -> App(Const (Event s), make_arg_let t)
  | App _ as t ->
      let t',ts = decomp_app t in
      let ts' = List.map make_arg_let ts in
      let xs = List.map (fun _ -> new_id "a") ts in
      let t'' = List.fold_left (fun t x -> App(t, Var x)) t' xs in
        List.fold_left2 (fun t' x t -> Let(x, t, t')) t'' xs ts'
  | Let _ -> assert false

let rec reduce_let env = function
    Const c -> Const c
  | Var x -> Var x
  | App(t1,t2) -> App(reduce_let env t1, reduce_let env t2)
  | Let(x,(Var _ | Const _ as t1),t2) -> reduce_let env (subst x t1 t2)
  | Let(x,t1,t2) ->
      match get_typ env t1 with
          TBase _ as typ -> Let(x, reduce_let env t1, reduce_let ((x,typ)::env) t2)
        | TFun _ -> reduce_let env (subst x t1 t2)
        | _ -> assert false
let make_arg_let env defs =
  List.map (fun (f,xs,t1,t2) -> f, xs, t1, reduce_let (get_env (List.assoc f env) xs @@ env) (make_arg_let t2)) defs



let add_label_term = function
    App(App(App(Const If, t1), t2), t3) ->
      make_if t1 (App(Const (Label 0), t2)) (App(Const (Label 1), t3))
  | t -> t

let rec add_label prog =
  let aux (env,defs,main) =
    env, List.map (fun (f,xs,t1,t2) -> f,xs,t1,add_label_term t2) defs, main
  in
  let prog = to_if_exp prog in
  let prog = aux prog in
    of_if_exp prog
    







