
open Syntax
open Util


let funs = ref []


let rec trans0 c = function
    Unit -> c Unit
  | True -> c True
  | False -> c False
  | Int n -> c (Int n)
  | NInt x -> c (NInt x)
  | Var x -> c (Var x)
  | Fun(x, t) ->
      let f = new_var' "f" in
        funs := f::!funs;
        trans0 c (Let(f, [x], t, Var f))
  | App(t1, []) -> assert false
  | App(t1, [t2]) ->
      let k = new_var' "k" in
      let r = new_var' "r" in
      let c1 y x = Let(k, [r], c (Var r), App(x, [y; Var k])) in
      let c2 y = trans0 (c1 y) t1 in
        funs := k::!funs;
        trans0 c2 t2
  | App(t1, t2::ts) ->
      trans0 c (App(App(t1,[t2]),ts))
  | If(t1, t2, t3, _) ->
      let k = new_var' "k" in
      let x = new_var' "x" in
      let t2' = trans0 (fun y -> App(Var k, [y])) t2 in
      let t3' = trans0 (fun y -> App(Var k, [y])) t3 in
      let c' y = Let(k, [x], c (Var x), If(y, t2', t3', Unit)) in
        funs := k::!funs;
        trans0 c' t1
  | Let(f, xs, t1, t2) ->
      begin
        match xs with
            [] ->
              let c' t = trans0 c (subst f t t2) in
                trans0 c' t1
          | _::[] ->
              let k = new_var' "k" in
              let t1' = trans0 (fun y -> App(Var k, [y])) t1 in
              let t2' = trans0 c t2 in
                Let(f, xs@[k], t1', t2')
          | x::xs' ->
              let g = new_var' "x" in
                funs := g::!funs;
                trans0 c (Let(f, [x], Let(g, xs', t1, Var g), t2))
      end
  | Letrec(f, xs, t1, t2) ->
      begin
        match xs with
            [] ->
              let c' t = trans0 c (subst f t t2) in
                trans0 c' t1
          | _::[] ->
              let k = new_var' "k" in
              let t1' = trans0 (fun y -> App(Var k, [y])) t1 in
              let t2' = trans0 c t2 in
                Letrec(f, xs@[k], t1', t2')
          | x::xs' ->
              let g = new_var' "x" in
                funs := g::!funs;
                trans0 c (Letrec(f, [x], Let(g, xs', t1, Var g), t2))
      end
  | BinOp(op, t1, t2) ->
      let c1 t1' t2' = c (BinOp(op, t1', t2')) in
      let c2 y1 = trans0 (fun y2 -> c1 y1 y2) t2 in
        trans0 c2 t1
  | Not t ->
      let c' t1 = c (Not t1) in
        trans0 c' t
  | Fail -> c (Fail)
  | Unknown -> c Unknown
  | _ -> assert false

let rec trans1 c = function
    Unit -> c Unit
  | True -> c True
  | False -> c False
  | Int n -> c (Int n)
  | NInt x -> c (NInt x)
  | Var x -> c (Var x)
  | Fun(x, t) ->
      let f = new_var' "f" in
        funs := f::!funs;
        trans1 c (Let(f, [x], t, Var f))
  | App(t1, ts) ->
      let k = new_var' "x" in
      let r = new_var' "x" in
      let c1 x = app2app x [Var k] in
      let cc = List.fold_right (fun t cc -> fun x -> trans1 (fun y -> cc (app2app x [y])) t) ts c1 in
        funs := k::!funs;
        Let(k, [r], c (Var r), trans1 cc t1)
  | If(t1, t2, t3, _) ->
      let k = new_var' "x" in
      let x = new_var' "x" in
      let t2' = trans1 (fun y -> App(Var k, [y])) t2 in
      let t3' = trans1 (fun y -> App(Var k, [y])) t3 in
      let c' y = Let(k, [x], c (Var x), If(y, t2', t3', Unit)) in
        funs := k::!funs;
        trans1 c' t1
  | Let(f, xs, t1, t2) ->
      begin
        match xs with
            [] ->
              let c' t = trans1 c (subst f t t2) in
                trans1 c' t1
          | _::_ ->
              let k = new_var' "x" in
              let t1' = trans1 (fun y -> App(Var k, [y])) t1 in
              let t2' = trans1 c t2 in
                Let(f, xs@[k], t1', t2')
      end
  | Letrec(f, xs, t1, t2) ->
      begin
        match xs with
            [] ->
              let c' t = trans1 c (subst f t t2) in
                trans1 c' t1
          | _::_ ->
              let k = new_var' "x" in
              let t1' = trans1 (fun y -> App(Var k, [y])) t1 in
              let t2' = trans1 c t2 in
                Letrec(f, xs@[k], t1', t2')
      end
  | BinOp(op, t1, t2) ->
      let c1 t1' t2' = c (BinOp(op, t1', t2')) in
      let c2 y1 = trans1 (fun y2 -> c1 y1 y2) t2 in
        trans1 c2 t1
  | Not t ->
      let c' t1 = c (Not t1) in
        trans1 c' t
  | Fail -> c (Fail)
  | Unknown -> c Unknown
  | t -> (Format.printf "%a@." (Syntax.print_term_fm Syntax.ML false) t; assert false)









let rec inlining funs defs = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(Var f, ts) ->
      if List.exists (fun g -> f.id = g.id) funs && List.length (get_args f.typ) = List.length ts
      then
        let xs,t = List.assoc f defs in
          List.fold_right2 subst xs ts t
      else App(Var f, ts)
  | App(Fail, ts) -> App(Fail, ts)
  | App _ -> assert false
  | If(t1, t2, t3, t4) ->
      let t2' = inlining funs defs t2 in
      let t3' = inlining funs defs t3 in
      let t4' = inlining funs defs t4 in
        If(t1, t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs ((f,(xs,t1'))::defs) t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = inlining funs defs t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
    


let rec replace_part_app = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(f, ts) ->
      let f' = replace_part_app f in
      let ts' = List.map replace_part_app ts in
      let typ = Typing.get_typ (App(f, ts)) in
      let args = get_args typ in
        begin
        match args with
            [] -> App(f', ts')
          | _ ->
            let g = new_var' "f" in
            let ts'' = List.map (fun x -> Var x) args in
              Let(g, args, App(f', ts'@ts''), Var g)
        end
  | If(t1, t2, t3, t4) ->
      let t1' = replace_part_app t1 in
      let t2' = replace_part_app t2 in
      let t3' = replace_part_app t3 in
      let t4' = replace_part_app t4 in
        If(t1', t2', t3', t4')
  | Branch(t1, t2) ->
      let t1' = replace_part_app t1 in
      let t2' = replace_part_app t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let t1' = replace_part_app t1 in
      let t2' = replace_part_app t2 in
        Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let t1' = replace_part_app t1 in
      let t2' = replace_part_app t2 in
        Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = replace_part_app t1 in
      let t2' = replace_part_app t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = replace_part_app t in
        Not t'
  | Fail -> Fail
  | Label(b,t) ->
      let t' = replace_part_app t in
        Label(b, t')



let trans t =
  let t1 = Typing.typing t in
  let t2 = replace_part_app t1 in
  let t3 =
    try
      Typing.typing (trans1 (fun x -> x) t2)
    with Typing.CannotUnify -> assert false
  in
  let t4 = inlining !funs [] t3 in
  let t5 = remove_unused t4 in
  let t6 = part_eval t5 in
    t6



