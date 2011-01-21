
open Syntax
open Util


type typed_term = {t_cps:t_cps; typ_cps:typ_cps}
and typed_ident = {id_cps:ident; id_typ:typ_cps}
and t_cps =
    UnitCPS
  | TrueCPS
  | FalseCPS
  | UnknownCPS
  | IntCPS of int
  | NIntCPS of typed_ident
  | VarCPS of typed_ident
  | FunCPS of typed_ident * typed_term
  | AppCPS of typed_term * typed_term list
  | IfCPS of typed_term * typed_term * typed_term
  | BranchCPS of typed_term * typed_term
  | LetCPS of rec_flag * typed_ident * typed_ident list * typed_term * typed_term
  | BinOpCPS of binop * typed_term * typed_term
  | NotCPS of typed_term
  | FailCPS
  | LabelCPS of bool * typed_term
  | EventCPS of string
and typ_cps =
    TBaseCPS
  | TVarCPS of typ_cps option ref
  | TFunCPS of bool ref * typ_cps * typ_cps




let rec print_typ_cps fm = function
    TBaseCPS -> Format.fprintf fm "o"
  | TVarCPS {contents = None} -> Format.fprintf fm "?"
  | TVarCPS {contents = Some typ} -> Format.fprintf fm "[%a]" print_typ_cps typ
  | TFunCPS({contents=b},typ1,typ2) ->
      Format.fprintf fm "(%a %s %a)" print_typ_cps typ1 (if b then "=>" else "->") print_typ_cps typ2

and print_typed_termlist fm = List.iter (fun bd -> Format.fprintf fm "@;%a" print_typed_term bd)

and print_typed_term fm {t_cps=t; typ_cps=typ} =
  Format.fprintf fm "(%a : %a)" print_t_cps t print_typ_cps typ

and print_t_cps fm = function
    UnitCPS -> Format.fprintf fm "unit"
  | TrueCPS -> Format.fprintf fm "true"
  | FalseCPS -> Format.fprintf fm "false"
  | UnknownCPS -> Format.fprintf fm "***"
  | IntCPS n -> Format.fprintf fm "%d" n
  | NIntCPS x -> Format.fprintf fm "?%a?" print_id x.id_cps
  | VarCPS x -> print_id fm x.id_cps
  | FunCPS(x, t) ->
      Format.fprintf fm "fun %a -> %a" print_id x.id_cps print_typed_term t
  | AppCPS(t, ts) ->
      Format.fprintf fm "%a%a" print_typed_term t print_typed_termlist ts
  | IfCPS(t1, t2, t3) ->
      Format.fprintf fm "@[@[if %a@]@;then @[%a@]@;else @[%a@]@]"
        print_typed_term t1 print_typed_term t2 print_typed_term t3
  | BranchCPS(t1, t2) ->
      Format.fprintf fm "br %a %a" print_typed_term t1 print_typed_term t2
  | LetCPS(flag, f, xs, t1, t2) ->
      let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
      let p_ids fm xs = Format.fprintf fm "%a" print_ids (List.map (fun x -> x.id_cps) xs)
      in
        begin
          match t2.t_cps with
              LetCPS _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]"
                  s_rec p_ids (f::xs) print_typed_term t1 print_typed_term t2
            | _ ->
                Format.fprintf fm "@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]"
                  s_rec p_ids (f::xs) print_typed_term t1 print_typed_term t2
        end
  | BinOpCPS(op, t1, t2) ->
      let op =
        match op with
            Eq -> "="
          | Lt -> "<"
          | Gt -> ">"
          | Leq -> "<="
          | Geq -> ">="
          | And -> "&&"
          | Or -> "||"
          | Add -> "+"
          | Sub -> "-"
          | Mult -> "*"
      in
        Format.fprintf fm "%a %s %a" print_typed_term t1 op print_typed_term t2
  | NotCPS t ->
      Format.fprintf fm "not %a" print_typed_term t
  | FailCPS -> Format.fprintf fm "fail"
  | LabelCPS(true, t) ->
      Format.fprintf fm "l_then %a" print_typed_term t
  | LabelCPS(false, t) ->
      Format.fprintf fm "l_else %a" print_typed_term t
  | EventCPS s -> Format.fprintf fm "{%s}" s





let new_tvar () = TVarCPS (ref None)

let rec set_tfun = function
    TBaseCPS -> assert false
  | TFunCPS(r, typ1, typ2) -> r := true
(*
  | TVarCPS({contents=Some typ}) -> set_tfun typ
*)
  | _ -> ()

let rec flatten = function
    TVarCPS{contents = Some typ} -> flatten typ
  | typ -> typ

let rec occurs r typ =
  match flatten typ with
      TFunCPS(_,typ1,typ2) -> occurs r typ1 || occurs r typ2
    | TVarCPS({contents = None} as r') -> r == r'
    | _ -> false

let rec unify typ1 typ2 =
  match flatten typ1, flatten typ2 with
      TBaseCPS, TBaseCPS -> ()
    | TVarCPS r1, TVarCPS r2 when r1 == r2 -> ()
    | TVarCPS({contents = None} as r), typ
    | typ, TVarCPS({contents = None} as r) ->
        if occurs r typ
        then raise Typing.CannotUnify
        else r := Some typ
    | TFunCPS(b1,typ11,typ12), TFunCPS(b2,typ21,typ22) ->
        let b = max !b1 !b2 in
          b1 := b;
          b2 := b;
          unify typ11 typ21;
          unify typ12 typ22
    | typ1,typ2 -> Format.printf "typ1:@.%a@.typ2:@.%a@." print_typ_cps typ1 print_typ_cps typ2; assert false

let rec infer_cont_pos env = function
    Unit -> {t_cps=UnitCPS; typ_cps=TBaseCPS}
  | True -> {t_cps=TrueCPS; typ_cps=TBaseCPS}
  | False -> {t_cps=FalseCPS; typ_cps=TBaseCPS}
  | Unknown -> {t_cps=UnknownCPS; typ_cps=TBaseCPS}
  | Int n -> {t_cps=IntCPS n; typ_cps=TBaseCPS}
  | NInt x -> {t_cps=NIntCPS{id_cps=x;id_typ=TBaseCPS}; typ_cps=TBaseCPS}
  | Var x ->
      let typ =
        try
          List.assoc x env
        with Not_found -> assert false
      in
        {t_cps=VarCPS{id_cps=x;id_typ=typ}; typ_cps=typ}
  | Fun(x, t1) -> assert false
  | App(t1, ts) ->
      let typed1 = infer_cont_pos env t1 in
      let typeds = List.map (infer_cont_pos env) ts in
      let typ_result = new_tvar () in
      let aux typed (typ,b) = TFunCPS(ref b,typed.typ_cps,typ), false in
      let typ,_ = List.fold_right aux typeds (typ_result,true) in
        unify typed1.typ_cps typ;
        {t_cps=AppCPS(typed1,typeds); typ_cps=typ_result}
  | If(t1, t2, t3) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
      let typed3 = infer_cont_pos env t3 in
        unify typed1.typ_cps TBaseCPS;
        unify typed2.typ_cps typed3.typ_cps;
        {t_cps=IfCPS(typed1,typed2,typed3); typ_cps=typed2.typ_cps}
  | Branch(t1, t2) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
        unify typed1.typ_cps typed2.typ_cps;
        {t_cps=BranchCPS(typed1,typed2); typ_cps=typed1.typ_cps}
  | Let(flag, f, xs, t1, t2) ->
      let typ_f = new_tvar () in
      let f' = {id_cps=f; id_typ=typ_f} in
      let typ_args = List.map (fun _ -> new_tvar ()) xs in
      let xs' = List.map2 (fun x typ -> {id_cps=x; id_typ=typ}) xs typ_args in
      let env2 = (f, typ_f) :: env in
      let env1 = List.combine xs typ_args @@ if flag = Nonrecursive then env else env2 in
      let typed1 = infer_cont_pos env1 t1 in
      let typed2 = infer_cont_pos env2 t2 in
      let b = ref true in
      let aux typ1 typ2 =
        let typ = TFunCPS(ref !b,typ1,typ2) in
          b := false; typ
      in
      let typ = List.fold_right aux typ_args typed1.typ_cps in
        unify typ_f typ;
        {t_cps=LetCPS(flag,f',xs',typed1,typed2); typ_cps=typed2.typ_cps}
  | BinOp(op, t1, t2) ->
      let typed1 = infer_cont_pos env t1 in
      let typed2 = infer_cont_pos env t2 in
        unify typed1.typ_cps TBaseCPS;
        unify typed2.typ_cps TBaseCPS;
        {t_cps=BinOpCPS(op,typed1,typed2); typ_cps=TBaseCPS}
  | Not t ->
      let typed = infer_cont_pos env t in
        unify typed.typ_cps TBaseCPS;
        {t_cps=NotCPS typed; typ_cps=TBaseCPS}
  | Fail ->
      let typ = new_tvar () in
        {t_cps=FailCPS; typ_cps=TFunCPS(ref false,TBaseCPS,typ)}
  | Label(b,t) ->
      let typed = infer_cont_pos env t in
        {t_cps=LabelCPS(b, typed); typ_cps=typed.typ_cps}
  | Event s ->
      {t_cps=EventCPS s; typ_cps=TFunCPS(ref false, TBaseCPS, TBaseCPS)}





let funs = ref []




let rec get_arg_num = function
    TBaseCPS -> 0
  | TVarCPS{contents=None} -> 0
  | TVarCPS{contents=Some typ} -> get_arg_num typ
  | TFunCPS({contents=true},typ1,typ2) -> 1
  | TFunCPS({contents=false},typ1,typ2) -> 1 + get_arg_num typ2

let rec app_typ typ typs =
  match typ,typs with
      TVarCPS{contents=Some typ},_ -> app_typ typ typs
    | TFunCPS(_,_,typ2), _::typs' -> app_typ typ2 typs'
    | _, [] -> typ
    | _ -> assert false

let rec trans2 c {t_cps=t; typ_cps=typ} =
  match t with
      UnitCPS -> c Unit
    | TrueCPS -> c True
    | FalseCPS -> c False
    | IntCPS n -> c (Int n)
    | NIntCPS x -> c (NInt x.id_cps)
    | VarCPS x -> c (Var x.id_cps)
    | FunCPS(x, t) -> assert false
    | AppCPS(t1, ts) ->
        let n = get_arg_num t1.typ_cps in
          if n = List.length ts
          then
            let k = new_var' "k" in
            let r = new_var' "r" in
            let c1 x = app2app x [Var k] in
            let cc = List.fold_right (fun t cc -> fun x -> trans2 (fun y -> cc (app2app x [y])) t) ts c1 in
              funs := k::!funs;
              Let(Nonrecursive, k, [r], c (Var r), trans2 cc t1)
          else
            let ts1,ts2 = take2 ts n in
            let typ' = app_typ t1.typ_cps ts1 in
              trans2 c {t_cps=AppCPS({t_cps=AppCPS(t1,ts1);typ_cps=typ'},ts2); typ_cps=typ}
    | IfCPS(t1, t2, t3) ->
        let k = new_var' "k" in
        let x = new_var' "x" in
        let t2' = trans2 (fun y -> App(Var k, [y])) t2 in
        let t3' = trans2 (fun y -> App(Var k, [y])) t3 in
        let c' y = Let(Nonrecursive, k, [x], c (Var x), If(y, t2', t3')) in
          funs := k::!funs;
          trans2 c' t1
    | LetCPS(flag, f, xs, t1, t2) ->
        if xs = []
        then
          let c' t = subst f.id_cps t (trans2 c t2) in
            trans2 c' t1
        else
          let n = get_arg_num f.id_typ in
            if n = List.length xs
            then
              let k = new_var' "k" in
              let f' = f.id_cps in
              let xs' = List.map (fun x -> x.id_cps) xs in
              let t1' = trans2 (fun y -> App(Var k, [y])) t1 in
              let t2' = trans2 c t2 in
                Let(flag, f', xs'@[k], t1', t2')
            else
              let xs1,xs2 = take2 xs n in
              let typ_g = app_typ f.id_typ xs1 in
              let g = {id_cps=new_var' f.id_cps.name; id_typ=typ_g} in
              let t1' = {t_cps=LetCPS(Nonrecursive, g, xs2, t1, {t_cps=VarCPS g;typ_cps=typ_g}); typ_cps=typ_g} in
                trans2 c {t_cps=LetCPS(flag,f,xs1,t1',t2); typ_cps=typ}
    | BinOpCPS(op, t1, t2) ->
        let c1 t1' t2' = c (BinOp(op, t1', t2')) in
        let c2 y1 = trans2 (fun y2 -> c1 y1 y2) t2 in
          trans2 c2 t1
    | NotCPS t ->
        let c' t1 = c (Not t1) in
          trans2 c' t
    | FailCPS -> c (Fail)
    | UnknownCPS -> c Unknown
    | EventCPS s -> c (Event s)
    | t -> (Format.printf "%a@." print_t_cps t; assert false)
let trans2 = trans2 (fun x -> x)









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
  | App(Event s, ts) -> App(Event s, ts)
  | App _ -> assert false
  | If(t1, t2, t3) ->
      let t2' = inlining funs defs t2 in
      let t3' = inlining funs defs t3 in
        If(t1, t2', t3')
  | Branch(t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let t1' = inlining funs defs t1 in
        let t2' = inlining funs ((f,(xs,t1'))::defs) t2 in
          Let(flag, f, xs, t1', t2')
      else
        let t1' = inlining funs defs t1 in
        let t2' = inlining funs defs t2 in
          Let(flag, f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = inlining funs defs t1 in
      let t2' = inlining funs defs t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = inlining funs defs t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s
    

(*
let rec match_arg arg_num b = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(Var f, ts) when b ->
      begin
        try
          let m = List.length ts in
          let n = List.assoc f arg_num in
            if n < m
            then
              let rec take xs n =
                match xs,n with
                    _,0 -> [], xs
                  | [],_ -> [], []
                  | x::xs',_ ->
                      let xs1,xs2 = take xs' (n-1) in
                        x::xs1, xs2
              in
              let ts1,ts2 = take ts n in
                match_arg arg_num true (App(App(Var f, ts1), ts2))
            else
              let ts' = List.map (match_arg arg_num true) ts in
              let args = tabulate (n-m) (fun _ -> new_var' "x") in
                begin
                  match args with
                      [] -> App(Var f, ts')
                    | _ ->
                      let g = new_var' "f" in
                      let ts'' = List.map (fun x -> Var x) args in
                        Let(Nonrecursive, g, args, App(Var f, ts'@ts''), Var g)
                end
        with Not_found ->
          match_arg arg_num false (App(Var f,ts))
      end
  | App(f, ts) ->
      let f' = match_arg arg_num true f in
      let ts' = List.map (match_arg arg_num true) ts in
      let typ = Typing.get_typ (App(f, ts)) in
      let args = get_args typ in
        begin
        match args with
            [] -> App(f', ts')
          | _ ->
            let g = new_var' "f" in
            let ts'' = List.map (fun x -> Var x) args in
              Let(Nonrecursive, g, args, App(f', ts'@ts''), Var g)
        end
  | If(t1, t2, t3) ->
      let t1' = match_arg arg_num true t1 in
      let t2' = match_arg arg_num true t2 in
      let t3' = match_arg arg_num true t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = match_arg arg_num true t1 in
      let t2' = match_arg arg_num true t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let arg_num' = (f,List.length xs)::arg_num in
        let t1' = match_arg arg_num true t1 in
        let t2' = match_arg arg_num' true t2 in
          Let(flag, f, xs, t1', t2')
      else
        let arg_num' = (f,List.length xs)::arg_num in
        let t1' = match_arg arg_num' true t1 in
        let t2' = match_arg arg_num' true t2 in
          Let(flag, f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = match_arg arg_num true t1 in
      let t2' = match_arg arg_num true t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = match_arg arg_num true t in
        Not t'
  | Fail -> Fail
  | Label(b,t) ->
      let t' = match_arg arg_num true t in
        Label(b, t')
  | Event s -> Event s
*)


(*
let rec app2letapp = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(f, ts) ->
      let xs = tabulate (List.length ts) (fun _ -> new_var' "x") in
        List.fold_right2 (fun x t t' -> Let(Nonrecursive,x,[],t,t')) xs ts (App(f, List.map (fun x -> Var x) xs))
  | If(t1, t2, t3) ->
      let t1' = app2letapp t1 in
      let t2' = app2letapp t2 in
      let t3' = app2letapp t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = app2letapp t1 in
      let t2' = app2letapp t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      let t1' = app2letapp t1 in
      let t2' = app2letapp t2 in
        Let(flag, f, xs, t1', t2')
(*
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f,xs,app2letapp t) bindings in
      let t' = app2letapp t in
        Let(flag, bindings', t')
*)
  | BinOp(op, t1, t2) ->
      let t1' = app2letapp t1 in
      let t2' = app2letapp t2 in
        BinOp(op, t1', t2')
  | Not t -> Not (app2letapp t)
  | Fail -> Fail
  | Label(b,t) -> Label(b,app2letapp t)
  | Event s -> Event s
*)

let rec normalize = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(Fail, [t1;t2]) -> App(Fail, [normalize t1])
  | App(Fail, _) -> assert false
  | App(Event s, [t1;t2]) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        App(Event s, [App(t2', [t1'])])
  | App(Event s, _) -> assert false
  | App(f, ts) ->
      let ts' = List.map normalize ts in
      let f' = normalize f in
        App(f', ts')
  | If(t1, t2, t3) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
      let t3' = normalize t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        Let(flag, f, xs, t1', t2')
(*
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f,xs,normalize t) bindings in
      let t' = normalize t in
        Let(flag, bindings', t')
*)
  | BinOp(op, t1, t2) ->
      let t1' = normalize t1 in
      let t2' = normalize t2 in
        BinOp(op, t1', t2')
  | Not t -> Not (normalize t)
  | Fail -> Fail
  | Label(b,t) -> Label(b,normalize t)
  | Event s -> assert false





(* ASSUME:
 *  - t have a type
 *  - variables must be renamed
 *)
let trans t =
  let cps_pre = infer_cont_pos [] t in
  let cps = trans2 cps_pre in
  let () = Typing.type_checking cps in
  let normalized = normalize cps in
  let typed = Typing.typing normalized in
  let inlined = inlining !funs [] typed in
(*
  let removed = remove_unused inlined in
*)
    part_eval inlined


