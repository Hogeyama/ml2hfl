open Util
open Ref_type

module S = Syntax
module U = Term_util
module T = Type

module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)

let add_comment s t =
  if !!Debug.check then
    U.add_comment s t
  else
    t

let rec generate_check typ_exn make_fail genv cenv x typ =
  Debug.printf "Ref_type_gen.generate_check: %a : %a@." Id.print x print typ;
  match typ with
  | Base(base, y, p) ->
      genv, cenv, U.subst_var y x p
  | Fun(y, typ1, Exn(typ2, typ3)) ->
      begin
        match typ_exn with
        | None ->
            generate_check typ_exn make_fail genv cenv x (Fun(y, typ1, typ2))
        | Some typ_exn' ->
            let genv',cenv',t_typ1 = generate typ_exn make_fail genv cenv typ1 in
            let z = U.new_var_of_term t_typ1 in
            let e = Id.new_var ~name:"e" typ_exn' in
            let typ2' = subst_var y z typ2 in
            let typ3' = subst_var y z typ3 in
            let r = Id.new_var ~name:"r" @@ to_simple typ2 in
            let genv'',cenv'',t_typ2' = generate_check typ_exn make_fail genv' cenv' r typ2' in
            let genv''',cenv''',t_typ3' = generate_check typ_exn make_fail genv' cenv' e typ3' in
            let t' = U.make_let [r, [], U.make_app (U.make_var x) [U.make_var z]] @@ t_typ2' in
            genv''', cenv''', U.make_let [z,[],t_typ1] @@ U.make_trywith_simple t' @@ U.make_fun e t_typ3'
      end
  | Fun(y,typ1,typ2) ->
      let genv',cenv',t_typ1 = generate typ_exn make_fail genv cenv typ1 in
      let z = Id.new_var t_typ1.S.typ in
      let t_typ2 = U.make_app (U.make_var x) [U.make_var z] in
      let r = Id.new_var ~name:"r" t_typ2.S.typ in
      let genv'',cenv'',t_typ2' =
        let typ2' = subst_var y z typ2 in
        generate_check typ_exn make_fail genv' cenv' r typ2'
      in
      genv'', cenv'', U.make_lets [z,[],t_typ1; r,[],t_typ2] t_typ2'
  | Tuple xtyps ->
      let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
      let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
      let genv',cenv',ts =
        let aux x' typ (genv,cenv,ts) =
          let genv',cenv',t = generate_check typ_exn make_fail genv cenv x' typ in
          genv', cenv', t::ts
        in
        List.fold_right2 aux xs' typs (genv,cenv,[])
      in
      genv', cenv', U.make_lets (List.mapi (fun i x' -> x', [], U.make_proj i @@ U.make_var x) xs') @@ U.make_ands ts
  | List(l,p_len,y,p_i,typ1) when p_i.S.desc = S.Const S.True && not @@ occur y typ1 ->
      let styp = to_simple typ in
      let atyp1 = to_abst_typ typ1 in
      let atyp = to_abst_typ typ in
      let l' = Id.new_var ~name:"l" T.TInt in
      let add_len t =
        let t' = U.make_and t @@ U.subst_var l l' p_len in
        if t' = U.true_term
        then U.true_term
        else U.make_let [l',[],U.make_length @@ U.make_var x] t'
      in
      let typ1' = subst_var l l' typ1 in
      let genv',cenv',t =
        if List.mem_assoc ~eq:same typ cenv
        then
          let f,_,_ = List.assoc ~eq:same typ cenv in
          genv, cenv, U.make_app (U.make_var f) [U.make_var x]
        else
          let zs = Id.new_var ~name:"xs" atyp in
          let f = Id.new_var ~name:("check_" ^ T.to_id_string styp) @@ T.TFun(zs,T.TBool) in
          let z = Id.new_var ~name:"x" atyp1 in
          let zs' = Id.new_var ~name:"xs'" atyp in
          let genv',cenv',t_b1 = generate_check typ_exn make_fail genv cenv z typ1' in
          if t_b1 = U.true_term
          then
            genv', cenv', U.true_term
          else
            let t_body =
              let pat_nil = U.make_pnil styp, U.true_term, U.true_term in
              let pat_cons =
                let t_b2 = U.make_app (U.make_var f) [U.make_var zs'] in
                U.make_pcons (U.make_pvar z) (U.make_pvar zs'), U.true_term, U.make_and t_b1 t_b2
              in
              U.make_match (U.make_var zs) [pat_nil; pat_cons]
            in
            let def = f, [zs], add_comment (Format.asprintf "CHECK: %a" print typ) t_body in
            Debug.printf "CHECK: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
            let t = U.make_app (U.make_var f) [U.make_var x] in
            if List.Set.supset ~eq:Id.eq [zs;U.make_length_var T.typ_unknown;f] @@ U.get_fv t_body
            then genv'@[typ,def], cenv', t
            else genv', cenv', U.make_let [def] t
      in
      genv', cenv', add_len t
  | Inter(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check typ_exn make_fail genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      Debug.printf "generate_check typ: %a@." (List.print print) typs;
      Debug.printf "generate_check ts: %a@." (List.print  Print.term) ts;
      genv'', cenv'', U.make_ands ts
  | Union(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check typ_exn make_fail genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      genv'', cenv'', U.make_ors ts
  | Exn(typ1, typ2) when is_bottom' typ2 -> generate_check typ_exn make_fail genv cenv x typ1
  | Exn _ -> Format.printf "typ: %a@." print typ; unsupported "Ref_type_gen.generate_check Exn"
  | ExtArg _ -> Format.printf "typ: %a@." print typ; unsupported "Ref_type_gen.generate_check ExtArg"
  | List _ -> Format.printf "typ: %a@." print typ; unsupported "Ref_type_gen.generate_check List"

and generate typ_exn make_fail genv cenv typ =
  Debug.printf "Ref_type_gen.generate: %a@." print typ;
  let genv',cenv',t =
    match typ with
    | Base(Int, x, p) ->
        let x' = Id.new_var T.TInt in
        let genv',cenv',t_check = generate_check typ_exn make_fail genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randint_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Bool, x, p) ->
        let x' = Id.new_var T.TBool in
        let genv',cenv',t_check = generate_check typ_exn make_fail genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randbool_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Unit, x, p) ->
        let genv',cenv',t_check = generate_check typ_exn make_fail genv cenv x typ in
        genv', cenv', U.make_assume t_check U.unit_term
    | Base(Abst s, x, p) ->
        let typ' = to_simple typ in
        let x' = Id.new_var typ' in
        let genv',cenv',t_check = generate_check typ_exn make_fail genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.make_randvalue_unit typ'] @@ U.make_assume t_check @@ U.make_var x'
    | Fun(x,typ1,typ2) ->
        let x' = Id.new_var @@ to_abst_typ typ1 in
        let typ2' = subst_var x x' typ2 in
        let genv',cenv',t_typ1 = generate_check typ_exn make_fail genv cenv x' typ1 in
        Debug.printf "Ref_type_gen.generate t_typ1: %a@." Print.term t_typ1;
        let t1 = U.make_or U.randbool_unit_term t_typ1 in
        let genv'',cenv'',t2 = generate typ_exn make_fail genv' cenv' typ2' in
        let t3 = make_fail @@ to_simple typ2' in
        genv'', cenv'', U.make_fun x' @@ add_comment (Format.asprintf "GEN FUN: %a" print typ2) @@ U.make_if t1 t2 t3
    | Tuple xtyps ->
        let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
        let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
        let genv',cenv',ts =
          let aux typ (genv,cenv,ts) =
            let genv',cenv',t = generate typ_exn make_fail genv cenv typ in
            genv',cenv',t::ts
          in
          List.fold_right aux typs (genv,cenv,[])
        in
        genv', cenv', U.make_lets (List.map2 (fun x t -> x,[],t) xs' ts) @@ U.make_tuple @@ List.map U.make_var xs'
    | Inter(styp, []) -> genv, cenv, make_fail styp
    | Inter(_, [typ]) -> generate typ_exn make_fail genv cenv typ
    | Inter(_, Base(base,x,p)::typs) ->
        let p' =
          let aux p typ =
            match typ with
            | Base(base', x', p') ->
                assert (base = base');
                U.make_and p (U.subst_var x' x p')
            | _ -> assert false
          in
          List.fold_left aux p typs
        in
        generate typ_exn make_fail genv cenv @@ Base(base, x, p')
    | Inter(styp, ((Fun(_,atyp,rtyp))::_ as typs)) when !Flag.Modular.use_ref_typ_gen_method_in_esop2017 ->
        Flag.Method.fail_as_exception := true;
        let rec wrap typ (genv,cenv,v) =
          match typ with
          | Base(base,x,p) ->
              genv, cenv, U.make_assume (U.subst x v p) v
          | Fun(x,typ1,typ2) ->
              let f = U.new_var_of_term v in
              let genv',cenv',t_check = generate_check typ_exn make_fail genv cenv x typ1 in
              let t_app = U.make_app (U.make_var f) [U.make_var x] in
              let r = U.new_var_of_term t_app in
              let genv'',cenv'',t_wrap = wrap typ2 (genv', cenv', U.make_var r) in
              let genv''',cenv''',t_gen = generate typ_exn make_fail genv'' cenv'' typ2 in
              let handler = U.make_fun (Id.new_var ~name:"u" @@ Option.get typ_exn) t_gen in
              genv''',
              cenv''',
              U.make_fun x @@
                U.make_let [f,[],v] @@
                  U.make_if
                    (U.make_or U.randbool_unit_term t_check)
                    (U.make_trywith_simple (U.make_let [r,[],t_app] t_wrap) handler)
                    t_app
          | Tuple xtyps ->
              let xttyps =
                let aux i (x,typ) =
                  let t = U.make_proj i v in
                  let x' = Id.set_typ x t.S.typ in
                  x', t, typ
                in
                List.mapi aux xtyps
              in
              let genv',cenv',ts =
                let aux (x,_,typ) (genv,cenv,ts) =
                  let genv',cenv',t = wrap typ (genv, cenv, U.make_var x) in
                  genv',cenv',t::ts
                in
                List.fold_right aux xttyps (genv,cenv,[])
              in
              let t =
                List.fold_right (fun (x,t,_) -> U.make_let [x,[],t]) xttyps (U.make_tuple ts)
              in
              genv', cenv', t
          | Inter(styp,typs) ->
              List.fold_right wrap typs (genv,cenv,v)
          | Exn(typ1,typ2) ->
              let genv',cenv',t1 = wrap typ1 (genv,cenv,v) in
              let e = Id.new_var ~name:"e" @@ Option.get typ_exn in
              let genv'',cenv'',t2 = wrap typ2 (genv',cenv',U.make_var e) in
              let handler = U.make_fun e t2 in
              genv'', cenv'', U.make_trywith_simple t1 handler
          | _ ->
              Format.printf "%a@." print typ;
              unsupported "Ref_type_gen.wrap"
        in
        let v0 =
          let arg_styp = to_simple atyp in
          let ret_styp = to_simple rtyp in
          U.make_fun (Id.new_var ~name:"u" arg_styp) @@ make_fail ret_styp
        in
        Format.printf "typs: %a@." (List.print print) typs;
        List.fold_right wrap typs (genv,cenv,v0)
    | Inter(_, ((Fun _)::_ as typs)) ->
        Flag.Method.fail_as_exception := true;
        let bss = Combination.take_each @@ List.map (Fun.const [true;false]) typs in
        Debug.printf "GEN bss: %a@." (List.print @@ List.print Format.pp_print_bool) bss;
        let x =
          match typs with
          | Fun(_,typ1,_)::_ -> Id.new_var @@ to_abst_typ typ1
          | _ -> assert false
        in
        let typs1,typs2 = List.split_map (function Fun(y,typ1,typ2) -> typ1, subst_var y x typ2 | _ -> assert false) typs in
        Debug.printf "GEN typs1: %a@." (List.print print) typs1;
        Debug.printf "GEN typs2: %a@." (List.print print) typs2;
        let xs = List.map (fun _ -> Id.new_var T.TBool) typs in
        Debug.printf "GEN xs: %a@." (List.print Id.print) xs;
        let genv',cenv',tbs =
          let aux typ1 (genv,cenv,tbs) =
            let genv', cenv', tb = generate_check typ_exn make_fail genv cenv x typ1 in
            let tb' =
              let e = Id.new_var ~name:"e" @@ Option.default U.typ_exn typ_exn in
              U.make_trywith tb e [U.make_pany @@ Id.typ e, U.true_term, U.false_term]
              |> U.make_or U.randbool_unit_term
              |*> add_comment @@ Format.asprintf "GEN INTER: beta(%a)" print typ1
            in
            genv', cenv', tb'::tbs
          in
          List.fold_right aux typs1 (genv,cenv,[])
        in
        Debug.printf "GEN tbs: %a@." (List.print Print.term) tbs;
        let tcs =
          let aux bs =
            xs
            |> List.map U.make_var
            |> List.filter_map2 Option.some_if bs
            |> U.make_ands
          in
          List.map aux bss
        in
        Debug.printf "GEN tcs: %a@." (List.print Print.term) tcs;
        let rstyp = to_simple @@ List.hd typs2 in
        let genv'',cenv'',trs =
          let aux bs (genv,cenv,trs) =
            let typ =
              typs2
              |> List.filter_map2 Option.some_if bs
              |> inter rstyp
            in
            Debug.printf "GEN typ: %a@." print typ;
            let genv',cenv',tr = generate typ_exn make_fail genv cenv typ in
            genv', cenv', tr::trs
          in
          List.fold_right aux bss (genv',cenv',[])
        in
        Debug.printf "GEN trs: %a@." (List.print Print.term) trs;
        let t =
          U.make_bottom rstyp
          |> List.fold_right2 U.make_if tcs trs
          |> U.make_lets @@ List.map2 (fun x tb -> x, [], tb) xs tbs
          |> U.make_fun x
        in
        Debug.printf "GEN t: %a@."  Print.term t;
        genv'', cenv'', t
    | Inter(_, ((Exn _)::_ as typs)) ->
        let typs1,typs2 = List.split_map (function Exn(typ1,typ2) -> typ1,typ2 | _ -> assert false) typs in
        let inter' typs = inter (to_simple @@ List.hd typs) typs in
        let typ' = Exn(inter' typs1, inter' typs2) in
        generate typ_exn make_fail genv cenv typ'
    | Inter(_, _) ->
        Format.printf "INTER: %a@." print typ;
        unsupported "Ref_type_gen.generate: Inter"
    | Union(styp, []) -> [], [], U.make_bottom styp
    | Union(_, [typ]) -> generate typ_exn make_fail genv cenv typ
    | Union(_, ((Tuple _)::_ as typs)) ->
        let genv',cenv',ts =
          let aux typ (genv,cenv,ts) =
            let genv',cenv',t = generate typ_exn make_fail genv cenv typ in
            genv',cenv',t::ts
          in
          List.fold_right aux typs (genv,cenv,[])
        in
        genv', cenv', List.fold_left U.make_br (List.hd ts) (List.tl ts)
    | Union(_, typs) -> unsupported "Ref_type_gen.generate: Union"
    | ExtArg(x,typ1,typ2) -> unsupported "Ref_type_gen.generate: ExtArg"
    | List(x,p_len,y,p_i,typ') ->
        if p_i.S.desc <> S.Const S.True || occur y typ' then
          unsupported "Ref_type_gen.generate"
        else
          let styp = to_simple typ in
          let l = Id.new_var ~name:"l" T.TInt in
          let p_len' = U.subst_var x l p_len in
          let genv',cenv',t =
            if List.mem_assoc ~eq:same typ genv
            then
              let f,_,_ = List.assoc ~eq:same typ genv in
              let t = U.make_app (U.make_var f) [U.make_var l] in
              genv, cenv, t
            else
              let n = Id.new_var T.TInt in
              let f = Id.new_var ~name:("make_r_" ^ T.to_id_string styp) @@ T.TFun(n, to_abst_typ typ) in
              let t_nil = U.make_nil2 styp in
              let genv',cenv',t_typ' = generate typ_exn make_fail genv cenv typ' in
              let t_cons = U.make_cons t_typ' @@ U.make_app (U.make_var f) [U.make_sub (U.make_var n) (U.make_int 1)] in
              let t_b = U.make_leq (U.make_var n) (U.make_int 0) in
              let def = f, [n], add_comment (Format.asprintf "GEN LIST: %a" print typ) @@ U.make_if t_b t_nil t_cons in
              let t = U.make_app (U.make_var f) [U.make_var l] in
              Debug.printf "GEN: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
              if List.Set.supset ~eq:Id.eq [n] @@ U.get_fv @@ Triple.trd def
              then genv'@[typ,def], cenv', t
              else genv', cenv', U.make_let [def] t
          in
          genv', cenv', U.make_let [l,[],U.randint_unit_term] @@ U.make_assume p_len' t
    | Exn(typ1,typ2) ->
        let genv',cenv',t_typ1 = generate typ_exn make_fail genv cenv typ1 in
        let genv'',cenv'',t_typ2 = generate typ_exn make_fail genv' cenv' typ2 in
        genv'', cenv'', U.make_br t_typ1 @@ U.make_raise t_typ2 t_typ1.S.typ
  in
  Debug.printf "Ref_type_gen.generate': %a@." print typ;
  genv', cenv', {t with S.typ = to_abst_typ typ}

let generate_check typ_exn ?(make_fail=U.make_fail) genv cenv x typ = generate_check typ_exn make_fail genv cenv x typ
let generate typ_exn ?(make_fail=U.make_fail) genv cenv typ = generate typ_exn make_fail genv cenv typ
