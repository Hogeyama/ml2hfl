open Util
open Ref_type

module S = Syntax
module U = Term_util
module T = Type

let debug () = List.mem "Ref_type" !Flag.debug_module

let rec generate_check genv cenv x typ =
  if !!debug then Format.printf "Ref_type.generate_check: %a : %a@." Id.print x print typ;
  match typ with
  | Base(base, y, p) ->
      genv, cenv, U.subst_var y x p
  | Fun(y,typ1,typ2) ->
      let genv',cenv',t_typ1 = generate genv cenv typ1 in
      let z = Id.new_var t_typ1.S.typ in
      let t_typ2 = U.make_app (U.make_var x) [U.make_var z] in
      let r = Id.new_var ~name:"r" t_typ2.S.typ in
      let genv'',cenv'',t_typ2' =
        let typ2' = subst_var y z typ2 in
        generate_check genv' cenv' r typ2'
      in
      genv'', cenv'', U.make_lets [z,[],t_typ1; r,[],t_typ2] t_typ2'
  | Tuple xtyps ->
      let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
      let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
      let genv',cenv',ts = List.fold_right2 (fun x' typ (genv,cenv,ts) -> let genv',cenv',t = generate_check genv cenv x' typ in genv',cenv',t::ts) xs' typs (genv,cenv,[]) in
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
          let genv',cenv',t_b1 = generate_check genv cenv z typ1' in
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
            let def = f, [zs], U.add_comment (Format.asprintf "CHECK: %a" print typ) t_body in
            if debug() then Format.printf "CHECK: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
            let t = U.make_app (U.make_var f) [U.make_var x] in
            if List.Set.supset ~eq:Id.eq [zs;U.length_var;f] @@ U.get_fv t_body
            then genv'@[typ,def], cenv', t
            else genv', cenv', U.make_letrec [def] t
      in
      genv', cenv', add_len t
  | Inter(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      if !!debug then Format.printf "generate_check typ: %a@." (List.print print) typs;
      if !!debug then Format.printf "generate_check ts: %a@." (List.print  Print.term) ts;
      genv'', cenv'', U.make_ands ts
  | Union(_, typs) ->
      let aux (genv',cenv',ts) typ =
        let genv'',cenv'',t = generate_check genv' cenv' x typ in
        genv'', cenv'', ts@[t]
      in
      let genv'',cenv'',ts = List.fold_left aux (genv,cenv,[]) typs in
      genv'', cenv'', U.make_ors ts
  | _ -> Format.printf "%a@." print typ; unsupported "Ref_type.generate_check"
and generate_simple typ = U.make_fail typ

and generate genv cenv typ =
  if !!debug then Format.printf "Ref_type.generate: %a@." print typ;
  let genv',cenv',t =
    match typ with
    | Base(Int, x, p) ->
        let x' = Id.new_var T.TInt in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randint_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Bool, x, p) ->
        let x' = Id.new_var T.TBool in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.randbool_unit_term] @@ U.make_assume t_check @@ U.make_var x'
    | Base(Unit, x, p) ->
        let genv',cenv',t_check = generate_check genv cenv x typ in
        genv', cenv', U.make_assume t_check U.unit_term
    | Base(Abst s, x, p) ->
        let typ' = to_simple typ in
        let x' = Id.new_var typ' in
        let genv',cenv',t_check = generate_check genv cenv x' typ in
        genv', cenv', U.make_let [x',[],U.make_randvalue_unit typ'] @@ U.make_assume t_check @@ U.make_var x'
    | Fun(x,typ1,typ2) ->
        let x' = Id.new_var @@ to_abst_typ typ1 in
        let typ2' = subst_var x x' typ2 in
        let genv',cenv',t_typ1 = generate_check genv cenv x' typ1 in
        if !!debug then Format.printf "Ref_type.generate t_typ1: %a@." Print.term t_typ1;
        let t1 = U.make_or U.randbool_unit_term t_typ1 in
        let genv'',cenv'',t2 = generate genv' cenv' typ2' in
        let t3 = generate_simple @@ to_simple typ2' in
        genv'', cenv'', U.make_fun x' @@ U.add_comment (Format.asprintf "GEN FUN: %a" print typ2) @@ U.make_if t1 t2 t3
    | Tuple xtyps ->
        let xs' = List.map (fun (x,typ) -> Id.new_var ~name:(Id.name x) @@ to_simple typ) xtyps  in
        let typs = List.fold_right2 (fun x' (x,typ) acc -> typ :: List.map (subst_var x x') acc) xs' xtyps [] in
        let genv',cenv',ts = List.fold_right (fun typ (genv,cenv,ts) -> let genv',cenv',t = generate genv cenv typ in genv',cenv',t::ts) typs (genv,cenv,[]) in
        genv', cenv', U.make_lets (List.map2 (fun x t -> x,[],t) xs' ts) @@ U.make_tuple @@ List.map U.make_var xs'
    | Inter(styp, []) -> generate genv cenv @@ make_weakest styp
    | Inter(_, [typ]) -> generate genv cenv typ
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
        generate genv cenv @@ Base(base, x, p')
    | Inter(_, ((Fun _)::_ as typs)) ->
        Flag.fail_as_exception := true;
        let bss = Combination.take_each @@ List.map (Fun.const [true;false]) typs in
        if !!debug then Format.printf "GEN bss: %a@." (List.print @@ List.print Format.pp_print_bool) bss;
        let x =
          match typs with
          | Fun(_,typ1,_)::_ -> Id.new_var @@ to_abst_typ typ1
          | _ -> assert false
        in
        let typs1,typs2 = List.split_map (function Fun(y,typ1,typ2) -> typ1, subst_var y x typ2 | _ -> assert false) typs in
        if !!debug then Format.printf "GEN typs1: %a@." (List.print print) typs1;
        if !!debug then Format.printf "GEN typs2: %a@." (List.print print) typs2;
        let xs = List.map (fun _ -> Id.new_var ~name:"b" T.TBool) typs in
        if !!debug then Format.printf "GEN xs: %a@." (List.print Id.print) xs;
        let genv',cenv',tbs =
          let aux typ1 (genv,cenv,tbs) =
            let genv', cenv', tb = generate_check genv cenv x typ1 in
            let tb' =
              let e = Id.new_var ~name:"e" U.typ_exn in
              U.make_trywith tb e [U.make_pany @@ Id.typ e, U.true_term, U.false_term]
              |> U.make_or U.randbool_unit_term
              |*> U.add_comment @@ Format.asprintf "GEN INTER: beta(%a)" print typ1
            in
            genv', cenv', tb'::tbs
          in
          List.fold_right aux typs1 (genv,cenv,[])
        in
        if !!debug then Format.printf "GEN tbs: %a@." (List.print Print.term) tbs;
        let tcs =
          let aux bs =
            xs
            |> List.map U.make_var
            |> List.filter_map2 Option.some_if bs
            |> U.make_ands
          in
          List.map aux bss
        in
        if !!debug then Format.printf "GEN tcs: %a@." (List.print Print.term) tcs;
        let rstyp = to_simple @@ List.hd typs2 in
        let genv'',cenv'',trs =
          let aux bs (genv,cenv,trs) =
            let typ =
              typs2
              |> List.filter_map2 Option.some_if bs
              |> inter rstyp
            in
            if !!debug then Format.printf "GEN typ: %a@." print typ;
            let genv',cenv',tr = generate genv cenv typ in
            genv', cenv', tr::trs
          in
          List.fold_right aux bss (genv',cenv',[])
        in
        if !!debug then Format.printf "GEN trs: %a@." (List.print Print.term) trs;
        let t =
          U.make_bottom rstyp
          |> List.fold_right2 U.make_if tcs trs
          |> U.make_lets @@ List.map2 (fun x tb -> x, [], tb) xs tbs
          |> U.make_fun x
        in
        if !!debug then Format.printf "GEN t: %a@."  Print.term t;
        genv'', cenv'', t
    | Inter(_, _) ->
        Format.printf "INTER: %a@." print typ;
        unsupported "Ref_type.generate: Inter"
    | Union(styp, []) -> [], [], U.make_bottom styp
    | Union(_, [typ]) -> generate genv cenv typ
    | Union(_, typs) -> unsupported "Ref_type.generate: Union"
    | ExtArg(x,typ1,typ2) -> unsupported "Ref_type.generate: ExtArg"
    | List(x,p_len,y,p_i,typ') ->
        if p_i.S.desc <> S.Const S.True || occur y typ' then
          unsupported "Ref_type.generate"
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
              let n = Id.new_var ~name:"n" T.TInt in
              let f = Id.new_var ~name:("make_r_" ^ T.to_id_string styp) @@ T.TFun(n, to_abst_typ typ) in
              let t_nil = U.make_nil2 styp in
              let genv',cenv',t_typ' = generate genv cenv typ' in
              let t_cons = U.make_cons t_typ' @@ U.make_app (U.make_var f) [U.make_sub (U.make_var n) (U.make_int 1)] in
              let t_b = U.make_leq (U.make_var n) (U.make_int 0) in
              let def = f, [n], U.add_comment (Format.asprintf "GEN LIST: %a" print typ) @@ U.make_if t_b t_nil t_cons in
              let t = U.make_app (U.make_var f) [U.make_var l] in
              if debug() then Format.printf "GEN: %a: %a@." print typ (Triple.print Print.id (List.print Print.id) Print.term) def;
              if List.Set.supset ~eq:Id.eq [n] @@ U.get_fv @@ Triple.trd def
              then genv'@[typ,def], cenv', t
              else genv', cenv', U.make_letrec [def] t
          in
          genv', cenv', U.make_let [l,[],U.randint_unit_term] @@ U.make_assume p_len' t
  in
  if !!debug then Format.printf "Ref_type.generate': %a@." print typ;
  genv', cenv', {t with S.typ = to_abst_typ typ}






(*
let rec generate_check_cps x typ tk th =
  let k = Id.new_var ~name:"k" @@ T.make_tfun T.TBool U.typ_result in
  let h = Id.new_var ~name:"h" @@ T.make_tfun T.TUnit U.typ_result in
  let tk = U.make_var k in
  let th = U.make_var h in
  let t =
    match typ with
    | Base(base, y, p) ->
        U.make_app tk [U.subst_var y x p]
    | Fun(y, Base(_,x,p), typ2) ->
        let t_typ1 k =
          match typ1 with
          |  -> U.make_app U.make_randvalue_cps [unit_term; U.make_fun r @@ U.make_assume (subst x r p) @@ U.make_app (make_var x) []]
          | Inter _ -> unsupported "generate_check_cps"
          | Union _ -> unsupported "generate_check_cps"
          | _ -> assert false
        in
        let z = Id.new_var t_typ1.S.typ in
        let r =
          match to_simple typ2 with
          | T.TFun(r, _) -> Id.new_var_id r
          | _ -> assert false
        in
        let t_typ2 = generate_check_cps r @@ subst_var y z typ2 in
        U.make_app
          t_typ1
          [U.make_fun z @@
             U.make_app
               (U.make_var x)
               [U.make_var z;
                U.make_fun r @@ U.make_app t_typ2 [tk; th]]]
    | Fun(y,typ1,typ2) ->
        let t_typ1 = generate_cps typ1 in
        let z = Id.new_var t_typ1.S.typ in
        let r =
          match to_simple typ2 with
          | T.TFun(r, _) -> Id.new_var_id r
          | _ -> assert false
        in
        let t_typ2 = generate_check_cps r @@ subst_var y z typ2 in
        U.make_app
          t_typ1
          [U.make_fun z @@
             U.make_app
               (U.make_var x)
               [U.make_var z;
                U.make_fun r @@ U.make_app t_typ2 [tk; th]]]
    | Tuple _ -> unsupported "Ref_type_gen.generate_check_cps: Tuple"
    | List _ -> unsupported "Ref_type_gen.generate_check_cps: List"
    | Inter(_, typs) ->
        let aux typ t =
          let t_typ = generate_check_cps x typ in
          let r = Id.new_var ~name:"b" T.TBool in
          U.make_app
            t_typ
            [U.make_fun r @@ U.make_if (U.make_var r) t (U.make_app tk [U.false_term]);
             th]
        in
        let t0 = U.make_app tk [U.true_term] in
        List.fold_right aux typs t0
    | Union(_, typs) ->
        let aux typ t =
          let t_typ = generate_check_cps x typ in
          let r = Id.new_var ~name:"b" T.TBool in
          U.make_app
            t_typ
            [U.make_fun r @@ U.make_if (U.make_var r) (U.make_app tk [U.true_term]) t;
             th]
        in
        let t0 = U.make_app tk [U.false_term] in
        List.fold_right aux typs t0
    | _ -> Format.printf "%a@." print typ; unsupported "Ref_type_gen.generate_check"
  in
  U.make_fun k @@ U.make_fun h t

and generate_cps typ =
  if !!debug then Format.printf "Ref_type.generate: %a@." print typ;
  match typ with
  | Base(_, x, p) -> assert (p.desc = Const.True); unit_term
  | Fun(x,typ1,typ2) ->
      let x' = Id.new_var_id x in
      let typ2' = subst_var x x' typ2 in
      let t_typ1 = generate_check_cps x' typ1 in
      let t_typ2' = generate_cps typ2' in
      let b = Id.new_var ~name:"b" T.TBool in
      let k' = Id.new_var ~name:"k" @@ to_simple typ2' in
      let t1 = U.make_or U.randbool_unit_term @@ U.make_var b in
      let t2 = U.make_app t_typ2' [U.make_var k'; th] in
      let t3 = generate_simple @@ to_simple typ2' in
      U.make_fun x' @@ U.make_app t_typ1 [U.make_if t1 t2 t3]
  | Tuple _ -> unsupported "Ref_type_gen.generate_check_cps: Tuple"
  | Inter(styp, []) ->
      assert false;
      generate_cps @@ make_weakest styp
  | Inter(_, [typ]) ->
      assert false;
      generate_cps typ
  | Inter(_, (Base(base,_,_)::_ as typs)) ->
      let typ' = to_simple typ in
      let x' = Id.new_var typ' in
      let p' =
        let aux p typ =
          match typ with
          | Base(base', x, p) ->
              assert (base = base');
              U.make_and p (U.subst_var x x' p)
          | _ -> assert false
        in
        List.fold_left aux U.true_term typs
      in
      U.make_app (U.make_randvalue typ') [U.make_fun x' @@ U.make_assume p' @@ U.make_app tk [U.make_var x']]
  | Inter(_, ((Fun _)::_ as typs)) ->
      assert false;
      let x =
        match typs with
        | Fun(_,typ1,_)::_ -> Id.new_var @@ to_abst_typ typ1
        | _ -> assert false
      in
      let typs1,typs2 = List.split_map (function Fun(y,typ1,typ2) -> typ1, subst_var y x typ2 | _ -> assert false) typs in
      let xs = List.map (fun _ -> Id.new_var ~name:"b" T.TBool) typs in
      let ts =
        let aux typ1 tbs =
          let tb = generate_check_cps x typ1 in
          let tb' =
            let e = Id.new_var ~name:"e" U.typ_exn in
            U.make_trywith tb e [U.make_pany @@ Id.typ e, U.true_term, U.false_term]
            |> U.make_or U.randbool_unit_term
          in
          tb'::tbs
        in
        List.fold_right aux typs1 []
      in
      let rstyp = to_simple @@ List.hd typs2 in
      let rec aux acc ttyps =
        match ttyps with
        | [] ->
            let t_typ = generate_cps @@ inter rstyp acc in
            U.make_app t_typ [tk; th]
        | (t_typ1,typ1,typ2)::ttyps' ->
            let b = Id.new_var ~name:"b" T.TBool in
            let u2 = Id.new_var ~name:"u" T.TUnit in
            let u3 = Id.new_var ~name:"u" T.TUnit in
            let t2 = U.make_fun u2 @@ aux (typ2::acc) ttyps' in
            let t3 = U.make_fun u3 @@ aux acc ttyps' in
            let f2 = U.new_var_of_term t2 in
            let f3 = U.new_var_of_term t3 in
            U.make_lets [f2,[],t2; f3,[],t3] @@
              U.make_app t_typ1
                [U.make_fun b @@
                   U.make_if
                     (U.make_or (U.make_var b) U.randbool_unit_term)
                     (U.make_app (U.make_var f2) [U.unit_term])
                     (U.make_app (U.make_var f3) [U.unit_term]);
                 U.make_var f3]
      in
      U.make_fun x @@ aux [] [assert false]
  | Inter(_, _) -> unsupported "Ref_type_gen.generate: Inter"
  | Union(styp, []) ->
      assert false;
      U.make_bottom styp
  | Union(_, [typ]) ->
      assert false;
      generate_cps typ
  | Union(_, typs) -> unsupported "Ref_type_gen.generate_cps: Union"
  | ExtArg(x,typ1,typ2) -> unsupported "Ref_type_gen.generate_cps: ExtArg"
  | List _ -> unsupported "Ref_type_gen.generate_check_cps: List"
 *)
