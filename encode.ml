open Util
open Syntax
open Term_util
open Type


module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)


let encode_mutable_record = make_trans ()

let encode_mutable_record_pat p =
  match p.pat_desc with
  | PRecord _ when List.exists (fun (_,(f,_)) -> f = Mutable) @@ decomp_trecord p.pat_typ ->
      unsupported "Pattern matching for mutable record (encode_mutable_record_pat)"
  | _ -> encode_mutable_record.tr_pat_rec p

let encode_mutable_record_term t =
  match t.desc with
  | Record fields ->
      let fields' =
        let aux (s,t) (_,(f,_)) =
          let t' = encode_mutable_record.tr_term t in
          s, if f = Mutable then make_ref t' else t'
        in
        try
        List.map2 aux fields @@ decomp_trecord t.typ
 with _ ->
                  Format.printf "t: %a@." Print.term t;
                  assert false
      in
      make_record fields' @@ encode_mutable_record.tr_typ t.typ
  | SetField(t1,s,t2) ->
      let t1' = encode_mutable_record.tr_term t1 in
      let t2' = encode_mutable_record.tr_term t2 in
      make_setref (make_field t1' s) t2'
  | Field(t1,s) ->
      let t1' = encode_mutable_record.tr_term t1 in
      let f,_ = List.assoc s @@ decomp_trecord t1.typ in
      let t' = make_field t1' s in
      if f = Mutable then
        make_deref t'
      else
        t'
  | _ -> encode_mutable_record.tr_term_rec t

let encode_mutable_record_typ typ =
  match typ with
  | TRecord fields ->
      let aux (s,(f,typ)) =
        let typ' = encode_mutable_record.tr_typ typ in
        let typ'' = if f = Mutable then make_tref typ' else typ' in
        s, (Immutable, typ'')
      in
      TRecord (List.map aux fields)
  | _ -> encode_mutable_record.tr_typ_rec typ

let () = encode_mutable_record.tr_typ <- encode_mutable_record_typ
let () = encode_mutable_record.tr_term <- encode_mutable_record_term
let () = encode_mutable_record.tr_pat <- encode_mutable_record_pat

let mutable_record = encode_mutable_record.tr_term




let abst_ref =
  let tr = make_trans () in
  let tr_term t =
    let t' = tr.tr_term_rec t in
    match t'.desc with
    | Ref t1 ->
        make_ignore t1 |@> Format.printf "@[%a ==> @[%a@." Print.term' t Print.term'
    | Deref t1 ->
        Flag.use_abst := true;
        Term.(seq t1 (rand t'.typ))
    | SetRef(t1, t2) ->
        Term.(seqs [t1;t2] unit)
    | _ -> t'
  in
  let tr_typ typ =
    match typ with
    | TApp(TRef, _) -> Ty.unit
    | _ -> tr.tr_typ_rec typ
  in
  tr.tr_term <- tr_term;
  tr.tr_typ <- tr_typ;
  tr.tr_term |- Trans.inst_randval



let encode_array = make_trans ()

let encode_array_desc desc =
  match desc with
  | App({desc=Var x}, [t1]) when Id.name x = "Array.of_list" && is_list_literal t1 ->
      let ts =
        t1
        |> decomp_list
        |> Option.get
        |> List.map encode_array.tr_term
      in
      let n = List.length ts in
      let len = make_int n in
      let i = Id.new_var ~name:"i" Ty.int in
      let ti = make_var i in
      let typ' = encode_array.tr_typ @@ list_typ t1.typ in
      let its = List.mapi Pair.pair ts in
      let t' =
      make_ref @@
        make_pair len @@
          make_fun i @@
            make_seq (make_assert @@ make_and (make_leq (make_int 0) ti) (make_lt ti len)) @@
              List.fold_right (fun (j,x) t -> make_if (make_eq ti (make_int j)) x t) its (make_bottom typ')
      in
      t'.desc
  | _ -> encode_array.tr_desc_rec desc

let encode_array_typ typ =
  match typ with
  | TApp(TArray, [typ]) -> Ty.(ref @@ pair int (fun_ int (encode_array.tr_typ typ)))
  | _ -> encode_array.tr_typ_rec typ

let () = encode_array.tr_desc <- encode_array_desc
let () = encode_array.tr_typ <- encode_array_typ
let array = encode_array.tr_term




let encode_record = make_trans ()
let rec encode_record_typ typ =
  match typ with
  | TRecord fields ->
      make_ttuple @@ List.map (fun (s,(f,typ)) -> if f = Mutable then unsupported "mutable record"; encode_record_typ typ) fields
  | _ -> encode_record.tr_typ_rec typ

let rec encode_record_pat p =
  match p.pat_desc with
  | PRecord fields ->
      let ps = List.map (snd |- encode_record.tr_pat) fields in
      let typ = encode_record.tr_typ p.pat_typ in
      {pat_desc=PTuple ps; pat_typ=typ}
  | _ -> encode_record.tr_pat_rec p

let encode_record_term t =
  match t.desc, t.typ with
  | Record fields, _ ->
      if is_mutable_record t.typ then
        unsupported "Mutable records";
      make_tuple @@ List.map (encode_record.tr_term -| snd) fields
  | Local(Decl_type decls, _), _ ->
      let tys = List.flatten_map (snd |- get_tdata) decls in
      let check (s,ty) =
        match ty with
        | TRecord _ -> List.mem s tys
        | _ -> false
      in
      if List.exists check decls then unsupported "recursive record types";
      encode_record.tr_term_rec t
  | Field(t,s), _ ->
      let fields = decomp_trecord t.typ in
      if is_mutable_record t.typ then
        unsupported "Mutable records";
      let t' = encode_record.tr_term t in
      make_proj (List.find_pos (fun _ (s',_) -> s = s') fields) t'
  | SetField _, _ -> unsupported "Mutable records"
  | _ -> encode_record.tr_term_rec t

let () = encode_record.tr_term <- encode_record_term
let () = encode_record.tr_pat <- encode_record_pat
let () = encode_record.tr_typ <- encode_record_typ
let record = encode_record.tr_term -| Trans.complete_precord



let rec is_simple_variant env typ =
  match typ with
  | TVariant(_,labels) -> List.for_all (snd |- (=) []) labels
  | TData s when List.mem_assoc s env -> is_simple_variant env @@ List.assoc s env
  | _ -> false

let rec position env c typ =
  match typ with
  | TVariant(_,labels) -> List.find_pos (fun _ (c',_) -> c = c') labels
  | TData s when List.mem_assoc s env -> position env c @@ List.assoc s env
  | _ -> invalid_arg "position"

let simple_variant =
  let tr = make_trans2 () in
  let tr_typ env typ =
    if is_simple_variant env typ then
      Ty.int
    else
      tr.tr2_typ_rec env typ
  in
  let tr_pat env p =
    match p.pat_desc with
    | PConstr(c, []) when is_simple_variant env p.pat_typ ->
        Pat.int @@ position env c p.pat_typ
    | _ -> tr.tr2_pat_rec env p
  in
  let tr_term env t =
    match t.desc with
    | Constr(c, []) when is_simple_variant env t.typ ->
        make_int @@ position env c t.typ
    | Local(Decl_type decls, _) ->
        tr.tr2_term_rec (decls@env) t
    | _ -> tr.tr2_term_rec env t
  in
  tr.tr2_term <- tr_term;
  tr.tr2_pat <- tr_pat;
  tr.tr2_typ <- tr_typ;
  tr.tr2_term []



let abst_rec_record =
  let tr = make_trans2 () in
  let tr_term recs t =
    match t.desc with
    | Local(Decl_type decls, t1) ->
        let recs' =
          let tys = List.flatten_map (snd |- get_tdata) decls in
          let check (s,ty) =
            match ty with
            | TRecord _ -> List.exists (List.mem -$- tys) @@ get_tdata ty
            | _ -> false
          in
          List.map (fun (s,_) -> TData s) @@ List.filter check decls
        in
        let decls' = List.map (fun (s,ty) -> s, if List.mem (TData s) recs' then Ty.int else ty) decls in
        let t1' = tr.tr2_term (recs'@recs) t1 in
        make_local (Decl_type decls') t1'
    | Record fields when List.mem t.typ recs ->
        Flag.use_abst := true;
        let bindings =
          fields
          |> List.map snd
          |> List.map (tr.tr2_term recs)
          |> List.map (Pair.add_left new_var_of_term)
        in
        make_lets bindings randint_unit_term
    | SetField(t1,_,t2) when List.mem t1.typ recs ->
        Flag.use_abst := true;
        let t1' = tr.tr2_term recs t1 in
        let t2' = tr.tr2_term recs t2 in
        make_seq t1' t2'
    | Field(t1,_) when List.mem t1.typ recs ->
        Flag.use_abst := true;
        let t1' = tr.tr2_term recs t1 in
        make_seq t1' @@ make_randvalue_unit @@ tr.tr2_typ recs t.typ
    | _ -> tr.tr2_term_rec recs t
  in
  let tr_typ recs ty =
    if List.mem ty recs then
      Ty.int
    else
      tr.tr2_typ_rec recs ty
  in
  tr.tr2_term <- tr_term;
  tr.tr2_typ <- tr_typ;
  tr.tr2_term []


let recdata = Encode_rec.trans
let list = Encode_list.trans


let pr s t =
  Debug.printf "##[Encode] %s: %a@." s Print.term_typ t

let all t =
  t
  |@> pr "INPUT"
  |> mutable_record
  |@> pr "MUTABLE_RECORD"
  |> record
  |@> pr "RECORD"
  |&!Flag.Method.ignore_exn_arg&> Trans.ignore_exn_arg
  |@!Flag.Method.ignore_exn_arg&> pr "IGNORE_EXN_ARG"
  |> simple_variant
  |@> pr "SIMPLE_VARIANT"
  |> recdata
  |@> pr "RECDATA"
  |> (list |- fst)
  |@> pr "LIST"
  |> array
  |@> pr "ARRAY"
  |> abst_ref
  |@> pr "ABST_REF"


let typ_of f typ =
  typ
  |> Id.new_var
  |> make_var
  |> f
  |> Syntax.typ
