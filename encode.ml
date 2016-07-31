open Util
open Syntax
open Term_util
open Type



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
        List.map2 aux fields @@ decomp_trecord t.typ
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




let abst_ref = make_trans ()

let abst_ref_term t =
  match t.desc with
  | Ref t1 ->
      make_ignore @@ abst_ref.tr_term t1
  | Deref t1 ->
      let t1' = abst_ref.tr_term t1 in
      let typ = abst_ref.tr_typ t.typ in
      make_seq t1' @@ make_randvalue_unit typ
  | SetRef(t1, t2) ->
      let t1' = abst_ref.tr_term t1 in
      let t2' = abst_ref.tr_term t2 in
      make_ignore @@ make_pair t1' t2'
  | _ -> abst_ref.tr_term_rec t

let abst_ref_typ typ =
  match typ with
  | TApp(TRef, _) -> TUnit
  | _ -> abst_ref.tr_typ_rec typ

let () = abst_ref.tr_term <- abst_ref_term
let () = abst_ref.tr_typ <- abst_ref_typ
let abst_ref t =
  t |> abst_ref.tr_term |> Trans.inst_randval



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
      let i = Id.new_var ~name:"i" TInt in
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
  | TApp(TArray, [typ]) -> make_tref @@ make_tpair TInt @@ make_tfun TInt @@ encode_array.tr_typ typ
  | _ -> encode_array.tr_typ_rec typ

let () = encode_array.tr_desc <- encode_array_desc
let () = encode_array.tr_typ <- encode_array_typ
let array = encode_array.tr_term




let encode_record = make_trans ()
let encode_record_typ typ =
  match fold_data_type typ with
  | TRecord fields ->
      make_ttuple @@ List.map (fun (s,(f,typ)) -> if f = Mutable then unsupported "mutable record"; typ) fields
  | _ -> encode_record.tr_typ_rec typ

let rec encode_record_pat p =
  match p.pat_desc with
  | PRecord fields ->
      let ps = List.map (snd |- encode_record.tr_pat) fields in
      let typ = encode_record.tr_typ p.pat_typ in
      {pat_desc=PTuple ps; pat_typ=typ}
  | _ -> encode_record.tr_pat_rec p

let encode_record_term t =
  match t.desc with
  | Record fields ->
      if is_mutable_record t.typ then
        unsupported "Mutable records";
      make_tuple @@ List.map (encode_record.tr_term -| snd) fields
  | Field(t,s) ->
      let fields = decomp_trecord t.typ in
      if is_mutable_record t.typ then
        unsupported "Mutable records";
      make_proj (List.find_pos (fun _ (s',_) -> s = s') fields) @@ encode_record.tr_term t
  | SetField _ -> unsupported "Mutable records"
  | _ -> encode_record.tr_term_rec t

let () = encode_record.tr_term <- encode_record_term
let () = encode_record.tr_pat <- encode_record_pat
let () = encode_record.tr_typ <- encode_record_typ
let record = encode_record.tr_term




let recdata = Encode_rec.trans
let list = Encode_list.trans
