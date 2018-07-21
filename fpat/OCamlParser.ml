open Util
open Combinator
open Asttypes
open Typedtree
open Parsetree
open Types
open MLExp

(** 4.03.0 or later is required *)

let () = Compmisc.init_path false

(* @TODO should be moved *)
type constr = {
  hcs: HCCS.t;
  dir: PredVarPoles.t;
  prior: PredVarPriority.t;
  templsize: (Idnt.t * (int * int)) list;
  size: SizeFun.t;
  rank: RankFun.t;
  strategies: PredSubst.t;
  lemma: (PredVar.t list * (PredVar.t * int) option
          * Formula.t * ProofTree.h) list;
  games: Game.t list
}
let hcs_of constr = constr.hcs
let dir_of constr = constr.dir
let prior_of constr = constr.prior
let templsize_of constr = constr.templsize
let size_of constr = constr.size
let rank_of constr = constr.rank
let strategies_of constr = constr.strategies
let lemma_of constr = constr.lemma
let games_of constr = constr.games

let tenv = ref ([] : TypEnv.t)
let rtenv = ref ([] : RefTypEnv.t)

(** auxiliary functions *)
let idnt_of_ident ident = ident |> Ident.name |> Idnt.make

(** parsing functions *)
type return =
  | Prog of Term.t * (Idnt.t * Type.t) list (* MLExp.t and simple types *)
  | Type of (Idnt.t * (Idnt.t * Type.t) list) list (* ADT definitions *)

type attr =
  | Templ of RefTypEnv.t
  | Constr of constr

let adtenv = ref []

let progs rets =
  List.fold_right
    (fun r res ->
       match r with
       | Prog (p,t) -> (p,t) :: res
       | Type _ -> res)
    rets []

let split_rets rets =
  List.fold_right
    (fun r (progs, types) ->
       match r with
       | Prog (p, t) -> ((p, t) :: progs, types)
       | Type (t) -> (progs, t @ types))
    rets ([], [])



(* concat let bindings with accessors if the constructor has arguments
   [match l with Cons(x,xs) -> e] => [let x = _get_Cons_0 l in let xs = _get_Cons_1 l in e] *)
let exp_w_args cond (pat, exp) =
  let accessors = ADTFormula.mk_accessors_wo_typing pat cond
  in (* (var_id, Accessor) list*)
  List.fold_right
    (fun (id, accessor) rest ->
       MLExp.mk_let Type.mk_unknown (id, Type.mk_unknown) accessor rest)
    accessors
    exp

let rec from_string str =
  str
  |> Lexing.from_string
  |> Parse.implementation
  |> from_structure

and from_file filename =
  filename
  |> open_in
  |> Lexing.from_channel
  |> Parse.implementation
  |> preprocess
  |> from_structure_with_attr
  |> RefTypEnv.update_base_types_of_templates

and preprocess parsetree =
  parsetree
  |> Ast_mapper.default_mapper.Ast_mapper.structure
    (Ppx.unknown_int_mapper []) (* Rewrite [%q] with RCamlConst.unknown_int "q" *)
  |> Ast_mapper.default_mapper.Ast_mapper.structure
    (Ppx.set_mapper []) (* Rewrite [%set str] using SetTerm.t *)


(** main procedure. return a pair of attributes information and
    program information *)
and from_structure_with_attr struc =
  let initial_env = Compmisc.initial_env () in
  let tystruc,signature,env =
    Typemod.type_structure initial_env struc Location.none
  in
  let attrs, str_items =
    tystruc.str_items
    |> List.partition
      (fun item ->
         match item.str_desc with
         | Tstr_attribute _ -> true
         | _ -> false)
  in
  (* replace [@@ rtype "(n:int) -> {v:int| v=n }"]
     with    [@@@ rtype "s :: (n:int) -> {v:int| v=n }"] *)
  let to_floating_attr s attr =
    let mk_const_exp s o attr l =
      {pexp_desc=Pexp_constant(Pconst_string(s,o)); pexp_loc=l; pexp_attributes=attr}
    in
    (function
      | (i,PStr({pstr_desc=Pstr_eval({pexp_desc=Pexp_constant Pconst_string(u,o)},a);pstr_loc=l}::tl))
        -> (i,PStr({pstr_desc=Pstr_eval(mk_const_exp (s^"::"^u) o [] l,a); pstr_loc=l}::tl))
           |> from_attribute
           |> (fun x -> Some x)
      | other -> None)
    |> (fun f -> List.filter_map f attr)
  in
  let item_attributes = (* item-attribute ::= [@@ attr-id  attr-payload ] *)
    tystruc.str_items
    |> List.map (function {str_desc=Tstr_value(_,vb_list)} -> vb_list | _ -> [])
    |> List.flatten
    |> List.filter_map
      (fun vb ->
         match vb.vb_pat.pat_desc with
         | Tpat_var({Ident.name=s},_) -> Some (vb.vb_attributes |> to_floating_attr s)
         | _ -> None)
    |> List.flatten
  in
  let attrs =
    attrs
    |> List.map
      (fun item ->
         match item.str_desc with
         | Tstr_attribute attr -> from_attribute attr
         | _ -> assert false)
    |> (@) item_attributes
    |> List.fold_left
      (fun (templs, constr) attr ->
         match attr with
         | Templ t -> (t@templs), constr
         | Constr c -> templs, { hcs = c.hcs @ constr.hcs;
                                 dir = c.dir @ constr.dir;
                                 prior = c.prior @ constr.prior;
                                 templsize = c.templsize @ constr.templsize;
                                 size = c.size @ constr.size;
                                 rank = c.rank @ constr.rank;
                                 strategies = c.strategies @ constr.strategies;
                                 lemma = c.lemma @ constr.lemma;
                                 games = c.games @ constr.games })
      ([], { hcs = []; dir = []; prior = []; templsize = [];
             size = []; rank = []; strategies = []; lemma = []; games = [] })
  in
  let attrs, str_items =
    (* 1. take size functions (let size [@size] = ...) from str_items,
       2. transform them to sizeFun.t (+ update type env for coefficents) and
       3. add them to attrs *)
    let size = ref [] in
    let str_items =
      List.map
        (fun si -> match si.str_desc with
           | Tstr_value(rec_flag,vb_list) ->
             let sizes, others =
               List.partition (fun vb ->
                   match vb.vb_pat.pat_attributes with
                   | ({Asttypes.txt="size"},_) :: _ -> true
                   | _ -> false)
                 vb_list in
             size := sizes @ !size;
             {si with str_desc = Tstr_value(rec_flag,others)}
           | _ -> si)
        str_items
    in
    let new_int =
      let cnt = ref 0 in
      fun () -> cnt := !cnt + 1; !cnt
    in
    let rec e2t e =
      match e.exp_desc with
      | Texp_ident(_, id, _) ->
        let open Longident in
        begin
          match id.txt with
          | Lident "+" -> Term.mk_const (Const.Add Type.mk_int)
          | Lident "-" -> Term.mk_const (Const.Sub Type.mk_int)
          | Lident "*" -> Term.mk_const (Const.Mul Type.mk_int)
          | Lident v -> Term.mk_var (Idnt.make v)
          | Ldot(Lident "RCamlConst", "unknown_int") -> Term.mk_var (Idnt.make "RCamlConst.unknown_int")
          | Ldot(Lident "RCamlConst", "set_str") -> Term.mk_var (Idnt.make "RCamlConst.set_str")
          | _ -> failwith "Not supported identifier in size function"
        end
      | Texp_constant(Asttypes.Const_int i) -> Term.mk_const (Const.Int i)
      | Texp_constant(Asttypes.Const_string (s, so)) -> Term.mk_const (Const.String s)
      | Texp_construct({Asttypes.txt = Longident.Lident "true"}, _, []) -> Term.mk_const Const.True
      | Texp_apply(exp, args) ->
        begin
          match e2t exp, List.map arg2t args with
          | Term.Var Idnt.V "RCamlConst.unknown_int", [Term.Const Const.String s] ->
            let update_type_env s =
              let x = Idnt.mk_coeff s in
              tenv := (x, Type.mk_int) :: !tenv;
              let v = Idnt.new_var () in
              let phi = IntFormula.eq (Term.mk_var v) (Term.mk_var x) in
              let rty = RefTyp.mk_base v Type.mk_int phi in
              rtenv := RefTypEnv.Env (x, rty) :: !rtenv;
              Term.mk_var x
            in
            begin
              (* Without postfixes, identify every "q" (q != q),
                 with the same postfixes, parse as the same coefficents (q1 == q1), and
                 with different postfixes, identify those coefficents (q1 != q2 *)
              match s with
              | "q" -> update_type_env @@ s^"_"^(new_int () |> string_of_int)
              | _ ->
                let sc = Idnt.mk_coeff s in
                if List.mem_assoc sc !tenv then
                  Term.mk_var sc
                else
                  update_type_env s
            end
          | Term.Var Idnt.V "size_list", _ -> (* 1 + (size_list hd) -> 1 + hd *)
            List.hd (List.map arg2t args)
          | Term.Var Idnt.V "RCamlConst.set_str", [Term.Const Const.String s] ->
            let rec to_cata t =
              match Term.fun_args t with
              | Term.Const Const.UFun _, hd::_ -> to_cata hd
              | v, args -> Term.mk_app v (List.map to_cata args)
            in
            Lexing.from_string s
            |> MLRefparser.value MLReflexer.token
            |> to_cata
          | e, args -> Term.mk_app e args
        end
      | _ -> failwith "Not supported expression in size function"
    and arg2t = function (_, Some e) -> e2t e | _ -> assert false in
    let load_sizeFun (vb : Typedtree.value_binding) =
      match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
      | Tpat_var({Ident.name=s},loc), Texp_function(_,case_list,_) ->
        let id = Idnt.make s in
        let body =
          List.map
            (fun {c_lhs=pat; c_rhs=e} ->
               match pat.pat_desc with
               | Tpat_construct({Asttypes.txt=Longident.Lident s},_,pat_list) ->
                 let constr = Idnt.make s in
                 let arg =
                   List.map (function
                       | {pat_desc=Tpat_var(id,_)} -> Idnt.make id.Ident.name
                       | {pat_desc=Tpat_construct({Asttypes.txt=Longident.Lident s'},_,pat_list')} -> Idnt.make s'
                       | _ -> assert false) pat_list in
                 let term = e2t e in
                 constr, (arg, term)
               | _ -> assert false) case_list
        in
        id, body
      | _ -> assert false
    in
    (Pair.map_snd
       (fun c -> {c with size=c.size @ List.map load_sizeFun !size})
       attrs,
     str_items)
  in
  let add_list_types (ts, types) =
    let ids = List.map fst types in
    let list = List.filter (fun (x,_) -> List.mem x ids |> not) !adtenv in
    let adts =
      List.filter (fun (x,ty) -> x = Idnt.make (Type.string_of ty)) list
    in
    let adt_id = List.map fst adts in
    let list = List.filter (fun (x,_) -> List.mem x adt_id |> not) list in
    let types =
      List.map
        (fun id ->
           let adt_ty = List.assoc id adts in
           let cs = List.filter (snd >> Type.ret_of >> (=) adt_ty) list in
           (id, cs))
        adt_id
      |> (@) types
    in (* @todo classify *)
    (ts, types)
  in
  (attrs, str_items |> List.map from_structure_item |> split_rets |> add_list_types)

and from_structure struc =
  let initial_env = Compmisc.initial_env () in
  let tystruc,signature,env =
    Typemod.type_structure initial_env struc Location.none
  in
  tystruc.str_items |> List.map from_structure_item |> progs

and core_type_trans ct =
  match ct.ctyp_desc with
  | Ttyp_constr (p, _, _) ->
    begin
      match Path.name p with
      | "int" -> Type.mk_int
      | "float" -> Type.mk_real
      | "unit" -> Type.mk_unit
      | "bool" -> Type.mk_bool
      | "string" -> Type.mk_string
      | s ->
        try List.assoc (Idnt.make s) !adtenv
        with Not_found -> Type.mk_unknown
    end
  | Ttyp_tuple (tys) -> Type.mk_tuple (List.map core_type_trans tys)
  | Ttyp_arrow (_, t1, t2) ->
    Type.mk_fun [core_type_trans t1; core_type_trans t2]
  | _ -> failwith "not impl"

and from_structure_item item =
  match item.str_desc with
  | Tstr_eval (e, _) ->
    Prog (from_expression e, [Idnt.make "-", restore_type e.exp_type])
  | Tstr_value (rec_flag, vblist) ->
    Prog (from_let Type.mk_unknown rec_flag vblist None,
          restore_type_from_vblist vblist)
  | Tstr_type (_, type_decls) -> (* Algebraic Data Types *)
    let type_kind = function
      | Ttype_variant constr_decls ->
        List.map
          (fun dcl ->
             idnt_of_ident dcl.Typedtree.cd_id,
             List.map core_type_trans
               (match dcl.Typedtree.cd_args with
                | Cstr_tuple cts -> cts
                | _ -> assert false))
          constr_decls
      | _ -> raise (Global.NotImplemented "OCamlParser.type_kind")
    in
    (* update adtenv with tenv of type declarations
       (required by mutual recursive definitions) *)
    let update_adtenv =
      List.iter
        (fun decl ->
           let id = idnt_of_ident decl.typ_id in
           adtenv := (id, Type.mk_adt id (type_kind decl.typ_kind
                                          |> List.map fst)) :: !adtenv)
    in
    let from_decl decl =
      let id = idnt_of_ident decl.typ_id in
      let env = type_kind decl.typ_kind in
      let ty = Type.mk_adt id (List.map fst env) in
      id, List.map
        (fun (id, args) ->
           let args_ty =
             args
             |> List.map
               (fun x -> if Type.string_of x = Idnt.string_of id then ty else x)
             |> flip Type.mk_fun_args_ret ty
           in
           adtenv := (id, args_ty) :: !adtenv;
           (id, args_ty)) env
    in
    let () = update_adtenv type_decls in
    Type (type_decls |> List.map from_decl)
  | _ -> raise (Global.NotImplemented "OCamlParser.from_implementation")

(* make a refinement type template or an additional constraint
   from attribute. returns (templ * option constr) *)
and from_attribute (id, payload) =
  let empty_constr =
    { hcs=[]; dir=[]; prior=[]; templsize=[];
      size=[]; rank=[]; strategies=[]; lemma=[]; games=[] }
  in
  let str =
    match payload with
    | Parsetree.PStr s ->
      begin
        match s |> from_structure with
        | [] -> ""
        | x :: _ ->
          x |> fst |> flip Term.let_const (function Const.String s -> s | _ -> "")
      end
    | _ -> ""
  in
  match id.txt with
  | "rtype"
  | "types"
  | "t" -> (* templates *)
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.types MLReflexer.token
        |> (fun x -> Templ(x))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        Format.printf
          "Parse error (types)@.line: %d@.number: %d@.token: %s@."
          curr.Lexing.pos_lnum
          (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
          (Lexing.lexeme lexbuf);
        raise exn
    end
  | "hccs"
  | "h" -> (* constraints *)
    str
    |> Lexing.from_string
    |> MLRefparser.hcs_main MLReflexer.token
    |> (fun (hcs, pole, prior, templ) ->
        Constr({ hcs = hcs; dir = pole; prior = prior; templsize = templ;
                 size = []; rank = []; strategies = []; lemma = []; games = [] }))
  | "size"
  | "s" -> (* size functions *)
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.sizes MLReflexer.token
        |> (fun x -> Constr({empty_constr with size = x}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (size functions)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "rank"
  | "r" -> (* ranking functions *)
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.ranks MLReflexer.token
        |> (fun x -> Constr({empty_constr with rank = x}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (ranking functions)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "angelic_nondet" ->
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.pred_def MLReflexer.token
        |> (fun x -> Constr({empty_constr with strategies = [x]}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (angelic nondet)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "lemma"
  | "l" -> (* induction lemma *)
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.lemmas MLReflexer.token
        |> (fun x -> Constr({empty_constr with lemma = x}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (induction lemma)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end

  | "safety_game_angel"
  | "safety_game_demon"
  | "liveness_game_angel"
  | "liveness_game_demon" ->
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.pred_def MLReflexer.token
        |> (fun (p, pred) ->
            let g =
              match id.txt with
              | "safety_game_angel" ->
                assert (p = Idnt.make "safe");
                Game.Safety
                  (true, (pred, Pred.mk_top (Pred.type_of pred)), false, None)
              | "safety_game_demon" ->
                assert (p = Idnt.make "safe");
                Game.Safety
                  (false, (pred, Pred.mk_top (Pred.type_of pred)), false, None)
              | "liveness_game_angel" ->
                assert (p = Idnt.make "goal");
                Game.Liveness
                  (true, (pred, Pred.mk_bot (Pred.type_of pred)), false, None)
              | "liveness_game_demon" ->
                assert (p = Idnt.make "goal");
                Game.Liveness
                  (false, (pred, Pred.mk_bot (Pred.type_of pred)), false, None)
              | _ -> assert false
            in
            Constr({empty_constr with games = [g]}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (games)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "weak_recurrence_game_angel"
  | "weak_recurrence_game_demon" ->
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.wrg MLReflexer.token
        |> (fun (cs, rset, ord) ->
            let ord = PartOrd.reflexive_transitive_closure_of ord in
            let cs = List.map (fun (id, pred) -> id, (pred, pred)) cs in
            let g =
              match id.txt with
              | "weak_recurrence_game_angel" ->
                Game.WeakRecurrence(true, cs, rset, ord, false)
              | "weak_recurrence_game_demon" ->
                Game.WeakRecurrence(false, cs, rset, ord, false)
              | _ ->
                assert false
            in
            Constr({empty_constr with games = [g]}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (games)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "AFMC_game" -> (* alternation-free *)
    let lexbuf = str |> Lexing.from_string in
    begin
      try
        lexbuf
        |> MLRefparser.prop MLReflexer.token
        |> (fun phi ->
            let g = Game.AFMC(phi) in
            Constr({empty_constr with games = [g]}))
      with exn ->
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Format.printf
          "Parser error (games)@.line: %d@.number: %d@.token: %s@."
          line cnum tok;
        raise exn
    end
  | "solver" ->
    begin
      try
        HCCSSolver.of_string_dyn str |> HCCSSolver.link_dyn;
        Templ []
      with Not_found -> Templ []
    end
  | "rsolver" ->
    begin
      try
        RHCCSSolver.ref_solver := RHCCSSolver.of_string_dyn str;
        GameSolver.inline := false; (*@todo*)
        Templ []
      with Not_found -> Templ []
    end
  | _ -> Constr(empty_constr)

(* get the type information from typedtree (return Type.t) *)
and restore_type ty =
  let env = ref [] in
  let rec dig ty =
    (* (Format.printf "%a@," Printtyp.raw_type_expr ty); *)
    match ty.desc with
    | Tvar None ->
      if List.mem_assoc ty.id !env then
        List.assoc ty.id !env |> Type.mk_var
      else begin
        let id = Idnt.new_tvar () in
        env := (ty.id, id)::!env;
        Type.mk_var id
      end
    | Tconstr(p,typ,_) ->
      begin
        match Path.name p with
        | "int" -> Type.mk_int
        | "float" -> Type.mk_real
        | "unit" -> Type.mk_unit
        | "bool" -> Type.mk_bool
        | "string" -> Type.mk_string
        | "list" ->
          begin
            try List.assoc (Idnt.make "list") !adtenv
            with Not_found ->
              let t = dig (List.hd typ) in
              let adt_id = mk_ocaml_list_tid t in
              try List.assoc (Idnt.make adt_id) !adtenv
              with Not_found -> mk_ocaml_list_ty t
          end
        | s ->
          try List.assoc (Idnt.make s) !adtenv with Not_found -> Type.mk_unknown
      end
    | Tarrow(_,t1,t2,_) -> Type.mk_fun [dig t1; dig t2]
    | Tlink t -> dig t
    | Ttuple(tys) -> Type.mk_tuple (List.map dig tys)
    | _ ->
      Format.printf "uncaught:%a@." Printtyp.raw_type_expr ty;
      assert false
  in
  dig ty
and mk_ocaml_list_tid ty = Type.string_of ty ^ "_list"
and mk_ocaml_list_nil ty = Idnt.make ("[]_" ^ Type.string_of ty)
and mk_ocaml_list_cons ty = Idnt.make ("::_" ^ Type.string_of ty)
and mk_ocaml_list_ty ty =
  let id = mk_ocaml_list_tid ty |> Idnt.make in
  let nil, cons =
    Idnt.make ("[]_" ^ mk_ocaml_list_tid ty), Idnt.make ("::_" ^ mk_ocaml_list_tid ty)
  in
  let typ = Type.mk_adt id [nil; cons] in
  adtenv := [(id, typ); (nil, typ); (cons, Type.mk_fun [ty;typ;typ])] @ !adtenv;
  typ

and restore_type_from_vblist vbl =
  vbl |> List.map (fun x -> (x.vb_pat |> from_pattern |> fst,
                             x.vb_pat.pat_type |> restore_type))

and from_expression {exp_desc=exp_desc; exp_loc=loc; exp_type=typ; exp_env=env} =
  let ty = restore_type typ in
  match exp_desc with
  | Texp_ident (path, lident, value_desc) -> path |> from_ident_path
  | Texp_constant c -> c |> from_constant
  | Texp_let (rec_flag, list, exp) -> from_let ty rec_flag list  (Some exp)
  | Texp_function (_, [{c_lhs; c_guard; c_rhs}], _) ->
    (* @todo support guard and type annotation *)
    MLExp.mk_fun_pat [pattern_trans ty c_lhs, Type.mk_unknown] (from_expression c_rhs)
  | Texp_function (_, cases, _) ->
    (* @todo implement this *)
    raise (Global.NotImplemented "OCamlParser.from_expression")
  | Texp_apply (e, es) ->
    conv_app
      (from_expression e)
      (List.map
         (function
           | (_, Some e') -> from_expression e'
           | _ -> raise (Global.NotImplemented "OCamlParser.from_expression"))
         es)
  | Texp_match (e, cases, _, partial) ->
    (* encode match by if-then-else with accessors and recognizers *)
    let cond = from_expression e in
    let cond_ty = restore_type e.exp_type in
    let cases_ty =
      List.hd cases
      |> (fun {c_lhs; c_guard; c_rhs} -> c_rhs.exp_type)
      |> restore_type
    in
    let cases =
      List.map
        (fun {c_lhs; c_guard; c_rhs} -> (* @todo support guards *)
           (pattern_trans cond_ty c_lhs, from_expression c_rhs))
        cases
    in
    begin
      match partial with
      | Partial ->
        List.fold_right
          (fun (pat, exp) acc ->
             MLExp.mk_if cases_ty
               (ADTFormula.mk_recognizers_wo_typing pat cond
                |> Formula.band |> Formula.term_of)
               (if Pattern.has_args pat then exp_w_args cond (pat,exp) else exp)
               acc)
          cases
          (Formula.mk_false |> Formula.term_of |> MLExp.mk_assert)
      (* should use fail? *)
      | Total ->
        List.fold_right
          (fun (pat, exp) acc ->
             MLExp.mk_if cases_ty
               (ADTFormula.mk_recognizers_wo_typing pat cond
                |> Formula.band |> Formula.term_of)
               (if Pattern.has_args pat then exp_w_args cond (pat,exp) else exp)
               acc)
          cases
          MLExp.mk_undef
    end

  | Texp_construct (_, cstr_desc, e) -> (* e represent args *)
    let args = List.map from_expression e in
    from_constructor_description ty cstr_desc args
  | Texp_ifthenelse (cond, e1, Some e2) ->
    MLExp.mk_if ty (from_expression cond) (from_expression e1) (from_expression e2)
  | Texp_ifthenelse (cond, e1, None) ->
    MLExp.mk_if ty (from_expression cond) (from_expression e1) UnitTerm.make
  | Texp_assert e -> e |> from_expression |> MLExp.mk_assert
  | Texp_tuple es ->
    TupTerm.make
      (List.map (fun e -> restore_type e.exp_type) es)
      (List.map from_expression es)
  | Texp_sequence(e1, e2) ->
    MLExp.mk_let ty
      (Idnt.new_var (), Type.mk_unit)
      (from_expression e1)
      (from_expression e2)
  | _ -> raise (Global.NotImplemented "OCamlParser.from_expression")

(** transform Typedtree.pattern into Pattern.t *)
and pattern_trans ty pat =
  match pat.pat_desc with
  | Tpat_any -> Pattern.W
  | Tpat_alias (_, id, _)
  | Tpat_var (id, _) -> Pattern.mk_var (idnt_of_ident id)
  | Tpat_construct (_, desc, []) ->
    begin
      match desc.cstr_name with
      | "()" -> Pattern.U
      | "true" -> Pattern.C (Pattern.Const_bool(true))
      | "false" -> Pattern.C (Pattern.Const_bool(false))
      | "[]" -> Pattern.K (mk_ocaml_list_nil ty , Pattern.U)
      | _ -> Pattern.K (Idnt.make desc.cstr_name, Pattern.U)
    end
  | Tpat_construct (_, desc, args) ->
    begin
      match desc.cstr_name with
      | "::" ->
        let ty =
          if Type.is_unknown ty then
            (* try to get type information for (::) from typedtree *)
            match args with
            | x :: _ -> restore_type x.pat_type
            | _ -> Logger.printf0 ~kind:Logger.Warning
                     "cannot identify the type of \"::\"@,";
              Type.mk_unknown
          else ty
        in
        Pattern.K (mk_ocaml_list_cons ty,
                   Pattern.T(List.map (pattern_trans ty) args))
      | _ -> Pattern.K (Idnt.make desc.cstr_name,
                        Pattern.T(List.map (pattern_trans ty) args))
    end
  | Tpat_tuple (ps) -> Pattern.T (List.map (pattern_trans ty) ps)
  | Tpat_constant (Asttypes.Const_int(n)) -> Pattern.C (Pattern.Const_int(n))
  | Tpat_constant (_) ->
    raise (Global.NotImplemented "OCamlParser.pattern_trans: Tpat_constant")
  | _ ->
    raise (Global.NotImplemented "OCamlParser.pattern_trans")

and from_ident_path path =
  let name = Path.name path in
  if (name.[0] > Char.uppercase_ascii (name.[0]))
  then (* variable *)
    MLExp.mk_var (Idnt.make name)
    (*(Idnt.make (name ^ (string_of_int (Path.binding_time path))))*)
  else (* module function or something *)
    MLExp.mk_var (Idnt.make name)

and from_constant = function
  | Const_int n -> IntTerm.make n
  | Const_string (s,_) -> StringTerm.make s
  | Const_float f -> RealTerm.make (float_of_string f)
  | _ -> raise (Global.NotImplemented "OCamlParser.from_constant")

and from_let ty rec_flag vblist optexp =
  let aux vb =
    let pat, e = from_value_binding vb in (* fpat pattern and term *)
    (pat, [], e)
  in
  match rec_flag with
  | Nonrecursive ->
    let aux vb = vb |> aux |> (fun (p,_,e) -> (p,e)) in
    let bodies = List.map aux vblist in
    begin
      match optexp with
      | None ->
        List.fold_right
          (fun (pat, e) acc -> MLExp.mk_let_pat ty pat e acc)
          bodies
          MLExp.mk_undef (* instead of toplevel def constant *)
      | Some exp ->
        List.fold_right
          (fun (pat, e) acc -> MLExp.mk_let_pat ty pat e acc)
          bodies
          (from_expression exp)
    end
  | Recursive ->
    match optexp with
    | None -> (* let rec f = e *)
      MLExp.mk_letrec_pat (List.map aux vblist) MLExp.mk_undef
    | Some exp -> (* let rec f = e in exp *)
      MLExp.mk_letrec_pat (List.map aux vblist) (from_expression exp)

and from_value_binding vb =
  (pattern_trans (Type.mk_unknown(*@todo*)) vb.vb_pat, Type.mk_unknown),
  from_expression vb.vb_expr

and from_pattern pat =
  match pat.pat_desc with
  | Tpat_alias (_, id, _) -> (idnt_of_ident id, Type.mk_unknown)
  | Tpat_var (id, _) -> (idnt_of_ident id, Type.mk_unknown)
  | Tpat_construct (_,desc,_) -> (Idnt.make desc.cstr_name, Type.mk_unit)
  | _ -> raise (Global.NotImplemented "OCamlParser.from_pattern")

and from_constructor_description ty desc args =
  match desc.cstr_name with
  | "true" -> BoolTerm.mk_true
  | "false" -> BoolTerm.mk_false
  | "()" -> UnitTerm.make
  | "[]" ->
    let id = mk_ocaml_list_nil ty in
    ADTTerm.mk_kon (id, (ADTFormula.lookup id !adtenv)) args
  | "::" ->
    let id = mk_ocaml_list_cons ty in
    ADTTerm.mk_kon (id, (ADTFormula.lookup id !adtenv)) args
  | c when Str.string_match (Str.regexp "[A-Z]") c 0 -> (* data constructor *)
    ADTTerm.mk_kon (Idnt.make c, (ADTFormula.lookup (Idnt.make c) !adtenv)) args
  | c ->
    Format.printf "@[uncaught constructor: %s@]@." c;
    raise (Global.NotImplemented "OCamlParser.from_constructor_description")

and conv_app t ts =
  match t, ts with
  | Term.Var(Idnt.V("Pervasives.+")), [t1; t2] -> IntTerm.add t1 t2
  | Term.Var(Idnt.V("Pervasives.+.")), [t1; t2] -> RealTerm.add t1 t2
  | Term.Var(Idnt.V("Pervasives.-")), [t1; t2] -> IntTerm.sub t1 t2
  | Term.Var(Idnt.V("Pervasives.-.")), [t1; t2] -> RealTerm.sub t1 t2
  | Term.Var(Idnt.V("Pervasives.*")), [t1; t2] -> IntTerm.mul t1 t2
  | Term.Var(Idnt.V("Pervasives.*.")), [t1; t2] -> RealTerm.mul t1 t2
  | Term.Var(Idnt.V("Pervasives./")), [t1; t2] -> IntTerm.div t1 t2
  | Term.Var(Idnt.V("Pervasives./.")), [t1; t2] -> RealTerm.div t1 t2
  | Term.Var(Idnt.V("Pervasives.~-")), [t] -> IntTerm.neg t
  | Term.Var(Idnt.V("Pervasives.~-.")), [t] -> RealTerm.neg t

  | Term.Var(Idnt.V("Pervasives.=")), [t1; t2]
  | Term.Var(Idnt.V("Pervasives.==")), [t1; t2] (* @todo pointer comp *) ->
    Formula.eq Type.mk_unknown t1 t2 |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.<>")), [t1; t2]
  | Term.Var(Idnt.V("Pervasives.!=")), [t1; t2] (* @todo pointer comp *) ->
    Formula.neq Type.mk_unknown t1 t2 |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.>")), [t1; t2] ->
    NumFormula.gt Type.mk_unknown t1 t2 |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.<")), [t1; t2] ->
    NumFormula.lt Type.mk_unknown t1 t2 |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.>=")), [t1; t2] ->
    NumFormula.geq Type.mk_unknown t1 t2 |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.<=")), [t1; t2] ->
    NumFormula.leq Type.mk_unknown t1 t2 |> Formula.term_of

  | Term.Var(Idnt.V("Pervasives.&&")), ts ->
    ts |> List.map Formula.of_term |> Formula.band |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.||")), ts ->
    ts |> List.map Formula.of_term |> Formula.bor |> Formula.term_of
  | Term.Var(Idnt.V("Pervasives.not")), [t] ->
    t |> Formula.of_term |> Formula.bnot |> Formula.term_of

  | Term.Var(Idnt.V("Random.bool")), [t] ->
    if t = Term.Const(Const.Unit) then MLExp.mk_rand_bool
    else raise (Global.NotImplemented "Random.bool can only take ().")
  | Term.Var(Idnt.V("Random.int")), [t] ->
    if t = IntTerm.zero then MLExp.mk_rand_int
    else raise (Global.NotImplemented "Random.int can only take 0.")
  | Term.Var(Idnt.V("Random.float")), [t] ->
    if t = RealTerm.zero then MLExp.mk_rand_real
    else raise (Global.NotImplemented "Random.float can only take 0.0.")

  | Term.Var(Idnt.V("Pervasives.read_int")), [t] ->
    if t = Term.Const(Const.Unit) then MLExp.mk_read_int
    else raise (Global.NotImplemented "read_int can only take ().")
  | Term.Var(Idnt.V("Pervasives.read_float")), [t] ->
    if t = Term.Const(Const.Unit) then MLExp.mk_read_real
    else raise (Global.NotImplemented "read_real can only take ().")

  | Term.Var(Idnt.V("RCamlConst.unknown")), [Term.Const (Const.String s)]
  | Term.Var(Idnt.V("RCamlConst.unknown_int")), [Term.Const (Const.String s)] ->
    let x = Idnt.mk_coeff s in
    tenv := (x, Type.mk_int) :: !tenv;
    let v = Idnt.new_var () in
    rtenv :=
      RefTypEnv.Env
        (x,
         RefTyp.mk_base v
           Type.mk_int
           (IntFormula.eq (Term.mk_var v) (Term.mk_var x))) :: !rtenv;
    Term.mk_var x
  | Term.Var(Idnt.V("RCamlConst.unknown_real")), [Term.Const (Const.String s)] ->
    let x = Idnt.mk_coeff s in
    tenv := (x, Type.mk_real) :: !tenv;
    let v = Idnt.new_var () in
    rtenv :=
      RefTypEnv.Env
        (x,
         RefTyp.mk_base v
           Type.mk_real
           (RealFormula.eq (Term.mk_var v) (Term.mk_var x))) :: !rtenv;
    Term.mk_var x

  | _ -> MLExp.mk_app t ts
