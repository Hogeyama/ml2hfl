open Format
open Util
open Type
open Syntax

let rec print_typ fm t = Type.print ~occur (print_term 0 false) fm t
and print_ids typ fm xs =
  if !Flag.web then
    let rec aux xs =
      match xs with
      | [] -> ()
      | [x] ->
	  fprintf fm "%a" print_id x
      | x1 :: x2 :: xs ->
	  if is_fun_typ @@ Id.typ x2 then
	    fprintf fm "$%a$ " print_id x1
	  else
	    fprintf fm "%a " print_id x1;
	  aux (x2 :: xs)
    in
    aux xs
  else
    if xs <> []
    then
      let p_id = if typ then print_id_typ else print_id in
      print_list p_id "@ " ~first:true fm xs

(*
  and print_id fm x = fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)
 *)
and print_id = Id.print

and print_id_typ fm x =
  if !Flag.print_as_ocaml then
    print_id fm x
  else
    fprintf fm "(@[%a:%a@])" print_id x (Color.cyan print_typ) (Id.typ x)

(* priority (low -> high)
   10 : Let, Letrec, If, Match, TryWith
   15 : Fun
   20 : Pair
   30 : Or
   40 : And
   50 : Eq, Lt, Gt, Leq, Geq
   60 : Cons
   65 : Add, Sub
   70 : Mult
   80 : Raise, App, Not, Ref, SetRef
   90 : Deref
 *)

and paren pri p = if pri < p then "","" else "(",")"

and print_binop fm = function
  | Eq -> fprintf fm "="
  | Lt -> fprintf fm "<"
  | Gt -> fprintf fm ">"
  | Leq -> fprintf fm "<="
  | Geq -> fprintf fm ">="
  | And -> fprintf fm "&&"
  | Or -> fprintf fm "||"
  | Add -> fprintf fm "+"
  | Sub -> fprintf fm "-"
  | Mult -> fprintf fm "*"

and print_termlist pri typ fm ts =
  if !Flag.web then
    let rec aux ts =
      match ts with
      | [] -> ()
      | [t] -> fprintf fm "@ %a" (print_term pri typ) t
      | t1 :: t2 :: ts' ->
          if is_fun_typ t2.typ then
            fprintf fm "@ $%a$" (print_term pri typ) t1
          else
            fprintf fm "@ %a" (print_term pri typ) t1;
          aux (t2 :: ts')
    in
    aux ts
  else
    print_list (print_term pri typ) "@ " fm ts

and print_const fm = function
  | Unit -> fprintf fm "()"
  | True -> fprintf fm "true"
  | False -> fprintf fm "false"
  | Int n when n<0 -> fprintf fm "(%d)" n
  | Int n -> fprintf fm "%d" n
  | Char c -> fprintf fm "%C" c
  | String s -> fprintf fm "%S" s
  | Float r -> fprintf fm "%f" r
  | Int32 n -> fprintf fm "%ldl" n
  | Int64 n -> fprintf fm "%LdL" n
  | Nativeint n -> fprintf fm "%ndn" n
  | CPS_result -> fprintf fm "{end}"
  | RandValue(TInt,false) when !Flag.print_as_ocaml -> fprintf fm "(fun () -> Random.int 0)"
  | RandValue(TInt,false) -> fprintf fm "rand_int"
  | RandValue(TInt,true) -> fprintf fm "rand_int_cps"
  | RandValue(typ',false) -> fprintf fm "rand_val[%a]" print_typ typ'
  | RandValue(typ',true) -> fprintf fm "rand_val_cps[%a]" print_typ typ'

and print_attr fm = function
  | ACPS -> fprintf fm "ACPS"
  | AAbst_under -> fprintf fm "AAbst_under"
  | ATerminate -> fprintf fm "ATerminate"
  | ANotFail -> fprintf fm "ANotFail"
  | ADeterministic -> fprintf fm "ADeterministic"
  | AComment s -> fprintf fm "AComment %S" s
  | AId n -> fprintf fm "AId %d" n
  | AMark -> fprintf fm "AMark"
  | ADoNotInline -> fprintf fm "ADoNotInline"

and ignore_attr_list = if true then ADoNotInline::const_attr else []

and print_attr_list fm attrs =
  List.print print_attr fm @@ List.Set.diff attrs ignore_attr_list

and print_term pri typ fm t =
  let decomp_comment a =
    match a with
    | AComment s -> Some s
    | _ -> None
  in
  let pr attr fm desc =
    let comments = List.filter_map decomp_comment t.attr in
    if comments = []
    then fprintf fm "@[%a@]" (print_desc attr pri typ) desc
    else fprintf fm "(@[(* @[%a@] *)@ %a@])" (print_list pp_print_string ", ") comments (print_desc attr pri typ) desc
  in
  let attr = List.filter (Option.is_none -| decomp_comment) t.attr in
  if List.Set.subset attr ignore_attr_list || !Flag.print_as_ocaml
  then pr t.attr fm t.desc
  else fprintf fm "(@[%a@ #@ %a@])" (pr t.attr) t.desc print_attr_list t.attr

and print_desc attr pri typ fm desc =
  match desc with
  | Const c -> print_const fm c
  | Var x -> print_id fm x
  | Fun _ ->
      let xs,t =
        let rec decomp = function
          | {desc=Fun(x,t)} ->
              let xs,t' = decomp t in
              x::xs, t'
          | t -> [], t
        in
        decomp {desc; typ=typ_unknown; attr=[]}
      in
      let p = 15 in
      let s1,s2 = paren pri (p+1) in
      fprintf fm "%s@[<hov 2>fun@[%a@] ->@ %a%s@]" s1 (print_ids typ) xs (print_term 0 typ) t s2
  | App({desc=Const(RandValue(TInt,false))}, [{desc=Const Unit}]) when !Flag.print_as_ocaml ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[<hov 2>%sRandom.int 0%s@]" s1 s2
  | App(t, ts) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[<hov 2>%s%a@ %a%s@]" s1 (print_term p typ) t (print_termlist p typ) ts s2
  | If(t1, {desc=Const Unit}, {desc=App({desc=Event("fail",_)}, [{desc=Const Unit}])}) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[<hov 2>%sassert %a%s@]" s1 (print_term p typ) t1 s2
  | If(t1, t2, t3) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      fprintf fm "%s@[<hv>if@[@ %a@]@ then@ " s1 (print_term p typ) t1;
      pp_print_if_newline fm ();
      pp_print_string fm "  ";
      fprintf fm "@[%a@]" (print_term p typ) t2;
      if t3.desc <> Const Unit then
        begin
          fprintf fm "@ else@ ";
          pp_print_if_newline fm ();
          pp_print_string fm "  ";
          fprintf fm "@[%a@]" (print_term p typ) t3
        end;
      fprintf fm "@]%s" s2
  | Let(_, [], t) ->
      Format.printf "@.%a@." (print_term 0 typ) t;
      assert false
  | Let(_, [_, [], {desc=App({desc=Event("fail",_)}, [{desc=Const Unit}])}], {desc=Bottom}) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "@[%sassert@ false%s@]" s1 s2
  | Let(flag, bindings, t2) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
      let b = ref true in
      let print_binding fm (f,xs,t1) =
        let pre =
          if !b then
            "let" ^ (if List.mem ADoNotInline attr && not !Flag.print_as_ocaml then "!" else "") ^ s_rec
          else
            "and"
        in
        fprintf fm "@[<hov 2>%s @[<hov 2>%a%a@] =@ %a@]" pre print_id f (print_ids typ) xs (print_term 0 typ) t1;
        b := false
      in
      let print_bindings bs = print_list print_binding "@ " ~last:true bs in
      fprintf fm "%s@[<v>@[<hv>%a@]in@ @[<hov>%a@]@]%s" s1 print_bindings bindings (print_term p typ) t2 s2
  | Not{desc = BinOp(Eq, t1, t2)} ->
      let p = 50 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a@ <>@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | BinOp(Mult, {desc=Const (Int -1)}, {desc=Var x}) ->
      let p = 70 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[-%a@]%s" s1 print_id x s2
  | BinOp(op, t1, t2) ->
      let p = match op with Mult -> 70 | Add|Sub -> 65 | And -> 40 | Or -> 30 | _ -> 50 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a@ %a@ %a@]%s" s1 (print_term p typ) t1 print_binop op (print_term p typ) t2 s2
  | Not t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[not@ %a@]%s" s1 (print_term p typ) t s2
  | Event(s,false) -> fprintf fm "<%s>" s
  | Event(s,true) -> fprintf fm "<|%s|>" s
  | Record fields ->
      let aux fm (s,t) = fprintf fm "%s=%a" s (print_term 0 typ) t in
      fprintf fm "{%a}" (print_list aux ";@ ") fields
  | Field(t,s) -> fprintf fm "%a.%s" (print_term 9 typ) t s
  | SetField(t1,s,t2) -> fprintf fm "%a.%s@ <-@ %a" (print_term 9 typ) t1 s (print_term 3 typ) t2
  | Nil -> fprintf fm "[]"
  | Cons(t1,t2) ->
      let p = 60 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a::@,%a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Constr(s,ts) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      if ts = []
      then pp_print_string fm s
      else fprintf fm "%s@[%s(%a)@]%s" s1 s (print_list (print_term 20 typ) ",") ts s2
  | Match(t,pats) ->
      let p = 10 in
      let s1,s2 = paren pri p in
      let first = ref true in
      let aux (pat,cond,t) =
        let print_cond fm =
          match cond.desc with
          | Const True -> ()
          | _ -> fprintf fm "when@ @[<hov 2>%a@]@ " (print_term p typ) cond
        in
        fprintf fm "@ @[<hov 4>";
        if !first then pp_print_if_newline fm ();
        first := false;
        pp_print_string fm "| ";
        fprintf fm "@[<hov 2>%a %t->@ %a@]@]" print_pattern pat print_cond (print_term p typ) t
      in
      fprintf fm "%s@[<hv>@[@[<hov 2>match@ %a@]@ with@]" s1 (print_term p typ) t;
      List.iter aux pats;
      fprintf fm "@]%s" s2
  | Raise t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[raise@ %a@]%s" s1 (print_term p typ) t s2
  | TryWith(t1,{desc=Fun(e,{desc=Match({desc=Var e'},pats)})})
       when Id.same e e' && (function ({pat_desc=PAny},{desc=Const True},{desc=Raise {desc=Var e''}}) -> Id.same e e'' | _ -> false) @@ List.last pats ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      let aux (pat,cond,t) =
        let print_cond fm =
          match cond.desc with
          | Const True -> ()
          | _ -> fprintf fm "when@ @[<hov 2>%a@]@ " (print_term p typ) cond
        in
        let bar = if List.length pats = 2 then "" else "| " in
        fprintf fm "@ @[<hov 4>%s@[<hov 2>%a %t->@]@ %a@]" bar print_pattern pat print_cond (print_term p typ) t
      in
      fprintf fm "%s@[@[<hov 2>try@ %a@]@ @[<hv 2>with" s1 (print_term p typ) t1;
      List.iter aux @@ fst @@ List.decomp_snoc pats;
      fprintf fm "@]@]%s" s2
  | TryWith(t1,t2) ->
      let p = 10 in
      let s1,s2 = paren pri (p+1) in
      fprintf fm "%s@[@[<hov 2>try@ %a@]@ with@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Tuple ts ->
      let p = 20 in
      fprintf fm "@[(%a)@]" (print_list (print_term p typ) ",@ ") ts
  | Proj(0,t) when tuple_num t.typ = Some 2 ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[fst@ %a@]%s" s1 (print_term p typ) t s2
  | Proj(1,t) when tuple_num t.typ = Some 2 ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[snd@ %a@]%s" s1 (print_term p typ) t s2
  | Proj(i,t) when !Flag.print_as_ocaml ->
      let p = 80 in
      let s1,s2 = paren pri p in
      let s = "fun (" ^ String.join "," (List.init (Option.get @@ tuple_num t.typ ) (fun j -> if i = j then "x" else "_") ) ^ ") -> x" in
      fprintf fm "%s@[(%s)@ %a@]%s" s1 s (print_term p typ) t s2
  | Proj(i,t) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[#%d@ %a@]%s" s1 i (print_term p typ) t s2
  | Bottom when !Flag.print_as_ocaml -> fprintf fm "let rec bot() = bot() in bot()"
  | Bottom -> fprintf fm "_|_"
  | Label(info, t) ->
      fprintf fm "(@[label[@[%a@]]@ %a@])" print_info info (print_term 80 typ) t
  | Ref t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[ref@ %a@]%s" s1 (print_term p typ) t s2
  | Deref t ->
      let p = 90 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[!%a@]%s" s1 (print_term p typ) t s2
  | SetRef(t1, t2) ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[%a := %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | TNone -> fprintf fm "None"
  | TSome t ->
      let p = 80 in
      let s1,s2 = paren pri p in
      fprintf fm "%s@[Some %a@]%s" s1 (print_term p typ) t s2

and print_info fm info =
  match info with
  | InfoId x ->
      fprintf fm "Id %a" print_id x
  | InfoString s ->
      fprintf fm "String %S" s
  | InfoInt n ->
      fprintf fm "Int %d" n
  | InfoTerm t ->
      fprintf fm "Term %a" (print_term 80 false) t
  | InfoIdTerm(x,t) ->
      fprintf fm "IdTerm(%a,@ %a)" print_id x (print_term 80 false) t



and print_pattern fm pat =
  match pat.pat_desc with
  | PAny -> pp_print_string fm "_"
  | PVar x -> print_id fm x
  | PAlias(p,x) -> fprintf fm "(%a as %a)" print_pattern p print_id x
  | PConst c -> print_term 1 false fm c
  | PConstruct(c,pats) ->
      let aux' = function
          [] -> ()
        | [pat] -> fprintf fm "(%a)" print_pattern pat
        | pat::pats ->
            fprintf fm "(%a" print_pattern pat;
            List.iter (fun pat -> fprintf fm ",%a" print_pattern pat) pats;
            pp_print_string fm ")"
      in
      pp_print_string fm c;
      aux' pats
  | PNil -> fprintf fm "[]"
  | PCons(p1,p2) -> fprintf fm "%a::%a" print_pattern p1 print_pattern p2
  | PRecord pats ->
      let aux' = function
          [] -> ()
        | [_,pat] -> fprintf fm "(%a)" print_pattern pat
        | (_,pat)::pats ->
            fprintf fm "(%a" print_pattern pat;
            List.iter (fun (_,pat) -> fprintf fm ",%a" print_pattern pat) pats;
            pp_print_string fm ")"
      in
      aux' pats
  | POr(pat1,pat2) -> fprintf fm "(%a | %a)" print_pattern pat1 print_pattern pat2
  | PTuple pats -> fprintf fm "(%a)" (print_list print_pattern ", ") pats
  | PNone -> fprintf fm "None"
  | PSome p -> fprintf fm "(Some %a)" print_pattern p
let print_term typ fm = print_term 0 typ fm

let rec print_term' pri fm t =
  fprintf fm "(@[";(
    match t.desc with
    | Const c -> print_const fm c
    | Var x when t.typ = Id.typ x -> print_id fm x
    | Var x -> print_id_typ fm x
    | Fun(x, t) ->
        let p = 2 in
        let s1,s2 = paren pri p in
        fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term' p) t s2
    | App(t, ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        fprintf fm "%s%a@ %a%s" s1 (print_term' p) t (print_termlist' p) ts s2
    | If(t1, t2, t3) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s@[@[if %a@]@ then @[%a@]@ else @[%a@]@]%s"
                s1 (print_term' p) t1 (print_term' p) t2 (print_term' p) t3 s2
    | Let(flag, bindings, t2) ->
        let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
        let p = 10 in
        let s1,s2 = paren pri (p+1) in
        let b = ref true in
        let print_binding fm (f,xs,t1) =
          let pre = if !b then "let" ^ s_rec else "and" in
          fprintf fm "@[<hov 2>%s%a =@ %a@ @]" pre (print_ids true) (f::xs) (print_term' p) t1;
          b := false
        in
        let print_bindings bs = print_list print_binding "" bs in
        begin
          match t2.desc with
          | Let _ -> fprintf fm "%s@[<v>@[<hov 2>%a@]@ in@ %a@]%s"
                             s1 print_bindings bindings (print_term' p) t2 s2
          | _ -> fprintf fm     "%s@[<v>@[<hov 2>%a@]@ @[<v 2>in@ @]@[<hov>%a@]@]%s"
                         s1 print_bindings bindings (print_term' p) t2 s2
        end
    | BinOp(op, t1, t2) ->
        let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
        let s1,s2 = paren pri p in
        fprintf fm "%s%a %a %a%s" s1 (print_term' p) t1 print_binop op (print_term' p) t2 s2
    | Not t ->
        let p = 6 in
        let s1,s2 = paren pri p in
        fprintf fm "%snot %a%s" s1 (print_term' p) t s2
    | Event(s,b) -> fprintf fm "{%s}" s
    | Record fields ->
        let aux fm (s,t) = fprintf fm "%s=%a" s (print_term' 0) t in
        fprintf fm "{%a}" (print_list aux ";@ ") fields
    | Field(t,s) -> fprintf fm "%a.%s" (print_term' 9) t s
    | SetField(t1,s,t2) -> fprintf fm "%a.%s <- %a" (print_term' 9) t1 s (print_term' 3) t2
    | Nil -> fprintf fm "[]"
    | Cons(t1,t2) ->
        let p = 7 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s%a::%a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
    | Constr(s,ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let aux fm = function
            [] -> ()
          | [t] -> fprintf fm "(%a)" (print_term' 1) t
          | t::ts ->
              fprintf fm "(%a" (print_term' 1) t;
              List.iter (fun t -> fprintf fm ",%a" (print_term' 1) t) ts;
              pp_print_string fm ")"
        in
        fprintf fm "%s%s%a%s" s1 s aux ts s2
    | Match(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,{desc=Const True},t) -> fprintf fm "%a -> %a@ " print_pattern' pat (print_term' p) t
          | (pat,cond,t) -> fprintf fm "%a when %a -> %a@ "
                                    print_pattern' pat (print_term' p) cond (print_term' p) t
        in
        fprintf fm "%s@[match %a with@ " s1 (print_term' p) t;
        List.iter aux pats;
        fprintf fm "@]%s" s2
    | Raise t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sraise %a%s" s1 (print_term' 1) t s2
    | TryWith(t1,t2) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%stry %a with@ %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
    | Tuple ts ->
        fprintf fm "@[(%a)@]" (print_list (print_term' 0) ",@ ") ts
    | Proj(0,t) when tuple_num t.typ = Some 2 ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s@[fst %a@]%s" s1 (print_term' 1) t s2
    | Proj(1,t) when tuple_num t.typ = Some 2 ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s@[snd %a@]%s" s1 (print_term' 1) t s2
    | Proj(i,t) ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s@[#%d %a@]%s" s1 i (print_term' 1) t s2
    | Bottom -> fprintf fm "_|_"
    | Label(info, t) ->
        fprintf fm "(@[label[%a]@ %a@])" print_info info (print_term' 0) t
    | Ref t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sref %a%s" s1 (print_term' 1) t s2
    | Deref t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s!%a%s" s1 (print_term' 1) t s2
    | SetRef(t1,t2) ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%s%a := %a%s" s1 (print_term' 1) t1 (print_term' 1) t2 s2
    | TNone -> fprintf fm "None"
    | TSome t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
        fprintf fm "%sSome %a%s" s1 (print_term' 1) t s2
  );fprintf fm ":@ @[%a@]@])" (Color.cyan print_typ) t.typ
and print_pattern' fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
    | PAny -> pp_print_string fm "_"
    | PVar x -> print_id_typ fm x
    | PAlias(p,x) -> fprintf fm "(%a as %a)" aux p print_id x
    | PConst c -> print_term' 1 fm c
    | PConstruct(c,pats) ->
        let aux' = function
            [] -> ()
          | [pat] -> fprintf fm "(%a)" aux pat
          | pat::pats ->
              fprintf fm "(%a" aux pat;
              List.iter (fun pat -> fprintf fm ",%a" aux pat) pats;
              pp_print_string fm ")"
        in
        pp_print_string fm c;
        aux' pats
    | PNil -> fprintf fm "[]"
    | PCons(p1,p2) -> fprintf fm "%a::%a" aux p1 aux p2
    | PRecord pats ->
        let aux' = function
            [] -> ()
          | [_,pat] -> fprintf fm "(%a)" aux pat
          | (_,pat)::pats ->
              fprintf fm "(%a" aux pat;
              List.iter (fun (_,pat) -> fprintf fm ",%a" aux pat) pats;
              pp_print_string fm ")"
        in
        aux' pats
    | POr(pat1,pat2) -> fprintf fm "(%a | %a)" aux pat1 aux pat2
    | PTuple pats -> fprintf fm "(%a)" (print_list aux ", ") pats
    | PNone -> fprintf fm "None"
    | PSome p -> fprintf fm "(Some %a)" aux p
  in
  fprintf fm "| %a" aux pat

and print_termlist' pri = print_list (print_term' pri) "@ "


let print_defs fm (defs:(id * (id list * term)) list) =
  let print_fundef (f, (xs, t)) =
    fprintf fm "%a %a-> %a.\n" print_id f (print_ids false) xs (print_term false) t
  in
  List.iter print_fundef defs


let string_of_const c = Format.asprintf "%a" print_const c
let string_of_binop op = Format.asprintf "%a" print_binop op
let string_of_typ typ = Format.asprintf "%a" print_typ typ
let string_of_constr t =
  match t.desc with
  | Const _ -> "Const"
  | Var _ -> "Var"
  | Fun _ -> "Fun"
  | App _ -> "App"
  | If _ -> "If"
  | Let _ -> "Let"
  | BinOp _ -> "BinOp"
  | Not _ -> "Not"
  | Event _ -> "Event"
  | Record _ -> "Record"
  | Field _ -> "Field"
  | SetField _ -> "SetField"
  | Nil -> "Nil"
  | Cons _ -> "Cons"
  | Constr _ -> "Constr"
  | Match _ -> "Match"
  | Raise _ -> "Raise"
  | TryWith _ -> "TryWith"
  | Tuple _ -> "Tuple"
  | Proj _ -> "Proj"
  | Bottom -> "Bottom"
  | Label _ -> "Label"
  | Ref _ -> "Ref"
  | Deref _ -> "Deref"
  | SetRef _ -> "SetRef"
  | TNone -> "TNone"
  | TSome _ -> "TSome"


let typ = print_typ
let id = print_id
let id_typ = print_id_typ
let pattern = print_pattern
let const = print_const
let desc = print_desc [] 0 false
let term = print_term false
let term' = print_term' 0
let term_typ = print_term true
let defs = print_defs
let constr fm t = pp_print_string fm @@ string_of_constr t
let attr = print_attr_list
