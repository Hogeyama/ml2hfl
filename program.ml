open Util

type t =
    {term: Syntax.term;
     env: Ref_type.env;
     attr: attr list}

and attr = ACPS

let term {term} = term
let env {env} = env
let attr {attr} = attr

let make ?(env=Ref_type.Env.empty) ?(attr=[]) term =
  {term; env; attr}

let map ?(tr_env=Fun.id) tr {term; env; attr} =
  let term = tr term in
  let env = tr_env env in
  {term; env; attr}

let map_on focus ?(tr_env=Fun.id) tr {term; env; attr} =
  let r = tr term in
  let ctx,term = focus r in
  let env = tr_env env in
  ctx {term; env; attr}

let print_attr fm attr =
  match attr with
  | ACPS -> Format.fprintf fm "ACPS"

let print fm {term; env; attr} =
  Format.fprintf fm "@[{term:%a;@ env:%a;@ attr:%a}@]" Print.term_typ term Ref_type.Env.print env (Print.list print_attr) attr

let print_debug fm {term; env; attr} =
  Format.fprintf fm "@[{term:%a;@ env:%a;@ attr:%a}@]" Print.term' term Ref_type.Env.print env (Print.list print_attr) attr
