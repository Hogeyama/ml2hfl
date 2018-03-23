open Util

type t =
    {term: Syntax.term;
     env: Ref_type.env;
     attr: attr list;
     kind: kind}

and kind =
  | Safety
  | Ref_type_check of Ref_type.env

and attr = ACPS

let term {term} = term
let env {env} = env
let attr {attr} = attr

let safety ?(env=Ref_type.Env.empty) ?(attr=[]) term =
  {term; env; attr; kind=Safety}

let ref_type_check ?(env=Ref_type.Env.empty) ?(attr=[]) term check =
  {term; env; attr; kind=Ref_type_check check}

let map ?(tr_env=Fun.id) tr {term; env; attr; kind} =
  let term = tr term in
  let env = tr_env env in
  {term; env; attr; kind}

let map_on focus ?(tr_env=Fun.id) tr {term; env; attr; kind} =
  let r = tr term in
  let ctx,term = focus r in
  let env = tr_env env in
  ctx {term; env; attr; kind}

let print_attr fm attr =
  match attr with
  | ACPS -> Format.fprintf fm "ACPS"

let print_kind fm kind =
  match kind with
  | Safety -> Format.fprintf fm "Safety"
  | Ref_type_check env -> Format.fprintf fm "Refinement type checking %a" Ref_type.Env.print env

let print fm {term; env; attr; kind} =
  Format.fprintf fm "@[{@[term:%a@];@ @[env:%a@];@ @[attr:%a@];@ @[kind:%a@]}@]"
                 Print.term_typ term
                 Ref_type.Env.print env
                 (Print.list print_attr) attr
                 print_kind kind

let print_debug fm {term; env; attr; kind} =
  Format.fprintf fm "@[{@[term:%a@];@ @[env:%a@];@ @[attr:%a@];@ @[kind:%a@]}@]"
                 Print.term' term
                 Ref_type.Env.print env
                 (Print.list print_attr) attr
                 print_kind kind
