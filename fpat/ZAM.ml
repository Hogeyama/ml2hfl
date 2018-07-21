open Util
open Combinator
open Term

(** ZINC abstract machine *)

type instr =
  | Primitive of Const.t
  | Access of int
  | Closure of code
  | TailApply
  | Apply
  | Pushmark
  | Grab
  | Return
  | Dummy
  | Update
  | EndLet
  | SkipIfNot of int
  | Skip of int
and v = V of Const.t list | C of code * env | M | D
and code = instr list
and env = v ref list
and stack = v list

let rec pr_instr ppf = function
  | Primitive(c) -> Format.fprintf ppf "PRIMITIVE(%s)" (Const.string_of c)
  | Access(n) -> Format.fprintf ppf "ACCESS(%d)" n
  | Closure(code) -> Format.fprintf ppf "CLOSURE(%a)" pr_code code
  | TailApply -> Format.fprintf ppf "TAILAPPLY"
  | Apply -> Format.fprintf ppf "APPLY"
  | Pushmark -> Format.fprintf ppf "PUSHMARK"
  | Grab -> Format.fprintf ppf "GRAB"
  | Return -> Format.fprintf ppf "RETURN"
  | Dummy -> Format.fprintf ppf "DUMMY"
  | Update -> Format.fprintf ppf "UPDATE"
  | EndLet -> Format.fprintf ppf "ENDLET"
  | SkipIfNot(n) -> Format.fprintf ppf "SKIPIFNOT(%d)" n
  | Skip(n) -> Format.fprintf ppf "SKIP(%d)" n
and pr_val ppf = function
  | V cs -> String.pr ppf (cs |> List.map Const.string_of |> String.concat " ")
  | C(code, env) -> Format.fprintf ppf "[<code>, <env>]"
  (*Format.fprintf ppf "[%a, %a]" pr_code code pr_env env*)
  | M -> Format.fprintf ppf "eps"
  | D -> Format.fprintf ppf "?"
and pr_code ppf = Format.fprintf ppf "@[<v>%a@]" (List.pr pr_instr ";@,")
and pr_env ppf = List.map (!) >> Format.fprintf ppf "%a" (List.pr pr_val ", ")
and pr_stack ppf = Format.fprintf ppf "%a" (List.pr pr_val ", ")

let reduce code env astack rstack =
  match code, env, astack, rstack with
  | Primitive(c) :: code, env, astack, rstack ->
    let v = V([c]) in
    code, env, v :: astack, rstack
  | Access(n) :: code, env, astack, rstack ->
    let rv = try List.nth env n with _ -> assert false in
    code, env, !rv :: astack, rstack
  | Closure(code') :: code, env, astack, rstack ->
    code, env, C(code', env) :: astack, rstack
  | TailApply :: code, env, V(cs1) :: astack, C(code', env') :: rstack ->
    let cs2, M :: astack' =
      List.span_map (function V([c]) -> Some(c) | _ -> None) astack
    in
    code', env', V(Const.eval (cs1 @ cs2)) :: astack', rstack
  | Apply :: code, env, V(cs1) :: astack, rstack ->
    let cs2, M :: astack' =
      List.span_map (function V([c]) -> Some(c) | _ -> None) astack
    in
    code, env, V(Const.eval (cs1 @ cs2)) :: astack', rstack
  | TailApply :: code, env, C(code', env') :: astack, rstack ->
    code', env', astack, rstack
  | Apply :: code, env, C(code', env') :: astack, rstack ->
    code', env', astack, C(code, env) :: rstack
  | Pushmark :: code, env, astack, rstack ->
    code, env, M :: astack, rstack
  | Grab :: code, env, M :: astack, C(code', env') :: rstack ->
    code', env', C(Grab :: code, env) :: astack, rstack
  | Grab :: code, env, v :: astack, rstack ->
    code, ref v :: env, astack, rstack
  | Return :: code, env, v :: M :: astack, C(code', env') :: rstack ->
    code', env', v :: astack, rstack
  | Return :: code, env, C(code', env') :: astack, rstack ->
    code', env', astack, rstack
  | Dummy :: code, env, astack, rstack ->
    code, ref D :: env, astack, rstack
  | Update :: code, rv :: env, v :: astack, rstack ->
    rv := v;
    code, rv :: env, astack, rstack
  | EndLet :: code, _ :: env, astack, rstack ->
    code, env, astack, rstack
  | SkipIfNot(n) :: code, env, V([Const.True]) :: astack, rstack ->
    code, env, astack, rstack
  | SkipIfNot(n) :: code, env, V([Const.False]) :: astack, rstack ->
    List.drop n code, env, astack, rstack
  | Skip(n) :: code, env, astack, rstack ->
    List.drop n code, env, astack, rstack
  | _ ->
    begin
      Format.printf
        "@[<v>code:@,  %a@,env:@,  %a@,astack:@,  %a@,rstack:@,  %a@]@,"
        pr_code code
        pr_env env
        pr_stack astack
        pr_stack rstack;
      assert false
    end

let run code =
  let rec aux i code env astack rstack =
    Logger.printf5
      "@[<v>reduction step %a:@,  @[<v>code:@,  %a@,env:@,  %a@,astack:@,  %a@,rstack:@,  %a@]@]@,"
      Integer.pr i
      pr_code code
      pr_env env
      pr_stack astack
      pr_stack rstack;
    match code with
    | [] -> ()
    | _ -> uncurry4 (aux (i + 1)) (reduce code env astack rstack)
  in
  aux 1 code [] [] []
let run = Logger.log_block1 "ZAM.run" run

let compile e =
  MLExp.fold
    (object
      method fvar (Idnt.N(n)) = fun tail ->
        if tail then [Access(n); Return] else [Access(n)]
      method fcon c = fun tail ->
        if tail then [Primitive(c); Return] else [Primitive(c)]
      method fif ty r1 r2 r3 = fun tail ->
        let c2 = r2 tail in
        let c3 = r3 tail in
        if tail then
          r1 false
          @ SkipIfNot(List.length c2) :: c2 (* @todo is this OK? *)
          @ c3
        else
          r1 false
          @ SkipIfNot(List.length c2 + 1) :: c2
          @ Skip(List.length c3) :: c3
      method flet ty _ r1 r2 = fun tail ->
        if tail then r1 false @ r2 true else r1 false @ r2 false @ [EndLet]
      method fletrec _ _ = fun tail -> assert false
      method fevent _ _ = fun tail -> assert false
      method fapp r1 rs = fun tail ->
        if tail then
          List.flatten (List.rev_map (feed false) rs) @
          r1 false
          @ [TailApply]
        else
          Pushmark :: List.flatten (List.rev_map (feed false) rs)
          @ r1 false
          @ [Apply]
      method ffun _ r1 = fun tail ->
        if tail then Grab :: r1 true else [Closure(Grab :: r1 true)]
      method ffix _ r1 = fun tail -> Dummy :: r1 false @ [Update]
      method fcls _ _ _ = fun tail -> assert false
      method ftuple _ _ = assert false
      method fkon _ _ = assert false
      method farray _ = assert false
      method faget a _ = assert false
      method faset a _ _ _ = assert false
    end)
    e
    false

(** @test simple *)
let test () =
  Format.printf "ZINC abstract machine@,";
  run (compile (IntTerm.add (IntTerm.one) (IntTerm.make 2)))

(** @test fib *)
let test_fib () =
  Format.printf "ZINC abstract machine@,";
  Format.printf "Compiling...@,";
  let c = compile MLExp.fib in
  Format.printf "Code: %a@," pr_code c;
  Format.printf "Running...@,";
  run c

(** @test fib in CPS *)
let test_fib_cps () =
  Format.printf "%a@," Term.pr MLExp.fib_cps;
  Format.printf "ZINC abstract machine@,";
  Format.printf "Compiling...@,";
  let c = compile MLExp.fib_cps in
  Format.printf "Code: %a@," pr_code c;
  Format.printf "Running...@,";
  run c
