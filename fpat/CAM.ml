open Util
open Combinator
open Term

(** Categorical abstract machine *)

type instr =
  | Primitive of Const.t
  | Access of int
  | Closure of code
  | Apply
  | Return
  | Dummy
  | Update
  | EndLet
  | SkipIfNot of int
  | Skip of int
and v = V of Const.t list | C of code * env | D
and code = instr list
and env = v ref list
and stack = v list

let rec pr_instr ppf = function
  | Primitive(c) -> Format.fprintf ppf "PRIMITIVE(%s)" (Const.string_of c)
  | Access(n) -> Format.fprintf ppf "ACCESS(%d)" n
  | Closure(code) -> Format.fprintf ppf "CLOSURE(%a)" pr_code code
  | Apply -> Format.fprintf ppf "APPLY"
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
  | D -> Format.fprintf ppf "?"
and pr_code ppf = Format.fprintf ppf "@[<v>%a@]" (List.pr pr_instr ";@,")
and pr_env ppf = List.map (!) >> Format.fprintf ppf "%a" (List.pr pr_val ", ")
and pr_stack ppf = Format.fprintf ppf "%a" (List.pr pr_val ", ")

let reduce code env stack =
  match code, env, stack with
  | Primitive(c) :: code, env, stack ->
    let v = V([c]) in
    code, env, v :: stack
  | Access(n) :: code, env, stack ->
    let rv = try List.nth env n with _ -> assert false in
    code, env, !rv :: stack
  | Closure(code') :: code, env, stack ->
    code, env, C(code', env) :: stack
  | Apply :: code, env, V([c]) :: V(cs) :: stack ->
    code, env, V(Const.eval (cs @ [c])) :: stack
  | Apply :: code, env, v :: C(code', env') :: stack ->
    code', ref v :: env', C(code, env) :: stack
  | Return :: code, env, v :: C(code', env') :: stack ->
    code', env', v :: stack
  | Dummy :: code, env, stack ->
    code, ref D :: env, stack
  | Update :: code, rv :: env, v :: stack ->
    rv := v;
    code, rv :: env, stack
  | EndLet :: code, _ :: env, stack ->
    code, env, stack
  | SkipIfNot(n) :: code, env, V([Const.True]) :: stack ->
    code, env, stack
  | SkipIfNot(n) :: code, env, V([Const.False]) :: stack ->
    List.drop n code, env, stack
  | Skip(n) :: code, env, stack ->
    List.drop n code, env, stack
  | _ -> assert false

let run code =
  let rec aux i code env stack =
    Logger.printf4
      "@[<v>reduction step %a:@,  @[<v>code:@,  %a@,env:@,  %a@,stack:@,  %a@]@]@,"
      Integer.pr i
      pr_code code
      pr_env env
      pr_stack stack;
    match code with
    | [] -> ()
    | _ -> uncurry3 (aux (i + 1)) (reduce code env stack)
  in
  aux 1 code [] []
let run = Logger.log_block1 "CAM.run" run

let compile =
  MLExp.fold
    (object
      method fvar (Idnt.N(n)) = [Access(n)]
      method fcon c = [Primitive(c)]
      method fif ty r1 r2 r3 =
        r1
        @ SkipIfNot(List.length r2 + 1) :: r2
        @ Skip(List.length r3) :: r3
      method flet ty _ r1 r2 = r1 @ r2 @ [EndLet]
      method fletrec _ _ = assert false
      method fevent _ _ = assert false
      method fapp r1 rs =
        List.fold_left (fun r1 r2 -> r1 @ r2 @ [Apply]) r1 rs
      method ffun _ r1 = [Closure(r1 @ [Return])]
      method ffix _ r1 = Dummy :: r1 @ [Update]
      method fcls _ _ _ = assert false
      method ftuple _ _ = assert false
      method fkon _ _ = assert false
      method farray _ = assert false
      method faget a _ = assert false
      method faset a _ _ _ = assert false
    end)

(** @test simple *)
let test () =
  Format.printf "Categorical abstract machine@,";
  run (compile (IntTerm.add (IntTerm.one) (IntTerm.make 2)))

(** @test fib *)
let test_fib () =
  Format.printf "Categorical abstract machine@,";
  Format.printf "Compiling...@,";
  let c = compile MLExp.fib in
  Format.printf "Code: %a@," pr_code c;
  Format.printf "Running...@,";
  run c

(** @test fib in CPS *)
let test_fib_cps () =
  Format.printf "%a@," Term.pr MLExp.fib_cps;
  Format.printf "Categorical abstract machine@,";
  Format.printf "Compiling...@,";
  let c = compile MLExp.fib_cps in
  Format.printf "Code: %a@," pr_code c;
  Format.printf "Running...@,";
  run c
