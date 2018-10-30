
type foo = Foo of int
         | Bar of bool * int * (int * int)

(*{SPEC}
type some_func
  :  m : int *
     {x : foo
     | match x with
       | Foo(n) -> n > 0 && n > m || m = 0
       | Bar(b,n,pair) -> not b && n > 0 || m = 0
     }
  -> unit
{SPEC}*)
let some_func (m, foo) = match foo with
  | Foo(n) ->
      if m = 0 then () else
      (assert (n > 0); assert (n > m))
  | Bar(b,n,_) ->
      if m = 0 then () else
      (assert (not b); assert (n > 0))

