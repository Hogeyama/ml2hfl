(*引数と返り値の関係を表す一般的な述語が発見できていない*)
let rec copy x = if x = 0 then x else 1 + copy (x - 1)
let main n = if n = 10 then assert(map n >= n)
