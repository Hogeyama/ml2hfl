(* Function composition�łQ�Ԗڂ�map��context��
n=0�Ƃ���specific�ɂȂ��Ă��܂��Ă��邽�ߎ��s *)
let rec copy x = if x = 0 then 0 else 1 + copy (x - 1)
let check x y = assert (x >= y)
let main n = check (copy (copy n)) n
