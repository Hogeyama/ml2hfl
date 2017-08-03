let rec loop b x y =
	if (x < 10) then
		if 0 <= b then
			loop b (x+1) (y+1)
		else loop b (x+1) (y-1)
	else
		assert ((y = 0) || (y >= 0 && b >= 0) || (b <= (-1) && y <= 0))

let main b =
	let x = 0 in
	let y = 0 in
	loop b x y
	
