let main n lda = 
	if n < lda then
		if 0 <= n then
			(assert(0 <= n);assert(n < lda);)
		else ()
	else ()


