include globdefs

# putbak _ push character back onto input
	subroutine putbak(c)
	character c
	include cdefio

	bp = bp + 1
	if (bp > BUFSIZE)
		call error("too many characters pushed back.")
	buf(bp) = c
	return
	end
