include globdefs
# pbstr _ push string back onto input
	subroutine pbstr(in)
	character in(MAXLINE)
	integer length
	integer i

	for (i = length(in); i > 0; i = i - 1)
		call putbak(in(i))
	return
	end
