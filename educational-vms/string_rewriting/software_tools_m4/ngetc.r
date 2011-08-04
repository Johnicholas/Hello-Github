include /usr/style/io/globdefs
# ngetc _ get a (possibly pushed back) character
	character function ngetc(c)
	character getc
	character c
	include cdefio

	if (bp > 0)
		c = buf(bp)
	else {
		bp = 1
		buf(bp) = getc(c)
		}
	if (c != EOF)
		bp = bp - 1
	ngetc = c
	return
	end
