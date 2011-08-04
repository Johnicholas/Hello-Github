include /usr/style/io/globdefs
include defdefs
# gettok _ get alphanumeric string or single non\(hyalpha for define
	character function gettok(token, toksiz)
	character ngetc, type
	integer i, toksiz
	character token(toksiz)

	for (i = 1; i < toksiz; i = i + 1) {
		gettok = type(ngetc(token(i)))
		if (gettok != LETTER & gettok != DIGIT)
			break
		}
	if (i >= toksiz)
		call error("token too long.")
	if (i > 1) {			# some alpha was seen
		call putbak(token(i))
		i = i - 1
		gettok = ALPHA
		}
	# else single character token
	token(i+1) = EOS
	return
	end
