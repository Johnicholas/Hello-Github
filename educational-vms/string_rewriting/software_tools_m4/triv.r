include /usr/style/io/globdefs
include	defdefs
# define _ simple string replacement macro processor
	character gettok
	character defn(MAXDEF), t, token(MAXTOK)
	integer lookup
#	string defnam "define"
	integer defnam(7)
	data defnam(1) /LETD/, defnam(2) /LETE/, defnam(3) /LETF/
	data defnam(4) /LETI/, defnam(5) /LETN/, defnam(6) /LETE/
	data defnam(7) /EOS/
	integer deftyp(2)
	data deftyp(1) /DEFTYPE/, deftyp(2) /EOS/

	call instal(defnam, deftyp)
	for (t = gettok(token, MAXTOK); t != EOF; t = gettok(token, MAXTOK))
		if (t != ALPHA)		# output non\(hyalpha tokens
			call putlin(token, STDOUT)
		else if (lookup(token, defn) == NO)	# and undefined
			call putlin(token, STDOUT)
		else if (defn(1) == DEFTYPE) {		# get definition
			call getdef(token, MAXTOK, defn, MAXDEF)
			call instal(token, defn)
			}
		else
			call pbstr(defn)	# push replacement onto input
	stop
	end

# getdef (for no arguments) _ get name and definition
	subroutine getdef(token, toksiz, defn, defsiz)
	character gettok, ngetc
	integer defsiz, i, nlpar, toksiz
	character c, defn(defsiz), token(toksiz)

	if (ngetc(c) != LPAREN)
		call error("missing left paren.")
	else if (gettok(token, toksiz) != ALPHA)
		call error("non-alphanumeric name.")
	else if (ngetc(c) != COMMA)
		call error("missing comma in define.")
	# else got (name,
	nlpar = 0
	for (i = 1; nlpar >= 0; i = i + 1)
		if (i > defsiz)
			call error("definition too long.")
		else if (ngetc(defn(i)) == EOF)
			call error("missing right paren.")
		else if (defn(i) == LPAREN)
			nlpar = nlpar + 1
		else if (defn(i) == RPAREN)
			nlpar = nlpar - 1
		# else normal character in defn(i)
	defn(i-1) = EOS
	return
	end
