include /usr/style/io/globdefs
include	defdefs
# macro _ expand macros with arguments
	character gettok
	character defn(MAXDEF), t, token(MAXTOK)
	integer lookup, push
	integer ap, argstk(ARGSIZE), callst(CALLSIZE), nlb, plev(CALLSIZE)
	include cmacro
#	string balp "()"
	integer balp(3)
	data balp(1) /LPAREN/, balp(2) /RPAREN/, balp(3) /EOS/
#	string defnam "define"
	integer defnam(7)
	data defnam(1) /LETD/, defnam(2) /LETE/, defnam(3) /LETF/
	data defnam(4) /LETI/, defnam(5) /LETN/, defnam(6) /LETE/
	data defnam(7) /EOS/
#	string incnam "incr"
	integer incnam(5)
	data incnam(1) /LETI/, incnam(2) /LETN/, incnam(3) /LETC/, incnam(4) /LETR/
	data incnam(5) /EOS/
#	string subnam "substr"
	integer subnam(7)
	data subnam(1) /LETS/, subnam(2) /LETU/, subnam(3) /LETB/
	data subnam(4) /LETS/, subnam(5) /LETT/, subnam(6) /LETR/
	data subnam(7) /EOS/
#	string ifnam "ifelse"
	integer ifnam(7)
	data ifnam(1) /LETI/, ifnam(2) /LETF/, ifnam(3) /LETE/
	data ifnam(4) /LETL/, ifnam(5) /LETS/, ifnam(6) /LETE/
	data ifnam(7) /EOS/
	integer deftyp(2)
	data deftyp(1) /DEFTYPE/, deftyp(2) /EOS/
	integer inctyp(2)
	data inctyp(1) /INCTYPE/, inctyp(2) /EOS/
	integer subtyp(2)
	data subtyp(1) /SUBTYPE/, subtyp(2) /EOS/
	integer iftyp(2)
	data iftyp(1) /IFTYPE/, iftyp(2) /EOS/

	call instal(defnam, deftyp)
	call instal(incnam, inctyp)
	call instal(subnam, subtyp)
	call instal(ifnam, iftyp)

	cp = 0
	ap = 1
	ep = 1
	for (t = gettok(token, MAXTOK); t != EOF; t = gettok(token, MAXTOK)) {
		if (t == ALPHA) {
			if (lookup(token, defn) == NO)
				call puttok(token)
			else {				# defined; put it in eval stack
				cp = cp + 1
				if (cp > CALLSIZE)
					call error("call stack overflow.")
				callst(cp) = ap
				ap = push(ep, argstk, ap)
				call puttok(defn)	# stack definition
				call putchr(EOS)
				ap = push(ep, argstk, ap)
				call puttok(token)	# stack name
				call putchr(EOS)
				ap = push(ep, argstk, ap)
				t = gettok(token, MAXTOK)	# peek at next
				call pbstr(token)
				if (t != LPAREN)	# add ( ) if not present
					call pbstr(balp)
				plev(cp) = 0
				}
			}
		else if (t == LBRACK) {		# strip one level of [ ]
			nlb = 1
			repeat {
				t = gettok(token, MAXTOK)
				if (t == LBRACK)
					nlb = nlb + 1
				else if (t == RBRACK) {
					nlb = nlb - 1
					if (nlb == 0)
						break
					}
				else if (t == EOF)
					call error("EOF in string.")
				call puttok(token)
				}
			}
		else if (cp == 0)			# not in a macro at all
			call puttok(token)
		else if (t == LPAREN) {
			if (plev(cp) > 0)
				call puttok(token)
			plev(cp) = plev(cp) + 1
			}
		else if (t == RPAREN) {
			plev(cp) = plev(cp) - 1
			if (plev(cp) > 0)
				call puttok(token)
			else {				# end of argument list
				call putchr(EOS)
				call eval(argstk, callst(cp), ap-1)
				ap = callst(cp)	# pop eval stack
				ep = argstk(ap)
				cp = cp - 1
				}
			}
		else if (t == COMMA & plev(cp) == 1) {	# new arg
			call putchr(EOS)
			ap = push(ep, argstk, ap)
			}
		else
			call puttok(token)		# just stack it
		}
	if (cp != 0)
		call error("unexpected EOF.")
	stop
	end

# push _ push ep onto argstk, return new pointer ap
	integer function push(ep, argstk, ap)
	integer ap, argstk(ARGSIZE), ep

	if (ap > ARGSIZE)
		call error("arg stack overflow.")
	argstk(ap) = ep
	push = ap + 1
	return
	end

# puttok _ put a token either on output or into evaluation stack
	subroutine puttok(str)
	character str(MAXTOK)
	integer i

	for (i = 1; str(i) != EOS; i = i + 1)
		call putchr(str(i))
	return
	end

# putchr _ put single char on output or into evaluation stack
	subroutine putchr(c)
	character c
	include cmacro

	if (cp == 0)
		call putc(c)
	else {
		if (ep > EVALSIZE)
			call error("evaluation stack overflow.")
		evalst(ep) = c
		ep = ep + 1
		}
	return
	end

# eval _ expand args i through j: evaluate builtin or push back defn
	subroutine eval(argstk, i, j)
	integer index, length
	integer argno, argstk(ARGSIZE), i, j, k, m, n, t, td
	include cmacro
#	string digits "0123456789"
	integer digits(11)
	data digits(1) /DIG0/
	data digits(2) /DIG1/
	data digits(3) /DIG2/
	data digits(4) /DIG3/
	data digits(5) /DIG4/
	data digits(6) /DIG5/
	data digits(7) /DIG6/
	data digits(8) /DIG7/
	data digits(9) /DIG8/
	data digits(10) /DIG9/
	data digits(11) /EOS/

	t = argstk(i)
	td = evalst(t)
	if (td == DEFTYPE)
		call dodef(argstk, i, j)
	else if (td == INCTYPE)
		call doincr(argstk, i, j)
	else if (td == SUBTYPE)
		call dosub(argstk, i, j)
	else if (td == IFTYPE)
		call doif(argstk, i, j)
	else {
		for (k = t+length(evalst(t))-1; k > t; k = k - 1)
			if (evalst(k-1) != ARGFLAG)
				call putbak(evalst(k))
			else {
				argno = index(digits, evalst(k)) - 1
				if (argno >= 0 & argno < j-i) {
					n = i + argno + 1
					m = argstk(n)
					call pbstr(evalst(m))
					}
				k = k - 1	# skip over $
				}
		if (k == t)			# do last character
			call putbak(evalst(k))
		}
	return
	end

# dodef _ install definition in table
	subroutine dodef(argstk, i, j)
	integer a2, a3, argstk(ARGSIZE), i, j
	include cmacro

	if (j - i > 2) {
		a2 = argstk(i+2)
		a3 = argstk(i+3)
		call instal(evalst(a2), evalst(a3))	# subarrays
		}
	return
	end

# doincr _ increment argument by 1
	subroutine doincr(argstk, i, j)
	integer ctoi
	integer argstk(ARGSIZE), i, j, k
	include cmacro

	k = argstk(i+2)
	call pbnum(ctoi(evalst, k)+1)
	return
	end

# pbnum _ convert number to string, push back on input
	subroutine pbnum(n)
	integer mod
	integer m, n, num
#	string digits "0123456789"
	integer digits(11)
	data digits(1) /DIG0/
	data digits(2) /DIG1/
	data digits(3) /DIG2/
	data digits(4) /DIG3/
	data digits(5) /DIG4/
	data digits(6) /DIG5/
	data digits(7) /DIG6/
	data digits(8) /DIG7/
	data digits(9) /DIG8/
	data digits(10) /DIG9/
	data digits(11) /EOS/

	num = n
	repeat {
		m = mod(num, 10)
		call putbak(digits(m+1))
		num = num / 10
		} until (num == 0)
	return
	end

# dosub _ select substring
	subroutine dosub(argstk, i, j)
	integer ctoi, length, max, min
	integer ap, argstk(ARGSIZE), fc, i, j, k, nc
	include cmacro

	if (j - i < 3)
		return
	if (j - i < 4)
		nc = MAXTOK
	else {
		k = argstk(i+4)
		nc = ctoi(evalst, k)		# number of characters
		}
	k = argstk(i+3)			# origin
	ap = argstk(i+2)			# target string
	fc = ap + ctoi(evalst, k) - 1	# first char of substring
	if (fc >= ap & fc < ap + length(evalst(ap))) {	# subarrays
		k = fc + min(nc, length(evalst(fc))) - 1
		for ( ; k >= fc; k = k - 1)
			call putbak(evalst(k))
		}
	return
	end

# doif _ select one of two arguments
	subroutine doif(argstk, i, j)
	integer equal
	integer a2, a3, a4, a5, argstk(ARGSIZE), i, j
	include cmacro

	if (j - i < 5)
		return
	a2 = argstk(i+2)
	a3 = argstk(i+3)
	a4 = argstk(i+4)
	a5 = argstk(i+5)
	if (equal(evalst(a2), evalst(a3)) == YES)	# subarrays
		call pbstr(evalst(a4))
	else
		call pbstr(evalst(a5))
	return
	end
