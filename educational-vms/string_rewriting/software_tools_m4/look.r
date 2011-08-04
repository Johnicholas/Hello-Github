include globdefs
include /usr/style/newdef/defdefs
# lookup _ locate name, extract definition from table
	integer function lookup(name, defn)
	character defn(MAXDEF), name(MAXTOK)
	integer i, j, k
	include clook
	
	for (i = lastp; i > 0; i = i - 1) {
		j = namptr(i)
		for (k = 1; name(k) == table(j) & name(k) != EOS; k = k + 1)
			j = j + 1
		if (name(k) == table(j)) {		# got one
			call scopy(table, j+1, defn, 1)
			lookup = YES
			return
			}
		}
	lookup = NO
	return
	end

# instal _ add name and definition to table
	subroutine instal(name, defn)
	character defn(MAXTOK), name(MAXDEF)
	integer length
	integer dlen, nlen
	include clook

	nlen = length(name) + 1
	dlen = length(defn) + 1
	if (lastt + nlen + dlen > MAXTBL | lastp >= MAXPTR) {
		call putlin(name, ERROUT)
		call remark(": too many definitions.")
		}
	lastp = lastp + 1
	namptr(lastp) = lastt + 1
	call scopy(name, 1, table, lastt + 1)
	call scopy(defn, 1, table, lastt + nlen + 1)
	lastt = lastt + nlen + dlen
	return
	end


#block data
	block data
	include clook

	data lastp /0/
	data lastt /0/

	end
