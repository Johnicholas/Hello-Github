/* Copyright 2007 by Jochen Hoenicke
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id$
 */

public final class DNA {
    public static final DNA EMPTY = new DNA(new byte[0],0,0,null);

    public static final int MERGE_SIZE = 256;
    public static final byte I = 0;
    public static final byte C = 1;
    public static final byte F = 2;
    public static final byte P = 3;

    private byte[] data;
    private int offset;
    private int end;
    private int nexttotal;
    private /*@nullable@*/ DNA next;

    /*@ private invariant data != null && 
      @     0 <= offset && offset <= end && data.length >= end &&
      @     nexttotal >= 0;
      @ private invariant offset == end ==> next == null;
      @ private invariant next == null ==> nexttotal == 0;
      @ private invariant next != null ==> 
      @               nexttotal == next.nexttotal + next.end - next.offset;
      @*/

    /*@ requires 0 <= o && o <= e;
      @ requires content != null && content.length >= e;
      @*/
    private /*@helper@*/ DNA (byte[] content, int o, int e) {
	this.data = content;
	this.offset = o;
	this.end = e;
    }

    /*@ requires 0 <= o && o <= e;
      @ requires content != null && content.length >= e;
      @*/
    private DNA (byte[] content, int o, int e, /*@nullable@*/ DNA next) {
	this.data = content;
	this.offset = o;
	this.end = e;
	this.nexttotal = next == null ? 0 : next.length();
	this.next = next;
    }

    public DNA (DNABuffer sb) {
	data = new byte[sb.len];
	System.arraycopy(sb.buff, 0, data, 0, sb.len);
	offset = 0;
	end = sb.len;
	nexttotal = 0;
	next = null;
    }

    public DNA (byte[] content) {
	this(content,0, content.length, null);
    }

    public DNA(DNA old) {
	data   = old.data;
	offset = old.offset;
	end    = old.end;
	next   = old.next;
	nexttotal  = old.nexttotal;
    }

    public boolean isEmpty() {
	return length() == 0;
    }

    public /*@pure@*/ int length() {
	return end - offset + nexttotal;
    }

    /*@ ensures this.length() == \old(this.length());
      @ ensures this.length() >= start
      @         ==> \result.length() == this.length() - start;
      @ ensures this.length() < start ==> \result.length() == 0;
      @*/
    public DNA substring(int start) {
	DNA dna = this;
	while (dna != null && dna.offset + start >= dna.end) {
	    start -= dna.end - dna.offset;
	    dna = dna.next;
	}
	if (dna == null)
	    return EMPTY;
	if (start == 0)
	    return dna;
	return new DNA(dna.data, dna.offset+start, dna.end, dna.next);
    }

    /*@ requires start >= 0 && len >= 0 && start+len >= 0 &&
      @          start + len <= this.length();
      @ ensures this.length() == \old(this.length());
      @ ensures \result.length() == len;
      @*/
    public DNA substring(int start, int len) {
	return substring(start).substrconcat(len, EMPTY);
    }

    /*@ requires this.length() > 0;
      @ ensures this.length() == \old(this.length())- 1;
      @*/
    public byte shift() {
	while (offset == end) {
	    data      = next.data;
	    offset    = next.offset;
	    end       = next.end;
	    nexttotal = next.nexttotal;
	    next      = next.next;
	}
	return data[offset++];
    }

    /*@ ensures \result ==> this.length() == \old(this.length()) - number;
      @ ensures \result <==> \old(this.length()) >= number;
      @*/
    public boolean skip(int number) {
	offset += number;
	if (offset <= end)
	    return true;
	int newlen = nexttotal + end - offset;
	if (newlen <= 0) {
	    offset = end;
	    next = null;
	    nexttotal = 0;
	    return newlen == 0;
	}
	number = offset - end;
	DNA dna = next;
	while (number >= dna.end - dna.offset) {
	    number -= dna.end - dna.offset;
	    dna = dna.next;
	}
	this.data      = dna.data;
	this.offset    = dna.offset + number;
	this.end       = dna.end;
	this.next      = dna.next;
	this.nexttotal = dna.nexttotal;
	return true;
    }

    public int search(DNABuffer s) {
// 	System.err.println("Search: "+new DNA(s)+" in "+this);
	DNA dna = this;
	int pos = this.offset;
	int skipped = 0;
	int slen = s.length();
	if (slen == 0)
	    return 0;
	byte c = s.buff[0];
    outer:
	while (true) {
	    while (pos < dna.end && dna.data[pos] != c) {
		pos++;
		skipped++;
	    }
	    if (pos == dna.end) {
		dna = dna.next;
		if (dna == null)
		    return -1;
		pos = dna.offset;
	    } else {
		int npos = pos+1;
		DNA ndna = dna;
		for (int i = 1; i < slen; i++) {
		    if (npos == ndna.end) {
			ndna = ndna.next;
			if (ndna == null)
			    return -1;
			npos = ndna.offset;
		    }
		    if (s.buff[i] != ndna.data[npos]) {
			pos++;
			skipped++;
			continue outer;
		    }
		    npos++;
		}
		if (npos == ndna.end && ndna.next != null) {
		    ndna = ndna.next;
		    npos = ndna.offset;
		}
		this.data = ndna.data;
		this.offset = npos;
		this.end = ndna.end;
		this.next = ndna.next;
		this.nexttotal = ndna.nexttotal;
// 		System.err.println("Found at "+skipped+":"+this);
		return skipped+slen;
	    }
	}
    }

    /*@ requires firstlen >= 0 && firstlen <= this.length();
      @ requires firstlen + second.length() >= 0;
      @ ensures this.length() == \old(this.length());
      @ ensures second.length() == \old(second.length());
      @ ensures \result.length() == firstlen + second.length();
      @*/
    public DNA substrconcat(int firstlen, DNA second) {
	if (firstlen == 0)
	    return second;
	if (firstlen == this.length() && second.isEmpty())
	    return this;
	while (second.offset == second.end && second.next != null)
	    second = second.next;
	DNA first = this;
	DNA result = new DNA(data, offset, end);
	DNA tail = result;
	while (firstlen > first.end - first.offset) {
	    firstlen -= first.end - first.offset;
	    tail.nexttotal = firstlen + second.length();

	    first = first.next;
	    tail.next = new DNA(first.data, first.offset, first.end);
	    tail = tail.next;
	}
	tail.end = first.offset + firstlen;
	if (tail.data == second.data && tail.end == second.offset) {
	    tail.end = second.end;
	    second = second.next;
	} else if (second.end - second.offset > 0
		   && firstlen + second.end - second.offset < 2*MERGE_SIZE) {
	    /* merge tail with second */
	    tail.data = new byte[firstlen + second.end - second.offset];
	    System.arraycopy(first.data, first.offset, tail.data, 0, firstlen);
	    System.arraycopy(second.data, second.offset, tail.data, firstlen,
			     second.end - second.offset);
	    tail.offset = 0;
	    tail.end = firstlen + second.end - second.offset;
	    second = second.next;
	}
	if (second == null || second.length() == 0) {
	    tail.nexttotal = 0;
	    tail.next = null;
	} else {
	    tail.nexttotal = second.length();
	    tail.next = second;
	}
	return result;
    }

    /*@ ensures this.length() == \old(this.length());
      @ ensures second.length() == \old(second.length());
      @ ensures \result.length() == this.length() + second.length();
      @*/
    public DNA concat(DNA second) {
	return substrconcat(this.length(), second);
    }

    /*@ requires this.length() >= 7;
      @ ensures this.length() == \old(this.length())- 7;
      @*/
    public int shiftRNA() throws FinishException {
	int cmd = 0;
	for (int i = 0; i < 7; i++) {
	    if (offset == end) {
		data = next.data;
		offset = next.offset;
		end = next.end;
		nexttotal = next.nexttotal;
		next = next.next;
	    }
	    byte c = data[offset++];
	    cmd = 4*cmd + c;
	}
	return cmd;
    }

    public String toString() {
	DNA dna = this;
	int fragments = 0;
	while (dna != null) {
	    fragments++;
	    dna = dna.next;
	}
	return "("+fragments+")"+toString(30);
    }

    private static void
    appendDNAString(StringBuffer sb, byte[] data, int offset, int len) {
	while (len-- > 0)
	    sb.append("ICFP".charAt(data[offset++]));
    }

    /*@
      @ assignable objectState;
      @*/
    public String toString(int depth) {
	if (depth == 0)
	    return "...";
	StringBuffer sb = new StringBuffer();
	sb.append(data).append(',').append(offset).append(',').append(end-offset);
	sb.append(',').append(length());
	if (end > offset) {
	    int len = end-offset;
	    if (depth < 30 && len > 25) {
		sb.append('=');
		appendDNAString(sb, data, offset, 4);
		sb.append("..");
		appendDNAString(sb, data, end-4, 4);
	    } else if (len > 100) {
		appendDNAString(sb.append('='),data,offset,80);
		sb.append("..");
		appendDNAString(sb, data, end-4, 4);
	    } else {
		sb.append('=');
		appendDNAString(sb, data, offset,len);
	    }
	}
	sb.append("]");
	if (next != null) {
	    sb.append(" + ").append(next.toString(depth-1));
	}
	return sb.toString();
    }

    public int fragmentation() {
	DNA dna = this;
	int fragments = 0;
	while (dna != null) {
	    fragments++;
	    dna = dna.next;
	}
	return fragments;
    }

    public DNA defragment() {
	int fragments = 0;
	DNA dna = this;
	while (dna != null) {
	    fragments++;
	    dna = dna.next;
	}
	if (fragments > 50) {
	    System.err.println("defragment: "+fragments);
	    byte[] newbuff = new byte[length()];
	    dna = this;
	    int off = 0;
	    while (dna != null) {
		System.arraycopy(dna.data, dna.offset, newbuff, off,
				 dna.end - dna.offset);
		off += dna.end - dna.offset;
		dna = dna.next;
	    }
	    this.data = newbuff;
	    this.offset = 0;
	    this.end = off;
	    this.next = null;
	    this.nexttotal = 0;
	    return this;
	}
	return this;
    }

    public int parseNat() {
	int base = 1;
	int nat  = 0;
	while (true) {
	    while(offset < end) {
		int c = data[offset++];
		if (c == DNA.P) {
		    return nat;
		}
		if (c == DNA.C)
		    nat += base;
		base += base;
	    }
	    data      = next.data;
	    offset    = next.offset;
	    end       = next.end;
	    nexttotal = next.nexttotal;
	    next      = next.next;
	}    
    }

    /**
     * Skips the pattern in front of the dna.
     * @return the dna encoding the pattern.
     * @throws FinishException if dna ends before pattern is finished
     */
    public DNA skipPattern(Visualizer visual) throws FinishException {
	DNA pattern = new DNA(this);
	int parenlvl = 0;
	int state = 0;
	while (true) {
	    while (offset < end) {
		int c = data[offset++];
		switch (state) {
		    case 0:
			while (c != I && offset < end)
			    c = data[offset++];
			if (c == I)
			    state = 1;
			break;
		    case 1:
			if (c == I)
			    state = 2;
			else if (c == P)
			    state = 3;
			else if (c == F)
			    state = 4;
			else
			    state = 0;
			break;
		    case 2:
			if (c == F || c == C) {
			    if (parenlvl-- == 0) {
				int pattlen = pattern.length() - length();
				DNA dna = pattern;
				while (pattlen > dna.end - dna.offset) {
				    pattlen -= dna.end - dna.offset;
				    dna.nexttotal = pattlen;
				    dna = dna.next;
				}
				dna.end = dna.offset + pattlen;
				dna.nexttotal = 0;
				dna.next = null;
				return pattern;
			    }
			} else if (c == P) {
			    parenlvl++;
			} else if (c == I) {
			    /* deliver RNA immediately */
			    visual.addRNA(shiftRNA());
			}
			state = 0;
			break;
		    case 3:
			while (c != P && offset < end)
			    c = data[offset++];
			if (c == P)
			    state = 0;
			break;
		    case 4:
			state = 0;
			break;
		}
	    }
	    if (next == null)
		throw new FinishException();
	    data      = next.data;
	    offset    = next.offset;
	    end       = next.end;
	    nexttotal = next.nexttotal;
	    next      = next.next;
	}    
    }


    /**
     * Skips the template in front of the dna.
     * @return the dna encoding the template.
     * @throws FinishException if dna ends before template is finished
     */
    public DNA skipTemplate(Visualizer visual) throws FinishException {
	DNA pattern = new DNA(this);
	int state = 0;
	while (true) {
	    while (offset < end) {
		int c = data[offset++];
		switch (state) {
		    case 0:
			while (c != I && offset < end)
			    c = data[offset++];
			if (c == I)
			    state = 1;
			break;
		    case 1:
			if (c == I)
			    state = 2;
			else if (c == P || c == F)
			    state = 3;
			else
			    state = 0;
			break;
		    case 2:
			if (c == F || c == C) {
			    int pattlen = pattern.length() - length();
			    DNA dna = pattern;
			    while (pattlen > dna.end - dna.offset) {
				pattlen -= dna.end - dna.offset;
				dna.nexttotal = pattlen;
				dna = dna.next;
			    }
			    dna.end = dna.offset + pattlen;
			    dna.nexttotal = 0;
			    dna.next = null;
			    return pattern;
			} else if (c == P) {
			    state = 4;
			} else if (c == I) {
			    /* deliver RNA immediately */
			    visual.addRNA(shiftRNA());
			    state = 0;
			}
			break;
		    case 3:
			while (c != P && offset < end)
			    c = data[offset++];
			if (c == P)
			    state = 4;
			break;
		    case 4:
			while (c != P && offset < end)
			    c = data[offset++];
			if (c == P)
			    state = 0;
			break;
		}
	    }
	    if (next == null)
		throw new FinishException();
	    data      = next.data;
	    offset    = next.offset;
	    end       = next.end;
	    nexttotal = next.nexttotal;
	    next      = next.next;
	}
    }

    static class DNASequence {
	DNA dna;
	int len;
	DNASequence prev;

	public DNASequence(DNASequence prev, DNA dna, int len) {
	    this.prev = prev;
	    this.dna = dna;
	    this.len = len;
	}

	public DNA compose() {
	    DNA result = dna;
	    if (len < dna.length())
		result = result.substring(0, len);
	    DNASequence ptr = prev;
	    while (ptr != null) {
		result = ptr.dna.substrconcat(ptr.len, result);
		ptr = ptr.prev;
	    }
	    return result;
	}
	    
    }
    
    private void asnat(DNABuffer sb, int nat) {
	while (nat != 0) {
	    sb.append((byte)(nat & 1));
	    nat >>>=1;
	}
	sb.append(DNA.P);
    }

    private void quote(DNABuffer sb, int c) {
	while (c > DNA.P) {
	    quote(sb, c - 4);
	    c -= 3;
	}
	sb.append((byte)c);
    }

    private void protect(DNABuffer sb, DNA dna, int len, int level) {
	Main.cost -= sb.length();
	dna = new DNA(dna);
	while (len-- > 0)
	    quote(sb, dna.shift() + level);
	Main.cost += sb.length();
    }

    /**
     * Interpret the current DNA as pattern and return the result of applying
     * this pattern with the given matches and the tail.
     */
    public DNA replace(DNA[] matches, int[] matchlens, int numMatches,
		       DNA tail) {
	DNASequence dnaseq = null;
	DNABuffer sb = new DNABuffer();
    loop:
	while(true) {
	    while (offset < end) {
		int c = data[offset++];
		if (c != I) {
		    sb.append((byte) (c-1));
		    continue;
		} else if (offset < end && data[offset] == C) {
		    offset++;
		    sb.append(P);
		    continue;
		}

		c = shift();
		switch (c) {
		case DNA.C:
		    sb.append(DNA.P);
		    break;
		case DNA.F:
		case DNA.P: {
		    int quotes = parseNat();
		    int matchnr = parseNat();
		    if (matchnr < 0
			|| matchnr >= numMatches
			|| matchlens[matchnr] == 0) {
			/* empty match */
			break;
		    }
		    DNA match = matches[matchnr];
		    int matchlen = matchlens[matchnr];
		    if (quotes < 0 || quotes > 88) {
			System.err.println
			    ("Ignoring ridiculously large quote level: "
			     +quotes);
			quotes = 0;
		    }
		    if (quotes > 0) {
			protect(sb, match, matchlen, quotes);
		    } else if (sb.length() > 0
			       && match.length() < DNA.MERGE_SIZE) {
			sb.append(match, matchlen);
		    } else {
			int len = sb.length();
			    if (len > 0)
				dnaseq = new DNASequence(dnaseq, new DNA(sb), len);
			    dnaseq = new DNASequence(dnaseq, match, matchlen);
			    sb.clear();
		    }
		    break;
		}
		case DNA.I:
		    c = shift();
		    switch (c) {
		    case DNA.I:
			skip(7);
			break;
		    case DNA.C:
		    case DNA.F:
			break loop;
		    case DNA.P:{
			int matchnr = parseNat();
			if (matchnr < 0
			    || matchnr >= numMatches) {
			    /* empty match */
			    asnat(sb, 0);
			} else {
			    asnat(sb, matchlens[matchnr]);
			}
		    }
		    }
		}
	    }
	    data      = next.data;
	    offset    = next.offset;
	    end       = next.end;
	    nexttotal = next.nexttotal;
	    next      = next.next;
	}
	if (sb.length() > 0)
	    dnaseq = new DNASequence(dnaseq, new DNA(sb), sb.length());
	dnaseq = new DNASequence(dnaseq, tail, tail.length());
	return dnaseq.compose();
    }

}
