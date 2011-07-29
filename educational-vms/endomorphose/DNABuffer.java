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

public final class DNABuffer {
    byte[] buff = new byte[64];
    int len = 0;

    public void clear() {
	len = 0;
    }

    public int length() {
	return len;
    }

    private void grow(int minlen) {
	int size = buff.length;
	do {
	    size = size*3/2;
	} while (size < minlen);
	byte[] newbuff = new byte[size];
	System.arraycopy(buff, 0, newbuff, 0, len);
	buff = newbuff;
    }
    
    public void append(byte b) {
	if (len >= buff.length)
	    grow(len+1);
	buff[len++] = b;
    }

    public void append(DNA dna, int newlen) {
	if (len+newlen > buff.length)
	    grow(len+newlen);
	dna = new DNA(dna);
	while (newlen-- > 0)
	    buff[len++] = dna.shift();
    }
}
