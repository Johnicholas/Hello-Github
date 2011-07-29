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

import java.io.*;
import java.util.LinkedList;

public class Main implements Runnable {
    public static int verboseLevel = 0;
    public static boolean gui = true;

    Visualizer visual;
    static long cost = 0;

    public byte[] readFully(InputStream is) throws IOException {
	byte[] buffer = new byte[4096];
	byte[] newbuffer;
	int offset = 0;
	while (true) {
	    int len = is.read(buffer, offset, buffer.length-offset);
	    if (len < 0) {
		newbuffer = new byte[offset];
		System.arraycopy(buffer,0,newbuffer,0,offset);
		is.close();
		return newbuffer;
	    }
	    offset += len;
	    if (offset == buffer.length) {
		newbuffer = new byte[2*buffer.length];
		System.arraycopy(buffer,0,newbuffer,0,offset);
		buffer = newbuffer;
	    }
	}
    }


    int[] matchlen  = new int[4096];
    DNA[] matches   = new DNA[4096];
    int numMatches;

    public boolean match(DNA pattern, DNA dna, DNA olddna) {
	LinkedList openmatches = new LinkedList();
	DNABuffer search = null;
	int offset = 0;
	while (true) {
	    int c = pattern.shift();
	    while (c != DNA.I) {
		/* unquote character */
		int unquoted = c-1;
		if (search != null) {
		    search.append((byte) unquoted);
		} else {
		    cost++;
		    if (dna.isEmpty() || dna.shift() != unquoted)
			return false;
		    offset++;
		}
		c = pattern.shift();
	    }
	    c = pattern.shift();
	    if (c == DNA.C) {
		if (search != null) {
		    search.append(DNA.P);
		} else {
		    cost++;
		    if (dna.isEmpty() || dna.shift() != DNA.P)
			return false;
		    offset++;
		}
		continue;
	    }
	    
	    if (search != null) {
		int skipped = dna.search(search);
		if (skipped < 0) {
		    cost += dna.length();
		    return false;
		}
		cost += skipped;
		offset += skipped;
		search = null;
	    }
	    switch (c) {
		case DNA.F:
		    pattern.shift();
		    search = new DNABuffer();
		    break;
		case DNA.P: {
		    int skip = pattern.parseNat();
		    offset += skip;
		    if (skip < 0 || !dna.skip(skip)) {
			return false;
		    }
		    break;
		}
		case DNA.I:
		    switch (pattern.shift()) {
			case DNA.I:
			    pattern.skip(7);
			    break;
			case DNA.C:
			case DNA.F: {
			    if (openmatches.size() == 0)
				return true;
			    DNA match = (DNA) openmatches.removeLast();
			    matches[numMatches] = match;
			    matchlen[numMatches++] = 
				match.length() - dna.length();
			    break;
			}
			case DNA.P:
			    openmatches.addLast(new DNA(dna));
			    break;
		    }
	    }
	}
    }

    public DNA matchReplace(DNA pattern, DNA template, DNA olddna) {
	DNA dna = new DNA(olddna);
	numMatches = 0;
	if (match(pattern, dna, olddna)) {
	    if (Main.verboseLevel > 1) {
		for (int i = 0; i < numMatches; i++)
		    System.err.println("  "+i+":"+
				       matches[i].substring(0,matchlen[i]));
	    }
// 	System.err.println("tail:"+dna);
	    dna = template.replace(matches, matchlen, numMatches, dna);
// 	System.err.println("result:"+dna);
	    if (Main.verboseLevel > 1) {
		System.err.println("  res:"+dna);
	    }
	} else {
	    dna = olddna;
	}
	/* clean up dna */
	while (numMatches-- > 0)
	    matches[numMatches] = null;
	return dna;
    }
    
    public DNA readDNA() {
	byte[] prefix, endo;
	try {
	    int i,j;
	    prefix = readFully(new FileInputStream("prefix.dna"));
	    j = 0;
	    for (i = 0; i < prefix.length; i++) {
		if (prefix[i] == 'I')
		    prefix[j++] = DNA.I;
		else if (prefix[i] == 'C')
		    prefix[j++] = DNA.C;
		else if (prefix[i] == 'F')
		    prefix[j++] = DNA.F;
		else if (prefix[i] == 'P')
		    prefix[j++] = DNA.P;
		else if (!Character.isWhitespace((char)prefix[i])) {
		    System.err.println("Prefix contains illegal character "
				       +(char) prefix[i]);
		    return null;
		}
	    }
	    if (j != i) {
		byte[] newprefix = new byte[j];
		System.arraycopy(prefix, 0, newprefix, 0, j);
		prefix = newprefix;
	    }
	} catch (FileNotFoundException ex) {
	    System.err.println("Please put a prefix into prefix.dna in the current directory.");
	    prefix = new byte[0];
	} catch (IOException ex) {
	    System.err.println("Can't read prefix.dna! The following exception was thrown: "+ex);
	    ex.printStackTrace();
	    return null;
	}
	try {
	    endo = readFully(getClass().getResourceAsStream("endo.dna"));
	    for (int i = 0; i < endo.length; i++) {
		if (endo[i] == 'I')
		    endo[i] = DNA.I;
		else if (endo[i] == 'C')
		    endo[i] = DNA.C;
		else if (endo[i] == 'F')
		    endo[i] = DNA.F;
		else if (endo[i] == 'P')
		    endo[i] = DNA.P;
		else {
		    System.err.println("endo.dna contains illegal byte "
				       +(int) endo[i]);
		    return null;
		}
	    }
	} catch (IOException ex) {
	    System.err.println("Can't read endo.dna! The following exception was thrown: "+ex);
	    ex.printStackTrace();
	    return null;
	}
	return new DNA(prefix).concat(new DNA(endo));
    }

    public void run() {
	DNA dna = readDNA();
	if (dna == null)
	    return;
// 	    System.err.println(dna);
	int iter = 0;
	int maxlen = dna.length();
	
	try {
	    while (!dna.isEmpty()) {
//   		    System.err.println(dna);
		if ((++iter % 100000) == 0) {
		    System.err.println("Iter: "+iter+
				       " Fragmentation: "+dna.fragmentation()+
				       " Maxlen: "+maxlen+
				       " Cost: "+cost);
		    }
		DNA pattern = dna.skipPattern(visual);
		cost += pattern.length();
		DNA template = dna.skipTemplate(visual);
		cost += template.length();

// 		if (pattern.pattern.toString().equals("(!(!))")
// 		    && pattern.patterndata[0] == 0x43ee11) {
// 		    DNA stack = dna.substring(0x7295a1, 48+48);
// 		    System.err.println(pattern);
// 		    System.err.println(stack);
// 		}
		
		if (verboseLevel > 0)
		    System.err.println(pattern+"->"+template);
		dna = matchReplace(pattern, template, dna);
		pattern = null;
		template = null;
		dna = dna.defragment();
		if (dna.length() > maxlen)
		    maxlen = dna.length();
	    }
	    visual.dumpImage();
	    System.err.println("Finished. Iterations: "+iter+
			       " Maxlen: "+maxlen+
			       " Cost: "+cost);
	} catch (FinishException ex) {
	    System.err.println("Aborted!  Iterations: "+iter+
			       " Maxlen: "+maxlen+
			       " Cost: "+cost);
		System.err.println(dna);
		ex.printStackTrace();
	}
    }

    public Main(String[] param) {
	for (int i = 0; i < param.length; i++) {
	    if (param[i].equals("-v"))
		verboseLevel++;
	    if (param[i].equals("-nogui"))
		gui = false;
	}
	visual = new Visualizer();
	if (gui) {
	    javax.swing.SwingUtilities.invokeLater(new Runnable() {
		    public void run() {
			visual.startGui();
		    }
		});
	}
	new Thread(this).start();
    }

    public static void main(String[] param) throws IOException {
	new Main(param);
    }
}
