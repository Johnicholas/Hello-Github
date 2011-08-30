/* table.c
   Preprocessor for the Iowa Logic Simulator
   input:   A circuit specification containing truth tables
   output:  A circuit specification with tables replaced by hard-wired logic
   use:  table < input > output
         table input > output

   author:  Douglas W. Jones
   version 2: Feb 1,  2001 -- permit DOS style CRLF eol, rid of extra ; at end
   version 1: Nov 17, 1998

   copyright 1998, Douglas W. Jones, all rights reserved.  Permission is
	hereby given to make and distribute unlimited copies of this work,
	for any purpose, using any medium, and to use this work as a basis
	for derived works of any kind.  The only restriction on your
	right to copy or derive code from this work is that this copyright
	notice must be preserved in any copies or in any derived work.

   warrantee:  None.  This software is hackwork; it is poorly engineered
	and was developed in a hurry because the author wanted a prototype.
	Use it at your own risk.  Improve on it if you wish, and take the
	blame yourself for any errors in your improved version.

  Edited August 30, 2011, by Johnicholas Hines - exit-related compile error.
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define INFOLEN 8192
#define LINELEN 255
char line[LINELEN];	/* line of text */
int lineno;		/* current line's number */
int first, last;	/* first and last analyzed characters in line */
int col;		/* print column of line[last] */
int len;		/* position of last valid char on line */
int margin;		/* position of left margin of current table */

error( msg )
char * msg;
{
	fprintf( stderr, "\nline %d: %s\n",lineno, msg );
	exit(1);
}

FILE *infile; /* unit number from which to read */

getline()
/* fill line from infile, set first, last, len */
{
	int ch;
	int pos = 0;

	len = -1;
	for (;;) {
		ch = getc( infile );
		if (ch == '\n') break;
		if (ch == '\r') continue;
		if (ch == EOF) exit(0);
		if (pos == LINELEN) break;
		line[pos] = ch;
		len = pos;
		pos++;
	}

	/* no characters yet analyzed */
	lineno++;
	last = 0;
	first = 0;
	if (len >= 0) {
		col = 1;
	} else {
		col = 0;
	}
}

putline(first, last)
int first, last;
/* put line[first .. last] (inclusive) to stdout */
{
	int ch;
	int pos = first;

	for (;;) {
		if (pos > last) break;
		if (pos > len) break;
		ch = line[pos];
		putchar(ch);
		pos++;
	}
}

nextchar()
/* advance one character */
{
	if (last > len) return;
	if (line[last] == '\t') {
		col = ((col + 7) & ~7) + 1;
	} else {
		col++;
	}
	last++;
}

tabout(i)
int i;
/* output tabs and spaces to margin */
{
	int c = 1;
	int m = margin + i;
	while ((c + 8) <= m) {
		c = c + 8;
		putchar( '\t' );
	}
	while (c < m) {
		c++;
		putchar( ' ' );
	}
}

nonblank(e)
int e;
#define echo 1
#define noecho 0
/* scan to a nonblank character and skip comments; optionally echo skipped */
{
	for (;;) {
		/* done if end of line */
		if (last > len) break;

		/* done if end of line comment */
		if ((line[last] == '-')
		&&  (last < len)
		&&  (line[last+1] == '-')) {
			last = len + 1;
			break;
		}

                /* skip in-line curly brace comments */
                if (line[last] == '{') {
                        for (;;) {
                                nextchar();
                                while (last > len) { /* newline */
					if (e) {
						putline(first, last);
						putchar('\n');
					}
                                        getline();
                                }
                                if (line[last] == '}') break;
                        }
			/* we're sitting on } character */
			nextchar();
			/* go on and process the next character */
			continue;
                }

                /* skip in-line compound brackets */
                if ((line[last] == '(')
                &&  (last < len)
                &&  (line[last + 1] == '*')) {
                        nextchar(); /* skip the * also */
                        for (;;) {
                                nextchar();
                                while (len < last) {
					if (e) {
						putline(first, last);
						putchar('\n');
					}
                                        getline();
                                }
                                if ((line[last] == '*')
                                &&  (last < len)
                                &&  (line[last + 1] == ')')) {
                                        nextchar();
                                        break;
                                }
                        }
			/* we're sitting on the / character */
			nextchar;
			/* go on and process the next character */
                        continue;
                }

		if (!isspace(line[last])) break;  /* nonblank */
		nextchar();
	}
}

clearmargin()
/* verify that line is clear to indenting margin */
{
	for (;;) {
		getline();
		nonblank(noecho); /* this skips multiline comments */
		if (len < last) continue; /* skip blank lines */
		if (col >= margin) break; /* reached margin */
		if (!isspace(line[last])) break;  /* found nonblank */
	}
	if (col < margin) error( "bad table indenting" );
}

comment( i, msg )
/* generate a comment to the output, indented i spaces from margin */
int i;
char * msg;
{
	tabout(i);
	fputs( "{ ", stdout );
	fputs( msg, stdout );
	fputs( " }\n", stdout );
}

int numval; /* value of number returned from getnum */

getnum()
/* consume integer */
{
	char ch;
	if (last > len) error( "number expected" );
	ch = line[last];
	if (!isdigit(ch)) error( "bad digit in number" );
	numval = ch - '0';
	for (;;) {
		nextchar();
		if (last > len) break;
		ch = line[last];
		if (!isdigit(ch)) break;
		numval = (numval * 10) + (ch - '0');
	}
}

int idfirst, idlast;  /* bounds of id returned from getid */

getid()
/* consume identifier */
{
	if (last > len) error( "identifier expected" );
	if (!isalpha(line[last])) error( "bad identifier" );
	idfirst = last;
	nextchar();
	while ((last <= len) && (isalnum(line[last]))) {
		nextchar();
	}
	idlast = last - 1;
}

int pinsub;	   /* 0 if no range, else sign tells order of range */
int pinmin, pinmax;  /* if idsub nonzero, min and max subscripts */

getpin()
/* consume identifier */
{
	pinsub = 0;
	getid();
	if (last > len) return;
	if (line[last] == '(') {
		nextchar();
		nonblank();
		getnum();
		pinmin = numval;
		nonblank();
		if (((last+1) > len)
		||  (line[last] != '.')
		||  (line[last+1] != '.')) error( ".. expected" );
		nextchar();
		nextchar();
		nonblank();
		getnum();
		if (pinmin < numval) {
			pinmax = numval;
			pinsub = 1;
		} else {
			pinmax = pinmin;
			pinmin = numval;
			pinsub = -1;
		}
		nonblank();
		if ((last > len)
		||  (line[last] != ')')) error( "missing )" );
		nextchar();
	}
}

int getentry()
/* consume table entry, return canonical 0, 1 or - value */
{
	if (last > len) error( "table entry expected" );
	if ((line[last] == '0') || (line[last] == 'L')) {
		nextchar();
		return '0';
	} else if ((line[last] == '1') || (line[last] == 'H')) {
		nextchar();
		return '1';
	} else if ((line[last] == '-')
	       ||  (line[last] == 'x')
	       ||  (line[last] == 'X')) {
		nextchar();
		return '-';
	} else {
		error( "0 1 or - table entry expected" );
	}
}

int rows, inputs, outputs; /* dimensions of table */
char info[INFOLEN];	   /* saved table information */
int ip;			   /* index used to save data in info */
int tableip;		   /* ip value for start of table */
int indx[LINELEN];	   /* index into info for input pins */
char inneg[LINELEN];	   /* has this input been negated in any row? */
int insub[LINELEN];	   /* has this input a subscript? */
int outdx[LINELEN];	   /* index into info for output pins */
int outsub[LINELEN];	   /* has this output a subscript? */
int outnz[LINELEN];	   /* how many ones for this output pin */
int outnzt[LINELEN];	   /* temp copy of outnz needed to gen wire list */
int rownz[INFOLEN/2];	   /* how many ones for this row */
int parts;		   /* do we need any parts? */

#define NOSUB (1 << (sizeof( int ) - 1))

/* info begins with blank delimited list of i/o pin identifiers
   info[tableip] is first entry for a table row
   indx 1..inputs point to the input pin identifiers in info
   outdx 1..outputs point to the output pin identifiers in info
   insub[i] and outsub[i] tell subscript value for input or output pin
   insub[i] or outsub[i] equal NOSUB indicates no subscript
   inneg[i] = 1 if input i is ever negated, 0 if not
   outnz[i] counts number of ones in column for each output
   rownz[i] counts number of ones or zeros in input for each row
*/

putinfo(ch)
char ch;
{
	if (ip >= INFOLEN) error( "table too big" );
	info[ip] = ch;
	ip++;
}

putid(ip)
int ip;
/* output identifier starting at ip in info */
{
	int i;
	for (i = ip; info[i] != ' '; i++) {
		putchar( info[i] );
	}
}

putpin( ip, subscr )
int ip;
int subscr;
/* output identifier and subscript for I/O pin */
{
	putid(ip);
	if (subscr != NOSUB) printf( "(%d)", subscr );
}

definpin( subscr )
int subscr;
/* define input pin */
{
	inputs++;
	if (inputs > LINELEN) error(" too many inputs ");
	indx[inputs] = ip; /* save ptr to identifier */
	insub[inputs] = subscr;
}

defoutpin( subscr )
int subscr;
/* define output pin */
{
	outputs++;
	if (outputs > LINELEN) error(" too many outputs ");
	outdx[outputs] = ip; /* save ptr to identifier */
	outsub[outputs] = subscr;
	outnz[outputs] = 0;
}

int timedel;	/* does this circuit have a time delay specified */

getbody()
/* consume table body */
{
	int divide; /* column dividing inputs from outputs */

	rows = 0;
	inputs = 0;
	outputs = 0;
	parts = 0;
	ip = 0;

	/* header issues, define time delays if needed */
	if (timedel) {
		tabout(2);
		fputs( "time GTD = TD * 0.2;\n", stdout );
		tabout(2);
		fputs( "time WTD = TD * 0.1;\n", stdout );
		/* total time delay
			= inwire + not + andwire + and + orwire + or + outwire
			= 4 * WTD + 3 * GTD = 0.4 * TD + 0.6 * TD = TD
		*/
	}

	/* first, read inputs from header */
	tabout(2);
	fputs( "inputs", stdout );
	clearmargin();
	nonblank(noecho);
	if (len < last) error( "table header expected" );
	if (line[last] == '|') error( "inputs expected in table header" );
	getpin();
	for (;;) {
		int i;

		/* put pin info in input table */
		if (pinsub == 0) { /* just one pin */
			definpin( NOSUB );
		} else if (pinsub > 0) {
			for (i = pinmin; i <= pinmax; i++) definpin( i );
		} else /* pinsub < 0 */ {
			for (i = pinmax; i >= pinmin; i--) definpin( i );
		}
		for (i = idfirst; i <= idlast; i++) putinfo( line[i] );
		putinfo( ' ' ); /* put delimiter */

		/* put out pin name in input list */
		putchar( ' ' );
		putid( indx[inputs] );
		if (pinsub != 0) {
			printf( "(%d .. %d)", pinmin, pinmax );
		}
		nonblank(noecho);
		if (len < last) break;            /* end of line */
		if (line[last] == '|') break;     /* input/output divide */
		getpin();
		putchar( ',' );
	}
	if (len < last) error( "| expected after input list" );
	fputs( ";\n", stdout );
	divide = col;
	nextchar(); /* skip | */

	/* then, read outputs from header */
	tabout(2);
	fputs( "outputs", stdout );
	nonblank(noecho);
	if (len < last) error( "outputs expected in table header" );
	getpin();
	for (;;) {
		int i;

		/* put pin info in output table */
		if (pinsub == 0) { /* just one pin */
			defoutpin( NOSUB );
		} else if (pinsub > 0) {
			for (i = pinmin; i <= pinmax; i++) defoutpin( i );
		} else /* pinsub < 0 */ {
			for (i = pinmax; i >= pinmin; i--) defoutpin( i );
		}
		for (i = idfirst; i <= idlast; i++) putinfo( line[i] );
		putinfo( ' ' ); /* put delimiter */

		/* put out pin name in output list */
		putchar( ' ' );
		putid( outdx[outputs] );
		if (pinsub != 0) {
			printf( "(%d .. %d)", pinmin, pinmax );
		}
		nonblank(noecho);
		if (len < last) break;            /* end of line */
		getpin();
		putchar( ',' );
	}
	fputs( ";\n", stdout );

	/* setup */
	{
		int i;
		for (i = 1; i <= inputs; i++) inneg[i] = 0;
	}

	/* now consume actual array */
	tableip = ip;
	for (;;) {
		int i; /* used to count inputs or output on each row */
		int rp; /* used to remember start of inputs on this row */
		int nz; /* used to count nonzeros */

		clearmargin();
		if ((line[last] == 'e')
		&&  ((last + 2) <= len)
		&&  (line[last + 1] == 'n')
		&&  (line[last + 2] == 'd')) break;

		rows++;
		rownz[rows] = 0;

		rp = ip; /* remember where this row starts */
		i = 0;
		for (;;) {
			nonblank(noecho);
			if (len < last) break;        /* end of line */
			if (line[last] == '|') break; /* input/output divide */
			i++;
			putinfo( getentry() );
		}
		if (len < last) error( "| expected after input list" );
		if (col != divide) error( "| not aligned in correct column" );
		if (i < inputs) error( "missing inputs in row" );
		if (i > inputs) error( "extra inputs in row" );
		nextchar(); /* skip | */

		nz = 0;
		i = 0;
		for (;;) {
			int e;
			nonblank(noecho);
			if (len < last) break;        /* end of line */
			i++;
			e = getentry();
			if (e == '1') {
				nz++;
				outnz[i]++;
			}
			putinfo( e );
		}
		if (i < outputs) error( "missing outputs in row" );
		if (i > outputs) error( "extra outputs in row" );
		if (nz == 0) { /* there were no nonzero outputs this row */
			ip = rp; /* forget this row! */
		} else { /* this row is to be counted */
			/* see if any inputs need inversion on this row */
			for (i = 1; i <= inputs; i++) { /* each input */
				if (info[rp + i - 1] == '0') inneg[i] = 1;
				if (info[rp + i - 1] != 'x') rownz[rows]++;
			}
			if (rownz[rows] > 0) parts = 1;
		}
	}
	nextchar(); /* skip to n */
	nextchar(); /* skip to d */
	nextchar(); /* skip beyond d */
	first = last;

	if (parts) { /* we need parts */
		int i;
	 	tabout(2);
		fputs( "parts\n", stdout );
		comment( 4, "inverters for inputs (if needed)" );
		for (i = 1; i <= inputs; i++) {
			if (inneg[i]) {
	 			tabout(4);
				printf( "IN%dBAR: not", i );
				if (timedel) fputs( "(GTD)", stdout );
				fputs( ";\n", stdout );
			}
		}
		comment( 4, "and gates for each table row" );
		for (i = 1; i <= rows; i++) {
			if (rownz[i] > 0) {
	 			tabout(4);
				printf( "ROW%d: and(%d", i, rownz[i] );
				if (timedel) fputs( ",GTD", stdout );
				fputs( ");\n", stdout );
			}
		}
		comment( 4, "or gates for each output column" );
		for (i = 1; i <= outputs; i++) {
			if (outnz[i] > 0) {
	 			tabout(4);
				printf( "OUT%d: or(%d", i, outnz[i] );
				if (timedel) fputs( ",GTD", stdout );
				fputs( ");\n", stdout );
			}
			outnzt[i] = outnz[i]; /* make copy for wire listing */
		}
	}

	{
		int i, j;
	 	tabout(2);
		fputs( "wires\n", stdout );
		comment( 4, "inputs to input inverters (if needed)" );
		for (i = 1; i <= inputs; i++) {
			if (inneg[i]) {
				tabout(4);
				putpin( indx[i], insub[i] );
				fputs( " to", stdout );
				if (timedel) fputs( "(WTD)", stdout );
				printf( " IN%dBAR.in;\n", i );
			}
		}
		ip = tableip;
		comment( 4, "the matrix wiring, arranged row by row" );
                for (i = 1; i <= rows; i++) { /* the matrix, and half first */
			int rownzt;
			rownzt = rownz[i];
                        if (rownzt > 0) { /* the expected case */
				for (j = 1; j <= inputs; j++) {
					if (info[ip] == '1') {
						tabout(4);
						putpin( indx[j], insub[j] );
					} else if (info[ip] == '0') {
						tabout(4);
						printf( "IN%dBAR.out", j );
					}
					if ((info[ip] == '1')
					||  (info[ip] == '0')) {
						fputs( " to", stdout );
						if (timedel)
							fputs("(WTD)",stdout);
						printf( " ROW%d.in", i );
						if (rownzt > 1) {
							printf( "(%d)",
								rownz[i] );
						}
						fputs( ";\n", stdout );
						rownz[i]--;
					}
					ip++;
				}
				for (j = 1; j <= outputs; j++) {
					if (info[ip] == '1') {
						tabout(6);
						printf( "ROW%d.out to", i );
						if (timedel)
							fputs("(WTD)",stdout);
						printf(" OUT%d.in", j );
						if (outnz[j] > 1) {
							printf( "(%d)",
								outnzt[j] );
						}
						fputs( ";\n", stdout );
						outnzt[j]--;
					}
					ip++;
				}
			}
                }
		comment( 4, "wires needed to connect outputs" );
		for (i = 1; i <= outputs; i++) { /* output pins */
			tabout(4);
			if (outnz[i]) {
				printf( "OUT%d.out to", i );
			} else {
				fputs( "low to", stdout );
			}
			if (timedel) fputs("(WTD)",stdout);
			putchar( ' ' );
			putpin( outdx[i], outsub[i] );
			fputs( ";\n", stdout );
		}
	}

	comment( 2, "end of automatically generated text" );
	tabout(0);
	printf( "end{=%d=}", lineno );
}

searchline()
/* search for keyword table in line */
{
	for (;;) {
		/* try the next non-comment character */
		nonblank(echo);

		/* see if done on this line */
		if (last > len) break;

		/* try for keyword table */
		if ((line[last] == 't')
		&&  ((last + 4) < len)
		&&  (line[last+1] == 'a')
		&&  (line[last+2] == 'b')
		&&  (line[last+3] == 'l')
		&&  (line[last+4] == 'e')
		&&  (isspace(line[last+5]))) {
			/* we do have the keyword! */
			margin = col;
			putline(first, last - 1);
			nextchar(); /* skip to a */
			nextchar(); /* skip to b */
			nextchar(); /* skip to l */
			nextchar(); /* skip to e */
			nextchar(); /* skip to blank (we hope) */

			/* put out circuit! */
			fputs( "circuit ", stdout );

			/* get and output the table name */
			nonblank(noecho);
			getid();
			putline (idfirst, idlast);

			/* see if parameterized */
			timedel = 0;  /* assume not */
			nonblank(noecho);
			if ((last <= len)
			&&  (line[last] == '(')) {
				nextchar(); /* skip open paren */
				nonblank(noecho);
				getid();
				if ((line[idfirst] != 't')
				||  ((idlast - idfirst) != 3)
				||  (line[idfirst + 1] != 'i')
				||  (line[idfirst + 2] != 'm')
				||  (line[idfirst + 3] != 'e'))
					error( "time expected" );
				nonblank(noecho);
				if ((last > len)
				||  (line[last] != ')')) error( ") expected" );
				nextchar(); /* skip close paren */
				timedel = 1; /* remember time delay present */
				fputs( "( time TD )", stdout);
			}
			
			/* end header line */
			fputs( ";\n", stdout );

			/* put comment */
			comment( 2, "generated by table preprocessor" );

			getbody();
		} else {
			nextchar();	/* it wasn't the word table */
		}
	}
}

main(argc, argv)
int argc;
char *argv[];
{
	lineno = 0;
	infile = stdin; /* default */
	if (argc > 2) error( "too many command line arguments" );
	if (argc > 1) {
		infile = fopen( &argv[1][0], "r" );
		if (infile == NULL) error( "cannot open input file" );
	}
	for (;;) {
		getline();
		searchline();
		putline(first, last);
		putchar('\n');
	}
}
