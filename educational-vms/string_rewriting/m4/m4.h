/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */
/*	Copyright (c) 1988 AT&T	*/
/*	  All Rights Reserved  	*/


/*
 * Copyright 2002 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

/*	from OpenSolaris "m4.h	6.13	05/06/08 SMI"	*/

/*
 * Portions Copyright (c) 2005 Gunnar Ritter, Freiburg i. Br., Germany
 *
 * Sccsid @(#)m4.h	1.4 (gritter) 12/25/06
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <limits.h>

#include <wchar.h>
#include <wctype.h>

// end of string is a wide zero
#define	EOS ((wchar_t)0)
#define	MAXSYM	5

#define	PUSH	1
#define	NOPUSH	0

#define	OK	0
#define	NOT_OK	1

#define	BUILTIN	0x40000000
#define	INVALID_CHAR 0x80000000

// x is builtin if it's builtin bit is on
// use builtin to set it on
#define	builtin(x) ((x) | BUILTIN)
#define	builtin_idx(x)	((x) & (wchar_t)~BUILTIN)
#define	is_builtin(x)	((x) != WEOF && ((x) & BUILTIN))

/*
 * Since we have expanded char to wchar_t, large value(has BUILTIN set)
 * can be given to the ctype macros. First check BUILTIN, and return
 * FALSE if it was set. EOF/WEOF will be in this case.
 */
#define	is_ascii(x)	(((x) & ~(wchar_t)0177) == 0)
#define	is_alpha(x)	(!is_builtin(x) && \
				(wide ? iswalpha(x) : isalpha(x)))
#define	is_alnum(x)	(!is_builtin(x) && \
				(wide ? iswalnum(x) : isalnum(x)))
#define	is_space(x)	(!is_builtin(x) && \
				(wide ? iswspace(x) : isspace(x)))
#define	is_digit(x)	(!is_builtin(x) && is_ascii(x) && isdigit(x))


// a bs is a pair of a function pointer and a name
struct bs {
  void (*bfunc)(wchar_t **, int);
  wchar_t* bname;
};

struct call {
  wchar_t** argp;
  int plev;
};

struct nlist {
  wchar_t* name;
  wchar_t* def;
  char tflag;
  struct nlist* next;
};

struct Wrap {
  wchar_t* wrapstr;
  struct Wrap* nxt;
};

typedef struct {
  unsigned char buffer[MB_LEN_MAX + 1];
  char nbytes;
} ibuf_t;

extern FILE* cf;
extern FILE* ifile[];
extern FILE* ofile[];
extern FILE* xfopen(char *, char *); // defined in m4.c

extern wchar_t** Ap;
extern wchar_t** argstk;
extern wchar_t* astklm;
extern void* xmalloc(size_t); // defined in m4.c
extern char* fname[];
extern wchar_t* ibuf;
extern wchar_t* ibuflm;

// in pointer
extern wchar_t* ip;
// in floor
extern wchar_t* ipflr;
// in stack
extern wchar_t* ipstk[10]; // max 10 things pushed back onto the in stack?

extern wchar_t* obuf;
extern wchar_t* obuflm;
extern wchar_t* op;
extern char* procnam;
extern char* tempfile;
extern wchar_t* token;
extern wchar_t* toklm;
extern wchar_t C;
extern wchar_t getchr(); // defined in m4.c
extern wchar_t lcom[];
extern wchar_t lquote[];
extern wchar_t nullstr[];
extern wchar_t rcom[];
extern wchar_t rquote[];
extern int bufsize;
extern int fline[];
extern int hshsize;
extern unsigned int hshval;
extern int ifx;
extern int nflag;
extern int ofx;
extern int sflag;
extern int stksize;
extern int sysrval;
extern int toksize;
extern int trace;
extern int exitstat;
extern long ctol(wchar_t *); // defined in m4.c
extern struct bs barray[];
extern struct call* Cp;
extern struct call* callst;
extern struct nlist** hshtab;
extern void install(wchar_t *, wchar_t *, int); // defined in m4.c
extern struct nlist* lookup(wchar_t *); // defined in m4.c
extern struct Wrap* wrapstart;
extern int wide;
extern ibuf_t ibuffer[];

extern void setfname(char *); // defined in m4.c
extern void pbstr(wchar_t *); // defined in m4.c
extern void pbnum(long); // defined in m4.c
extern void pbnbr(long, int, int); // defined in m4.c
extern void undiv(int, int); // defined in m4.c
extern void delexit(int, int); // defined in m4.c
extern void error(char *); // define in m4.c
extern int min(int, int); // defined in m4.c
extern void putbak(wchar_t); // macro defined here?
extern void stkchr(wchar_t); // macro defined here?
extern void error2(char *, int); // defined in m4.c

extern wchar_t *wstrdup(wchar_t *); // defined in m4.c
extern int wstoi(wchar_t *); // defined in m4.c
extern char *wstr2str(wchar_t *, int); // defined in m4.c
extern wchar_t *str2wstr(char *, int); // define in m4.c

extern void dodef(wchar_t **, int);
extern void doundef(wchar_t **, int);
extern int undef(wchar_t *);

extern int yylex(void);
extern void yyerror(const char *);

/*
 * macros for performance reason.
 */
// put a character on the input?
#define	putbak(c)	\
	if (ip < ibuflm)	\
		*ip++ = (c);	\
	else	\
		error2("pushed back more than %d chars", bufsize)

// put a character on the output? or stack?
#define	stkchr(c)	\
	if (op < obuflm)	\
		*op++ = (c);	\
	else	\
		error2("more than %d chars of argument text", bufsize)
