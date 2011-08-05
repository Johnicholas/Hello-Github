
/* the current location in the code, measured in bytecodes */
int curloc;
/* the value of the most recently lexed token */
/* used for character literals, integer literals, and symbols */
/* symbols are understood to have a lexval that is an index into the names */
int lexval;
/* the type of the most recently lexed token */
/* e.g. '(' ')', '[', ']', STRING, RETURN IF ELSE WHILE DO */
int token;
/* the argument of the operation */
int pusharg;
/* the upcoming character, used for peeking */
int thechar;
/* the number of globals */
int nglob;
/* the number of locals */
int nlocal;
/* the number of functions */
int nfun;
/* the size of the most-recently parsed string */
int strsize;
/* the number of arguments */
int narg;
/* the current free spot (in memory) to put the next global var or array */
int curgloboffs;
/* the number of symbols in the names array */
int nsym;
/* the op to push */
int pushop;
/* the args, expressed as indexes into the names */
int argids[50];
/* the globals, expressed as indexes into the names */
int globids[300];
/* the positions of the globals in memory */
int globoffs[300];
/* an array of booleans indicating whether particular globals are lvals */
int globscalar[300];
/* the functions, expressed as indexes into the names */
int funids[300];
/* the addresses of the functions */
int funoffs[300];
/* the locals, expressed as indexes into the names */
int localids[50];
/* strings may not be longer than 5000 characters */
char symbol[5000];
/* the code may not be longer than 5000 opcodes */
char code[5000];
/* this is a sequence of null-terminated strings, used for everything */
char names[5000];

/* returns true if c is a digit */
/* assumes ascii */
/* note times-for-boolean-and */
int digit(int c)
{
    return (( '/' < c ) * (  c < ':' )) ;
}

/* returns true if c is a letter */
/* assumes ascii */
int letter(int c)
{
    return (( '`' < c ) * (  c < '{' )) ;
}

/* returns true if p and q are equal as null-terminated strings */
int eqstr(char* p, char* q)
{
    while (*p)
    {
        if (*p++ != *q++)
            return 0;
    }
    return !*q;
}

/* atom management - returns the index of name within the names array */
int lookup(char* name)
{
    int i;
    char* ns;

    ns = names;
    i = 0;
    while (i < nsym)
    {
        if (eqstr(ns, name))
        {
            /* found it */
            return i;
        }
        /* skip ns forward over the one that didn't match */
        while (*ns++)
            ;

        i++;
    }
    /* not found, so copy name into the names array */
    while (*ns++ = *name++)
        ;
    /* increment nsym, and return the previous value */
    return nsym++;
}

/* get a char, keeping track of "one ahead" (for peeking?) */
int next()
{
    int r;

    r = thechar;
    thechar = getchar();
    return r;
}

/* conditionally advances through the instream - only if there is a match */
int gobble(int t, int rr, int r)
{
    if (thechar == t)
    {
        next();
        return rr;
    }
    return r;
}

/* parse a string from the input, terminated by delim */
/* put the string into the symbol array */
/* Delim is probably a single or double quote. */
/* Note: escaping the escape and escaping the quote are also supported */
/* Note: the initial quote is assumed to be already consumed */
int getstring(int delim)
{
    int c;

    strsize = 0;
    while ((c = next()) != delim)
    {
        if (c == '\\')
        {
            if ( (c = next()) == 'n') c = '\n';
            if (c == 't') c = '\t';
            if (c == '0') c = 0;
        }
        symbol[strsize++] = c;
    }
    symbol[strsize++] = 0;
}

int getlex()
{
    int c;
    char *p;

    /* first, skip some stuff - whitespace? */
    while ( (( 0 < (c = next()) ) * (  c < '!' )) )
        ;

    /* Second, if the character is punctuation, return it literally*/
    if ((( c == -1 ) + (
                (( c == '(' ) + (
                     (( c == ')' ) + (
                          (( c == '[' ) + (
                               (( c == ']' ) + (
                                    (( c == '{' ) + (
                                         (( c == '}' ) + (
                                                 (( c == ',' ) + (
                                                         c == ';' ))  ))  ))  ))  ))  ))  ))  )) )
        return c;

    /* Third, if the character looks like a comment beginning, eat a comment */
    if (c == '/')
    {
        if (thechar == '*')
        {
            while ((( next() != '*' ) + (  thechar != '/' )) )
                ;
            next();
            /* Notice the recursion - a tail call */
            return getlex();
        }
        else
        {
            /* Actually, it looked like a comment, but it was a divop */
            return 1115 ;
        }
    }
    if (c == '*') return 1114 ;
    if (c == '%') return 1116 ;
    if (c == '-')
    {
        /* could be -- or - */
        return gobble(c, 1225, 1013);
    }
    if (c == '<')
        return 808 ;
    if (c == '=')
    {
        /* could be == or = */
        return gobble(c, 806 ,101 );
    }
    if (c == '+')
    {
        /* could be ++ or + */
        return gobble(c, 1219, 1012);
    }
    if (c == '!')
    {
        /* could be ! or != */
        return gobble('=', 807, 1217 );
    }
    if (c == '\'')
    {
        /* a character literal */
        getstring(c);
        /* only the first character of the string is used */
        lexval = symbol[0];
        return 257; /* 257 is an integer-or-character literal */
    }
    if (c == '"')
    {
        getstring(c);
        return 258;
    }
    if (digit(c))
    {
        /* a little inline itoa */
        lexval = c - '0';
        while (digit(thechar))
        {
            lexval = lexval * 10 + next() - '0';
        }
        return 257; /* 257 is an integer-or-character literal */
    }

    /* Okay in the last, default case is it's a symbol */
    /* copy it into the symbol array */
    p = symbol;
    *p++ = c;
    while (letter(thechar))
        *p++ = next();
    *p = 0;
    /* If it's an early symbol, then it's a keyword */
    if ( (lexval = lookup(symbol)) < 7 )
    {
      /* both 'int' and 'char' keywords have the same lexval */
      if (lexval == 6 )
	return 517;
      return lexval + 512 ;
    }
    /* It's a non-keyword symbol */
    return 256;
}

/* Analogous to gobble in the lexing world, this allows consuming a token */
/* just in the case where the caller already knows what it is */
int istoken(int t)
{
    if (token == t)
    {
        token = getlex();
        return 1;
    }
    return 0;
}

int type()
{
    istoken(517); /* WHAT IS 517? */
    while (istoken(1114)) /* 1114 is *, so this erases any number of stars */
        ;
}

/* Store the current lexval temporarily, then lex a token, */
/* then return the stored lexval */
int name()
{
    int r;

    r = lexval;
    token = getlex();
    return r;
}

/* push an opcode into the code array */
int emit(int opc)
{
    code[curloc++] = opc;
}

/* backpatch the code array, putting two bytes (c) at a */
int emitat(int a, int c)
{
    code[a++] = c;
    code[a] = c / 256;
}

/* emit an operator with an immediate, two-byte argument. */
/* the low three bits of the operator are used (7= 0b111) to indicate that */
/* an argument is coming up. */
/* returns the address of the argument, presumably for backpatching */
int emitop(int rator, int rand)
{
    int r;

    emit(rator + 7);
    r = curloc;
    emit(rand);
    emit(rand / 256);
    return r;
}

/* walk over the array from 0 to max, looking for lexval. */
/* stores puop in the global pushop, and if found, the index in pusharg */
/* returns whether or not it was found. */
int pushloop(int puop, int max, int* arr) {
    int i;

    i = 0;
    pushop = puop;
    while (i < max)
    {
        if (arr[i] == lexval)
        {
            pusharg = i;
            return 1;
        }
        i++;
    }
    return 0;
}

/* eventually calls emitop, but handles various addressing modes */
/* returns whether the value was an lval */
int pushval()
{
    int lval;

    /* by default, we assume it is an lval */
    lval = 1;
    /* 64 is for getting locals */
    if (pushloop(64, nlocal, localids))
    {
      /* lexval was found in the local ids */
      /* nothing to do, pusharg is correct. */
    }
    else if (pushloop(32, narg, argids)) /* 32 is for getting args */
    {
      /* lexval was found in the arg ids */
      /* adjust pusharg from the index within arg ids */
      pusharg = narg - pusharg - 1;
    }
    else if (pushloop(96, nglob, globids)) /* 96 is for getting globals */
    {
      /* lexval was found in the global ids */
      /* Some globals are lvals and some are not; the info is recorded */
      lval = globscalar[pusharg];
      /* adjust pusharg to be the address of that global */
      pusharg = globoffs[pusharg];
    }
    else
    {
      /* definitely not an lval */
      lval = 0;
      if (pushloop(192, nfun, funids)) /* 192 is get function? */
      {
	/* lexval was found in the function ids */
	pusharg = funoffs[pusharg];
      }
      else if (lexval < 9)
      {
	/* not found anywhere, but the lexval is small. */
	/* pushop is still 192 */
	/* maybe these are primitive function calls? */
	pusharg = lexval - 7 ;
      }
      /* not found anywhere, and the lexval is 10 or larger */
      /* pushop is still 192 */
      /* pusharg is still whatever it was when pushval was called */
    }
    emitop(pushop, pusharg);
    return lval;
}

/* maybe push a deref - if necessary */
int pderef(int l)
{
    if (l)
      emit(23); /* 23 is deref */
}

/* expr is part of the parser */
/* needval is true/1 if the context needs it? */
/* prec is precedence? */
int expr(int needval, int prec)
{
    int na;
    int islval;
    int jdst;
    int op;
    int any;
    int opprec;

    /* by default, the expression is not an lval */
    islval = 0;
    if (istoken(257)) /* 257 is a character literal */
    {
      emitop(128, lexval); /* 128 is push char literal? */
    }
    else if (istoken(258)) /* 258 is a string literal */
    {
      emitop(144, strsize); /* 258 is push a string literal? */
      /* Then we copy the string DIRECTLY into the bytecode */
      any = 0;
      while (any < strsize)
	emit(symbol[any++]);
    }
    else if (istoken(256)) /* 256 is a symbol */
    {
      /* search the various namespaces and do the right thing */
      islval = pushval();
    }
    else if (istoken('(')) /* expr ::= '(' exp ')' */
    {
        islval = expr(0, 0);
        istoken(')');
    }
    else if (istoken(1217)) /* 1217 is !. exp ::= '!' exp */
    {
        expr(1, 100);
        emit(17); /* 17 is logical negation */
    }
    else if (istoken(1013)) /* 1013 is -. exp ::= '-' exp */
    {
        expr(1, 100);
        emit(18); /* 18 is additive inverse */
    }
    else if (istoken(1114)) /* 1114 is *. exp ::= '*' exp */
    {
        expr(1, 100);
        islval = 1;
    }
    /* This loop allows various things that can be followed by themselves? */
    /* for example, x ++ ++ */
    any = 1;
    while (any)
    {
      /* a correspondence between token and opcode is built into into the */
      /* token and opcode tables */
      op = token % 100 ;
      if (istoken('(')) { /* expr ::= '(' { expr ',' } ')' */
	pderef(islval);
	na = 0;
	if (!istoken(')')) {
	  do {
	    expr(1, 0 );
	    na++;
	  } while (istoken(','));
	  istoken( ')' ) ;
	}
	emitop(176, na*2); /* function call is opcode 176? */
	islval = 0;
      } else if (istoken('[')) { /* expr ::= '[' expr ']' */
	pderef(islval);
	expr(1, 0);
	emit(12);
	istoken( ']' );
	islval = 1;
      } else if (istoken(1219)) { /* 1219 is ++ */
	emit(op);
	islval = 0;
      } else if (istoken(1225)) { /* 1225 is -- */
	emit(op);
	islval = 0;
      } else {
	any = 0;
      }
    }
    /* a correspondence between the token and the precedence is built */
    /* into the lexing tables */
    opprec = token / 100 ;
    while (prec < opprec)
    {
        if ( (op = token % 100 ) != 1 )
        {
            pderef(islval);
        }
        {
            token = getlex();
            expr(1, opprec);
            emit(op);
        }
        islval = 0;
        opprec = token / 100 ;
    }
    if (needval)
    {
        pderef(islval);
        islval = 0;
    }
    return islval;
}

/* parse a parenthesized expression */
int pexpr() {
    istoken('(');
    expr(1, 0);
    istoken(')');
}

int stmt()
{
    int jdest;
    int tst;

    if (istoken('{')) /* stmt ::= '{' ( stmt )* '}' */
    {
      while (!istoken('}'))
	stmt();
    }
    else if (istoken(513)) /* 513 is 'if' */
    {
      /* stmt ::= 'if' '(' expr ')' stmt */
      /* stmt ::= 'if' '(' expr ')' stmt 'else' stmt */
        pexpr();
	/* 224 is jump if zero? */
	/* store the dest for backpatching */
        jdest = emitop(224, 0); 
        stmt(); /* the true branch */
        if (istoken(514)) /* 514 is 'else' */
        {
	  /* 208 is jump unconditionally */
	  /* store the destination for backpatching */
	  tst = emitop(208, 0);
	  /* backpatch jdest - we're starting the false branch now */
	  emitat(jdest, curloc); 
	  stmt(); /* the false branch */
	  /* backpatch the unconditional jump at the end of the true branch */
	  /* to jump here, over the false branch */
	  emitat(tst, curloc);
        }
        else
        {
	  /* one-branch if */
	  /* backpatch jdest to come here */
	  emitat(jdest, curloc);
        }
    }
    else if (istoken(515)) /* 515 is 'while' */
    {
      /* store the current location for backpatching */
      tst = curloc;
      pexpr();
      /* emit a jump if zero, and store the location for backpatching */
      jdest = emitop(224, 0);
      stmt(); /* the body */
      /* jump unconditionally back up to before the test */
      emitop(208, tst); 
      /* backpatch the jump if zero to come here */
      emitat(jdest, curloc);
    }
    else if (istoken(516)) /* 516 is 'do' */
    {
      /* store the current location for backpatching */
      jdest = curloc;
      /* the body */
      stmt();
      istoken(515); /* 515 is 'while' */
      /* the test */
      pexpr();
      /* 17 is logical negation */
      emit(17);
      /* jump if zero back to jdest */
      emitop(224, jdest);
    }
    else if (istoken(512)) /* expr ::= 'return' expr ';' */
    {
        expr(1, 0);
        istoken(';');
        emit(10);
    }
    else if (istoken(';')) /* semicolon by itself is a fine statement */
    {
    }
    else /* stmt ::= expr ';' */
    {
        expr(1, 0);
        emit(22); /* 22 is 'drop' */
        istoken(';') ;
    }
}
	
int parse() {
    int objid;

    /* From the hint, we know that a program is a sequence of declarations */
    token = getlex();
    while (1)
    {
        if (token < 0)
            return 1;
	/* every declaration starts with type */
        type();
	/* every declaration continues with name */
        objid = name();
	if (istoken('('))
        {
	  /* must be a fundec */
	  funids[nfun] = objid;
	  funoffs[nfun++] = curloc; /* we're going to put it right here */
	  /* build the arguments */
	  narg = 0;
	  if (!istoken(')')) {
	    do {
	      type();
	      argids[narg++] = name();
	    } while (istoken(','));
	    istoken(')');
	  }
	  /* start of function body */
	  istoken( '{' ) ;
	  /* build the locals */
	  nlocal = 0;
	  while (token == 517) { /* 517 is 'int' or 'char' */
	    type();
	    do {
	      localids[nlocal++] = name();
	    } while (istoken(','));
	    istoken( ';' ) ;
	  }
	  while (!istoken('}'))
	    stmt();
	  emit(10); /* 10 is return */
        }
        else
        {
	  /* must be a vardec */
	  globoffs[nglob] = curgloboffs;
	  if (istoken('[')) {
	    istoken(257); /* 257 is an integer literal */
	    /* leave some space */
	    curgloboffs = curgloboffs + lexval;
	    istoken(']');
	    /* remember that this global is an array */
	    globscalar[nglob] = 0;
	  }
	  else
          {
	    curgloboffs++;
	    /* remember that this global is a scalar */
	    globscalar[nglob] = 1;
	  }
	  globids[nglob++] = objid;
	  /* vardecs end with a semicolon */
	  istoken(';');
        }
    }
}

int main() {
    int n;
    char* p;
    char* q;

    /* there will be nine keywords */
    nsym = 9;
    /* load the keywords into the names table */
    p = names;
    q = "return\0if\0else\0while\0do\0int\0char\0getchar\0putchar";
    n = 48; /* the length of the constant string here */
    do {
      *p++ = *q++;
    } while (n--);
    /* skip a bit forward, leaving some space */
    curloc = 7;
    /* initialize the peek system */
    thechar = getchar();
    /* parse and generate all the opcodes */
    parse();

    /* store the current location in n */
    n = curloc;
    /* move back to the beginning */
    curloc = 0;
    /* look up the main function's address */
    lexval = lookup("main");
    /* emit the address of main */
    pushval();
    /* emit "pushframe"? */
    emitop(176, 0);
    /* emit "jump"? */
    emit(27);

    /* start writing the code. */
    /* First two zeros, then the length of the bytecode as a 16-bit number */
    putchar(0);
    putchar(0);
    putchar(n);
    putchar(n / 256);
    /* write each byte from the beginning to the end */
    p = code;
    while (n--) {
        putchar(*p++);
    }
    return 0;
}
