// A re2c-based scanner for vtprolog tokens,
// ripped almost completely from the "002_strip_comments" example from
// re2c/examples/lessons
//
// A major TODO is quoted strings.
//
#include <assert.h>
#include <stdio.h>
#include <stdlib.h> // for exit
#include <string.h>
#include "new_vtprolog_parser.h"

/*!max:re2c */
#define BSIZE 128

#if BSIZE < YYMAXFILL
# error BSIZE should be greater than YYMAXFILL
#endif

#define YYCTYPE unsigned char
#define YYCURSOR s.cur
#define YYLIMIT s.lim
#define YYFILL(n) { if ((res = fill(&s, n)) >= 0) break; }

typedef struct
{
    FILE* fp;
    unsigned char* cur; // the cursor
    unsigned char* tok; // the start of the current token
    unsigned char* lim;
    unsigned char* eof;
    unsigned char buffer[BSIZE];
} Scanner;

int fill(Scanner* s, int len)
{
    if (len == 0)
    {
        s->cur= s->buffer;
        s->tok= s->buffer;
        s->lim= s->buffer;
        s->eof= 0;
    }
    if (! s->eof)
    {
        int got= s->tok - s->buffer;
        int cnt= s->tok - s->buffer;
        if (cnt > 0)
        {
            // TODO(johnicholas.hines@gmail.com): This copying backward seems crazy.
            memcpy(s->buffer, s->tok, s->lim - s->tok);
            s->tok -= cnt;
            s->cur -= cnt;
            s->lim -= cnt;
        }
        cnt= BSIZE - cnt;
        if ((got = fread(s->lim, 1, cnt, s->fp)) != cnt)
        {
            s->eof= &s->lim[got]; // what is this doing?
        }
        s->lim += got;
    }
    else if (s->cur + len > s->eof)
    {
        fprintf(stderr, "not enough input data\n");
        return 0; // What does this mean?
    }
    return -1; // What does this mean?
}

int scan(FILE* fp)
{
    int res= 0;
    Scanner s;
    void* parser;

    assert(fp);
    s.fp= fp;
    fill(&s, 0);
    parser= ParseAlloc( malloc );
    while (1)
    {
        s.tok= s.cur;

        /*!re2c
          re2c:indent:top    = 2;
          re2c:yyfill:enable = 0;

          whitespace = [ \t\n]+ ;
	  
          lowerid = [a-z][a-zA-Z0-9_]* ;
          upperid = [A-Z_][a-zA-Z0-9_]* ;

          whitespace	{ continue; }
          '(' '*'	{ goto comment; }

          '.'	{ Parse(parser, PERIOD, NULL); continue; }
          ':-'	{ Parse(parser, IMPLIED_BY, NULL); continue; }
          '?-'	{ Parse(parser, BEGIN_QUERY, NULL); continue; }
          '@'	{ Parse(parser, INCLUDE, NULL); continue; }
          ','	{ Parse(parser, COMMA, NULL); continue; }
          'exit'	{ Parse(parser, EXIT, NULL); continue; }
          lowerid	{ 
	  		// printf("constant(%.*s)\n", s.cur - s.tok, s.tok); 
	  		Parse(parser, LOWER_ID, NULL);
			continue;
			}
          upperid	{
	  		// printf("variable(%.*s)\n", s.cur - s.tok, s.tok);
			Parse(parser, UPPER_ID, NULL);
			continue;
			}
          '('	{ Parse(parser, OPEN_PAREN, NULL); continue; }
          ')'	{ Parse(parser, CLOSE_PAREN, NULL); continue; }

      	  . { break; }
        */
comment:
        s.tok= s.cur;
        /*!re2c
          '*' ')' { continue; }
          [^] { goto comment; }
        */
    }
    Parse(parser, 0, NULL);
    ParseFree(parser, free);

    return res;
}

int main()
{
    return scan(stdin);
}

