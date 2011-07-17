/*
// This is an edited/prettyprinted version of Lennart Augustsson's 1996 ioccc-winning entry.
// Misunderstandings by Johnicholas.
//
// memory is layed out:
//  first (near zero) the bytecode of the program itself.
//  second (after bytecode) any globals
//  near the bottom of memory is the stack, growing upward.
//
// some opcodes don't take any arguments.
// those that take arguments use the high nibble to indicate the op
// and the low 4 bits to indicate the op argument.
// 0 through 6 are used for small arguments.
// 7 means the argument is the next two bytes.
// 8-16 means the argument is the next byte (read the code, it's simpler)
// the "call" opcode codes its argument as a number of parameters,
// and a 1-bit number of return values
*/

int m[60000];

int main()
{
    /* during loading, counts forward, always pointing to the next free spot for bytecode
    // not used during the main loop */
    int* q;
    /* the length of the bytecode */
    int c;
    /* always 256 */
    int b;
    /* the stack pointer */
    int s;
    /* the frame pointer */
    int f;
    /* r is used as a temporary register for most calculations */
    int r;
    /* o is used to hold the second byte of the input initially
    //    zero in the second byte indicates uncompressed bytecode
    // o is used as the current operator during the main loop */
    int o;
    /* l is used as the operator's argument during the main loop */
    int l;
    /* during loading, counts down how many bytes left to load
    // during the main loop, acts as the instruction pointer */
    int p;
    /* part of the decompression process? or just obfuscation? */
    char* d;

    d = " \t\n;{}\0A PÄ≥≤âä@…µÅgEœÕ±∞å0çéãq";
    b = 256;
    q = m;
    /* ignore the first byte */
    getchar();
    /* store second byte, but just to decide which bytecode scheme we're using */
    o = getchar();
    /* the length of the bytecode is in the 3rd and fourth bytes */
    c = getchar();
    c = c + getchar() * b;
    p = c;
    if (o) {
        /* this is a compression scheme that takes advantage of the
          // peculiar length restriction of the ioccc in the year it was winner.
          //   "The number of characters excluding whitespace (tab, space, newline,
          //   formfeed, return) and excluding any ; { or } immediately followed by
          //   whitespace or end of file, must be <= 2048"
          // that is, whitespace was cheap, and so were semicolons and braces, so long as
          // they are followed by whitespace. */
        while (p) {
            o = getchar();
            if (o == 59) { /* 59 is ';' */
                r = 2;
            } else if (o == '{') {
                r = 5;
            } else if (o == '}') {
                r = 8;
            } else {
                r = 0; /* something other than {}; */
            }

            if (r) {
                /* a run length encoding - perhaps helpful for initialized arrays? */
                r = r + getchar() % 4; /* this character is almost certainly whitespace */
                l = 1 - getchar() - r; /* the spot (presumably previous in the bytecode output) to repeat */
                while (r--) {
                    *q = q[l];
                    q++;
                    p--;
                }
            } else {
                if ((o == 32) + (o == 9) + (o == 10)) { /* space or tab or newline */
                    r = getchar();
                    /* 123 is '{' - presumably the nonlinearity disambiguates it from something else that is 3 mod 8 */
                    o = o % 4 + (r + (r == 123)) % 8 * 3;
                    if (8 < o) {
                        o = o + getchar() % 4 * 9;
                    }
                    /* to avoid overrunning d, o should be less than 36 here. */
                    o = d[o] + b;
                }
                *q++ = o % b;
                p--;
            }
        }
        /* clean up a bit to support catting the bytecode with the bytecode's input */
        while (getchar() != 12) { /* eat to the end of the line */
            ;
        }
        getchar(); /* consume the newline */
    } else {
        /* the simple encoding scheme - bytes are opcodes. */
        while (p--) {
            *q++ = getchar();
        }
    }


    s = 60000;
    p = 0;
    while (1) {
        o = m[p++];
        if (o == 17) { /* boolean negation */
            m[s] = ! m[s];
        } else if (o == 18) { /* arithmetic negation */
            m[s] = - m[s];
        } else if (o == 12) { /* add */
            r = m[s++];
            m[s] = m[s] + r;
        } else if (o == 13) { /* subtract */
            r = m[s++];
            m[s] = m[s] - r;
        } else if (o == 14) { /* multiply */
            r = m[s++];
            m[s] = m[s] * r;
        } else if (o == 15) { /* divide */
            r = m[s++];
            m[s] = m[s] / r;
        } else if (o == 16) { /* mod */
            r = m[s++];
            m[s] = m[s] % r;
        } else if (o == 6) { /* equals */
            r = m[s++];
            m[s] = m[s] == r;
        } else if (o == 7) { /* not-equals */
            r = m[s++];
            m[s] = m[s] != r;
        } else if (o == 8) { /* less-than */
            r = m[s++];
            m[s] = m[s] < r;
        } else if (o == 1) { /* poke and leave the poked val on the stack */
            r = m[s++];
            m[m[s]] = r;
            m[s] = r;
        } else if (o == 9) { /* poke and don't leave the poked val */
            r = m[s++];
            m[m[s]] = r;
            s++;
        } else if (o == 10) { /* return */
            r = m[s]; /* store top of stack in temporary */
            s = f; /* stack moves back to frame pointer */
            p = m[s- 1]; /* instruction pointer */
            f = m[s- 2]; /* old frame pointer */
            l = m[s- 3]; /* type of function called - number of args (3bits) and number of return values (1bit) */
            s = s + l / 2; /* move stack down based on the high bits of l */
            m[s] = r; /* put the temporary back on top of the stack */
            if (l % 2) { /* based on the lowest bit of l */
                s++; /* pop the stack */
            }
        } else if (o == 22) { /* pop */
            s++;
        } else if (o == 27) { /* exit */
            return m[s];
        } else if (o == 21) { /* dup */
            r = m[s];
            s--;
            m[s] = r;
        } else if (o == 23) { /* follow a pointer */
            m[s] = m[m[s]];
        } else if (o == 19) { /* some clever thing */
            m[s] = m[m[s]]++;
        } else if (o == 25) { /* some other clever thing */
            m[s] = m[m[s]]--;
        } else {
            /* operators with arguments */
            l = o % 16;
            o = o / 16;
            if (l == 7) { /* 7 indicates the arg doesn't fit in 4 bits, but it does fit in two bytes. */
                l = m[p++];
                l = l + m[p++] * b;
            } else if (7 < l) { /* 8-15 indicates the arg doesn't fit in 4 bits, but does fit in something like 12 bits */
                l = l + m[p++] * 8 - 1;
            } else {
                /* l is left as a small literal */
            }
            if (o == 2) { /* get local? */
                s--;
                m[s] = f + l;
            } else if (o == 4) { /* get arg? */
                s--;
                m[s] = f - l - 4;
            } else if (o == 3) { /* get local-by-ref? */
                s--;
                m[s] = m[f+l];
            } else if (o == 5) { /* get arg-by-ref? */
                s--;
                m[s] = m[f-l-4];
            } else if (o == 6) { /* get address of global? */
                s--;
                m[s] = c + l;
            } else if (o == 7) { /* get global? */
                s--;
                m[s] = m[c+l];
            } else if (o == 12) { /* push */
                s--;
                m[s] = l;
            } else if (o == 8) { /* also push? */
                s--;
                m[s] = l;
            } else if (o == 9) { /* tail call? */
                s--;
                m[s] = p; /* put the current instruction pointer on the stack */
                p = p + l; /* jump */
            } else if (o == 11) { /* function call */
                /* the type of the function called is expressed using l.
                // 3 bits to express the number of args
                // 1 to express the number of return values */
                r = m[s- -l/2]; /* get something from possibly deep in the stack? */
                if (r < 3) {
                    /* I think one could put other primitives in this section, just increment that 3 and use a switch */
                    if (r) {
                        /* primitive 1 or 2 is putchar */
                        r = putchar(m[s]);
                    } else {
                        /* primitive 0 is getchar */
                        /* note: we're getting this from stdin after the bytecode
                        // cat is your friend */
                        r = getchar();
                    }
                    s = s + l / 2; /* pop the arguments */
                    m[s] = r; /* put the answer on top of the stack */
                    if (l % 2) { /* but maybe pop it */
                        s++;
                    }
                } else {
                    /* a non-primitive call */
                    m[s- 1] = p; /* store instruction pointer */
                    m[s- 2] = f; /* store frame pointer */
                    m[s- 3] = l; /* store the type of call including number of args and the return void flag */
                    f = s; /* move the frame pointer up */
                    s = s - 19; /* give lots of space in the new frame? */
                    p = r; /* jump */
                }
            } else if (o == 13) { /* unconditional jump */
                p = l;
            } else if (o == 15) { /* jump if true */
                if (m[s++]) {
                    p = l;
                }
            } else if (o == 14) { /* jump if false */
                if (!m[s++]) {
                    p = l;
                }
            } else { /* unrecognized operator */
                ;
            }
        }
    }
    return 0;
}
