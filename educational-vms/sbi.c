/* simple brainfuck interpreter */
/* 20 April 2006 */
/* Daniel B. Cristofani */
/* http://www.brainfuck.org/ */

#include <stdio.h>
#include <stdlib.h>
#define ARRAYSIZE 65536
#define MAXCODESIZE 65536

//For simplicity, we'll use statically allocated arrays with matching indices.
int stack[MAXCODESIZE], stackp; //to store locations of still-unmatched '['s.
char code[MAXCODESIZE]; int codep, codelength; //copy of the program we'll read into memory.
short int array[ARRAYSIZE], memp; //the memory used by the brainfuck program.
int targets[MAXCODESIZE]; //to save matching '[' for each ']' and vice versa.
int c;
FILE *prog;

int main(int argc, char **argv){
    if (argc > 2) fprintf(stderr, "Too many arguments.\n"), exit(1);
    if (argc < 2) fprintf(stderr, "I need a program filename.\n"), exit(1);
    if(!(prog = fopen(argv[1], "r"))) fprintf(stderr,"Can't open the file %s.\n", argv[1]),exit(1);
    codelength = fread(code, 1, MAXCODESIZE, prog);
    fclose(prog);
    for(codep=0; codep<codelength; codep++){
        if (code[codep]=='[') stack[stackp++]=codep;//put each '[' on the stack
        if (code[codep]==']'){ //If we meet a ']',
            if(stackp==0){ //and there is no '[' left on the stack, it's an error.
                fprintf(stderr,"Unmatched ']' at byte %d.", codep), exit(1);
            } else {
                --stackp; //if there is one, we take the matching '[' from the stack top,
                targets[codep]=stack[stackp]; //save it as the match for the current ']',
                targets[stack[stackp]]=codep; //and save the current ']' as the match for it.
            }
        }
    }
    if(stackp>0){ //Any unmatched '['s still left on the stack are an error too.
        fprintf(stderr,"Unmatched '[' at byte %d.", stack[--stackp]), exit(1);
    }
    for(codep=0;codep<codelength;codep++){//Everything is okay; we start executing the program.
         switch(code[codep]){
            case '+': array[memp]++; break;
            case '-': array[memp]--; break;
            case '<': memp--; break;
            case '>': memp++; break;
            case ',': if((c=getchar())!=EOF) array[memp]=c=='\n'?10:c; break;
            case '.': putchar(array[memp]==10?'\n':array[memp]); break;
            case '[': if(!array[memp]) codep=targets[codep]; break;
            case ']': if(array[memp]) codep=targets[codep]; break;
        }
    }
    exit(0);
}
