/*
// Taken whole from the ioccc design doc:
// http://www.ioccc.org/1992/buzzard.2.design
// and then mangled / edited

Section 1:  FIRST

Environment

    The stack is simply a standard LIFO data structure that is used
implicitly by most of the FIRST primitives.  The stack is made up
of ints, whatever size they are on the host machine.

    String storage is used to store the names of built-in and defined
primitives.  Separate storage is used for these because it allows
the C code to use C string operations, reducing C source code size.

    Main memory is a large array of ints.  When we speak of
addresses, we actually mean indices into main memory.  Main memory
is used for two things, primarily: the return stack and the dictionary.

    The return stack is a LIFO data structure, independent of
the abovementioned "the stack", which is used by FIRST to keep
track of function call return addresses.

    The dictionary is a list of words.  Each word contains a header
and a data field.  In the header is the address of the previous word,
an index into the string storage indicating where the name of this
word is stored, and a "code pointer".  The code pointer is simply
an integer which names which "machine-language-primitive" implements
this instruction.  For example, for defined words the code pointer
names the "run some code" primitive, which pushes the current program
counter onto the return stack and sets the counter to the address of
the data field for this word.

    There are several important pointers into main memory.  There is
a pointer to the most recently defined word, which is used to start
searches back through memory when compiling words.  There is a pointer
to the top of the return stack.  There is a pointer to the current
end of the dictionary, used while compiling.

    For the last two pointers, namely the return stack pointer and
the dictionary pointer, there is an important distinction: the pointers
themselves are stored in main memory (in FIRST's main memory).  This
is critical, because it means FIRST programs can get at them without
any further primitives needing to be defined.


Instructions

    There are two kinds of FIRST instructions, normal instructions and
immediate instructions.  Immediate instructions do something significant
when they are used.  Normal instructions compile a pointer to their
executable part onto the end of the dictionary.  As we will see, this
means that by default FIRST simply compiles things.

  Integer Operations
Symbol	Name		Function
  -	binary minus	pop top 2 elements of stack, subtract, push
  *	multiply	pop top 2 elements of stack, multiply, push
  /	divide		pop top 2 elements of stack, divide, push
  <0	less than 0	pop top element of stack, push 1 if < 0 else 0

Note that we can synthesize addition and negation from binary minus,
but we cannot synthesize a time efficient divide or multiply from it.
<0 is synthesizable, but only nonportably.

  Memory Operations
Symbol	Name		Function
  @	fetch		pop top of stack, treat as address to push contents of
  !	store		top of stack is address, 2nd is value; store to memory
				and pop both off the stack

  Input/Output Operations
Name			Function
echo			output top of stack through C's putchar()
key			push C's getchar() onto top of stack
_read			read a space-delimited word, find it in the
				dictionary, and compile a pointer to
				that word's code pointer onto the
				current end of the dictionary

Although _read could be synthesized from key, we need _read to be able
to compile words to be able to start any syntheses.

  Execution Operations
Name			Function
exit			leave the current function: pop the return stack
				into the program counter

  Immediate (compilation) Operations
Symbol	Name		Function
  :	define		read in the next space-delimited word, add it to
				the end of our string storage, and generate
				a header for the new word so that when it
				is typed it compiles a pointer to itself
				so that it can be executed.
immediate immediate	when used immediately after a name following a ':',
				makes the word being defined run whenever
				it is typed.

: cannot be synthesized, because we could not synthesize anything.
immediate has to be an immediate operation, so it could not be
synthesized unless by default operations were immediate; but that
would preclude our being able to do any useful compilation.

  Stack Operations
Name			Function
pick			pop top of stack, use as index into stack and copy up
				that element

If the data stack were stored in main memory, we could synthesize pick;
but putting the stack and stack pointer in main memory would significantly
increase the C source code size.

    There are three more primitives, but they are "internal only"--
they have no names and no dictionary entries.  The first is
"pushint".  It takes the next integer out of the instruction stream
and pushes it on the stack.  This could be synthesized, but probably
not without using integer constants.  It is generated by _read when
the input is not a known word.  The second is "compile me".  When
this instruction is executed, a pointer to the word's data field is
appended to the dictionary.  The third is "run me"--the word's data
field is taken to be a stream of pointers to words, and is executed.

    One last note about the environment: FIRST builds a very small
word internally that it executes as its main loop.  This word calls
_read and then calls itself.  Each time it calls itself, it uses
up a word on the return stack, so it will eventually trash things.
This is discussed some more in section 2.


Here's a handy summary of all the FIRST words:

	- * /		binary integer operations on the stack
	<0		is top of stack less than 0?
	@ !		read from or write to memory
	echo key	output or input one character
	_read		read a word from input and compile a pointer to it
	exit		stop running the current function
	:		compile the header of a definition
	immediate	modify the header to create an immediate word

    Here is a sample FIRST program.  I'm assuming you're using
the ASCII character set.  FIRST does not depend upon ASCII, but
since FIRST has no syntax for character constants, one normally has
to use decimal values.  This can be gotten around using getchar, though.
Oh.  One other odd thing.  FIRST initially builds its symbol table
by calling : several times, so it needs to get the names of the base
symbols as its first 13 words of input.  You could even name them
differently if you wanted.
    These FIRST programs have FORTH comments in them: they are contained
inside parentheses.  FIRST programs cannot have FORTH comments; but I need
some device to indicate what's going on.  (THIRD programs are an entirely
different subject.)

	( Our first line gives the symbols for the built-ins )
: immediate _read @ ! - * / <0 exit echo key _pick

	( now we define a simple word that will print out a couple characters )

: L			( define a word named 'L' )
  108 echo		( output an ascii 'l' )
  exit

: hello			( define a word named 'hello')
  72 echo		( output an ascii 'H' )
  101 echo		( output an ascii 'e' )
  111			( push ascii 'o' onto the stack )
  L L			( output two ascii 'l's )
  echo			( output the 'o' we pushed on the stack before )
  10 echo		( print a newline )
  exit			( stop running this routine )

: test immediate	( define a word named 'test' that runs whenever typed )
  hello			( call hello )
  exit

test

( The result of running this program should be: 
Hello
)


Section 2: Motivating THIRD

    What is missing from FIRST?  There are a large number of
important primitives that aren't implemented, but which are
easy to implement.  drop , which throws away the top of the
stack, can be implemented as { 0 * + } -- that is, multiply
the top of the stack by 0 (which turns the top of the stack
into a 0), and then add the top two elements of the stack.

    dup , which copies the top of the stack, can be easily
implemented using temporary storage locations.  Conveniently,
FIRST leaves memory locations 3, 4, and 5 unused.  So we can
implement dup by writing the top of stack into 3, and then
reading it out twice: { 3 ! 3 @ 3 @ }.

    we will never use the FIRST primitive 'pick' in building THIRD,
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// the string storage
char s[5000];
// an index into free string storage
int t = 64;
// main memory, used for the return stack and the dictionary
// m[0] is the return stack pointer
// m[1] is the program counter
int m[20000] = {32};
// the data stack
int T[500];
// nearly the top of the stack
int* S = T;
// the actual top cell of the stack
int f;
// address of the previous word
int L = 1;
// is I a code pointer?
int I;
int w;

// adds a word to the dictionary
void add(int prim)
{
  // push the address of the previous word
  m[m[0]++] = L;
  // store the address of this word in the register,
  // for the next word
  L = m[0]-1;
  // push the current free string
  m[m[0]++] = t;
  // push the primitive that should be used
  m[m[0]++] = prim;
  scanf("%s", s + t);
  t += strlen(s + t) + 1;
}

enum opcodes {
  op_pushint, // internal-use only
  op_compileme, // internal-use only
  op_runme, // internal-use only
  op_define, // usually ":"
  op_immediate, // usually "immediate"
  op_read, // usually "_read"
  op_peek, // usually "@"
  op_poke, // usually "!"
  op_minus, // usually "-"
  op_times, // usually "*"
  op_divide, // usually "/"
  op_negative, // usually "<0"
  op_exit, // usually "exit"
  op_echo, // usually "echo"
  op_key, // usually "key"
  op_pick // usually "_pick"
};

// run something starting at code pointer x?
void run(int x)
{
    switch (m[x++])
    {
    case op_pushint: // internall use only
      // spill the top-of-stack register, preparing to push
      *++S = f;
      // push something from memory onto the stack?
      f = m[I++];
      break;
    case op_compileme: // internal use only
      // push x (to the return stack?)
      m[m[0]++] = x;
      break;
    case op_runme: // internal use only
      m[++m[1]] = I;
      I = x;
      break;
    case op_define:
      // compile the header of a definition
      add(op_compileme);
      m[m[0]++] = op_runme;
      break;
    case op_immediate:
      // modify the header to create an immediate word
      m[0] -= 2;
      m[m[0]++] = op_runme;
      break;
    case op_read:
      // read a word from input and compile a pointer to it
      for (w =
	     scanf("%s", s) < 1
	     ?
	     exit(0),0
	     :
	     L;
	   strcmp(s, &s[m[w+1]]);
	   w = m[w]) {
	; // empty loop body
      }
      if (w-1) {
	run(w + 2);
      } else {
	m[m[0]++]= 2; // is this actually op_runme?
	m[m[0]++]= atoi(s);
      }
      break;
    case op_peek:
      // pop the top of the stack, interpret as a pointer into memory,
      // peek at that location of memory, and push it
      f = m[f];
      break;
    case op_poke:
      // interpret the top two locations of the stack as address (top)
      // and value (next-to-top). Poke the value into the address, and pop
      // both address and value.
      m[f] = *S--;
      // pop the stack
      f = *S--;
      break;
    case op_minus:
      // pop top 2 elements of stack, subtract, push
      f = (*S--) - f;
      break;
    case op_times:
      // pop top 2 elements of stack, multiply, push
      f *= (*S--);
      break;
    case op_divide:
      // pop top 2 elements of stack, divide, push
      f = (*S--)/f;
      break;
    case op_negative:
      // pop top element of stack, push 1 if it was less than 0 else 0
      f = 0 > f;
      break;
    case op_exit:
      // leave the current function: pop the return stack
      // into the program counter
      I = m[m[1]--];
      break;
    case op_echo:
      putchar(f);
      // pop the stack
      f = (*S--);
      break;
    case op_key:
      // spill the top-of-stack register to the stack (prepare to push)
      *++S = f;
      // get a character from standard input and put it on top of the stack
      f = getchar();
      break;
    case op_pick:
      // interpret the top-of-stack as an index into the stack,
      // pop it, and then push the value of the stack at that index
      f = S[-f];
      break;
    }
}

int main()
{
  // add three specific things to the dictionary
  add(op_define); // zero
  add(op_immediate); // one
  add(op_compileme); // two
  // store the current return stack pointer in w
  w= m[0];
  // push read onto the return stack
  m[m[0]++] = op_read;
  // push runme onto the return stack?
  m[m[0]++] = op_runme;
  // store the current return stack pointer in I
  I= m[0];
  // push w onto the return stack
  m[m[0]++] = w;
  // push I-1 onto the return stack
  m[m[0]++] = I-1;
  for (w=6; w<16;) {
    // add something to the dictionary
    add(op_compileme); // six through fifteen
    // push 6, 7, 8... up to 15
    m[m[0]++] = w++;
  }
  m[1]= m[0];
  for (m[0] += 512;; run(m[I++])) {
    ;
  }
  return 0;
}
