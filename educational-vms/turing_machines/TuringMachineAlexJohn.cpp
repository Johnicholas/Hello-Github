// Universal Turing machine implementation, released under ISC/BSD license:
// Copyright (c) 2009, Alex Stangl <alex@stangl.us>, John Tromp <john.tromp@gmail.com>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
// WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
// ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
// WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
// ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
// OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

// This implementation, written on 01/02/2009 is an attempt to write a shortest
// possible implementation of a Universal Turing machine, which takes 5n + 1 command-line
// integer arguments, first of which is maximum number of steps to run, and then each
// 5-tuple consists of (input state, input symbol, output state, output symbol, direction)
// see http://www.mathrix.org/experimentalAIT/TuringMachine.html for full rules and other details.
// Compromises were made with coding style and error handling, to shorten the source code.

// Z(n) returns n+argc'th cmdline arg converted to int, compressing source code; note argc gets reused
#define Z(n) atoi(argv[argc+n])
#define C std::cout<<

#include <iostream>
int main(int argc, char **argv)
{
	// single declaration/initialization line for sake of compression, using calloc to allocate zero-filled tape array
	int *tapePtr, maxSteps=Z(1-argc), *tape=(int*)calloc(maxSteps, 8), *maxPos, *minPos, state=1, step=0;

	// Init tape position pointer and high and low watermarks, then crank FSM until terminal state reached or out of steps
	for (tapePtr=maxPos=minPos=tape+maxSteps; state && step < maxSteps; ++step) {
		for (argc = 2; Z(0)-state | Z(1)-*tapePtr; argc += 5);
		// Update current state, write current symbol, update high and low watermarks, move tape, then continue with next cycle
		state = Z(2);
		*tapePtr = Z(3);
		maxPos += (tapePtr += Z(4)) > maxPos;
		minPos -= tapePtr < minPos;
	}

	// Output results:  2 if machine hasn't halted or written tape contents if it has, followed by # of steps processed
	if (state)
		C 2;
	else
		while (minPos <= maxPos) C *minPos++;
	C '\n' << step;
	return 0;
}
