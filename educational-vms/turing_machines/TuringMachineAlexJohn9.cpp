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

// This implementation, initially written on 01/02/2009 is an attempt to
// write a shortest possible implementation of a Universal Turing machine,
// which takes 5n command-line integer arguments consisting of
// (input state, input symbol, output state, output symbol, direction)
// see http://www.mathrix.org/experimentalAIT/TuringMachine.html for full
// rules and other details. Compromises were made with coding style and
// error handling, to shorten the source code.

#include <iostream>

// GETARG returns argc'th cmdline arg converted to int, incrementing argc
#define GETARG atoi(argv[argc++])

int main(int argc, char **argv)
{
	// Init all variables; minPos is low water mark, calloc allocates
	// zero-filled tape array.
	int state=argc=1, step=0, tapeSize=2;
	char *oldTape, *newTape=0, *minPos, *tapePtr=0;

	// Crank FSM until terminal state reached
	while (state)
	{
		// allocate new zero-filled tape, copy old tape
		// over it, adjust pointers; leaves old tape allocated for now
		if (tapePtr <= newTape | tapePtr >= newTape+tapeSize)
			oldTape = newTape,
			newTape=(char*)calloc(tapeSize, 2),

			tapePtr = oldTape ? (memcpy(newTape+tapeSize/2, oldTape, tapeSize), tapePtr - oldTape + tapeSize/2 + newTape) : newTape + tapeSize,
			minPos = oldTape ? minPos - oldTape + tapeSize/2 + newTape : tapePtr,
			
			tapeSize *= 2;

		// Does this cmdline tuple rule match current (state, tape symbol)?
		argc = GETARG-state | GETARG-*tapePtr%2
			// non-matching rule, advance to next rule
			? argc + 3

			// matching rule; update state, write symbol, move tape head,
			// update low water mark, increment step count, and reset argc
			: (state=GETARG, *tapePtr=GETARG|48, tapePtr+=GETARG,
				minPos -= tapePtr < minPos, ++step, 1);
	}

	// Output results:  written tape contents, followed by # of steps processed
	printf("%s\n%d", minPos, step);

	// per Stroustrup, main needs no return stmt, in which case it returns 0
}
