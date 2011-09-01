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

#include<iostream>
#define Z atoi(b[a++])
int main(int a,char**b){int c=a=1,d=0,e=2;char*f,*g=0,*h,*i=0;while(c){if(i<=g|i>=g+e)f=g,g=(char*)calloc(e,2),i=f?(memcpy(g+e/2,f,e),i-f+e/2+g):g+e,h=f?h-f+e/2+g:i,e*=2;a=Z-c|Z-*i%2?a+3:(c=Z,*i=Z|48,i+=Z,h-=i<h,++d,1);}printf("%s\n%d",h,d);}
