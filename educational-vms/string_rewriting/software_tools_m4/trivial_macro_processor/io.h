#ifndef IO_H
#define IO_H

/*
 * This is the interface to the buffered input.
 * Based on Kernighan and Plaugher's Software Tools, Chapter 8.1.
 */

void put_back(char); /* push a character back on the buffered input */
int get_char(); /* get a (possibly pushed back) character */

#endif



