#ifndef TABLE_H
#define TABLE_H

/*
 * This is the interface to the table of names and definitions.
 * Based on Kernighan and Plaugher's Software Tools, Chapter 8.2.
 */

/* max chars in a token */
#define MAX_TOKEN 200
/* max chars in a definition */
#define MAX_DEFINITION 200

enum found { YES = 1, NO = 0 };
int lookup(const char name[MAX_TOKEN], char definition[MAX_DEFINITION]);
void install(const char name[MAX_TOKEN], const char definition[MAX_DEFINITION]);

#endif

