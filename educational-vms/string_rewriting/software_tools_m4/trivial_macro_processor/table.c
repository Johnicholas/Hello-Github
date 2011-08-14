#include "table.h"

#include <string.h>

/* consider pulling these configuration variables out into another header */
#define MAX_POINTER 50
#define MAX_TABLE 500

/* name pointers */
static int name_pointer[MAX_POINTER];
/* last used in name_pointer */
static int last_pointer;
/* actual text of names and definitions */
static char table[MAX_TABLE];
/* last used in table */
static int last_table;

int lookup(const char* name, char* definition) {
  int i;
  int j;

  /* Note: we search backwards so that later definitions
   * will override earlier definitions.
   */
  for (i= last_pointer - 1; i >= 0; --i) {
    char* candidate_match= table + name_pointer[i];
    for (j= 0; candidate_match[j] == name[j] && name[j] != '\0'; ++j) {
      ; /* deliberately empty loop body */
    }
    if (candidate_match[j] == name[j]) {
      /* found */
      strcpy(definition, candidate_match + j + 1);
      return YES;
    }
  }
  return NO;
}

void install(const char* name, const char* definition) {
  int name_length= strlen(name) + 1;
  int definition_length= strlen(definition) + 1;

  name_pointer[last_pointer++]= last_table;
  strcpy(table + last_table, name);
  last_table += name_length;
  strcpy(table + last_table, definition);
  last_table += definition_length;
}

