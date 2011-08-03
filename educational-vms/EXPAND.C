/* expand.c */

#include <stdio.h>

/* Decompress data from input to output */
void expand (FILE *input, FILE *output)
{
  unsigned char left[256];
  unsigned right[256];
  unsigned stack[30];

  // The file as a whole is a sequence of blocks.
  // The opening byte of each block is a count.
  for (short count= getc(input); count != EOF; count= getc(input)) {

    // For each block we start over, initializing
    // the "left" array to map 0 to 0, 1 to 1, etc.
    for (short i = 0; i < 256; i++) {
      left[i] = i;
    }

    // Each block consists of a pair table,
    // followed by use of that pair table
    for (short c = 0;;) {

      /* Skip range of literal bytes */
      if (count > 127) {
        c += count - 127;
        count = 0;
      }
      if (c == 256) {
	break;
      }

      /* Read pairs, skip right if literal */
      for (short i = 0; i <= count; i++, c++) {
        left[c] = getc(input);
        if (c != left[c]) {
          right[c] = getc(input);
	}
      }
      if (c == 256) {
	break;
      }
      count = getc(input);
    }

    // size is encoded as a pair of bytes.
    short int size = 256 * getc(input) + getc(input);

    /* Unpack data block */
    for (short i = 0;;) {

      /* Pop byte from stack or read byte */
      if (i) {
        c = stack[--i];
      } else {
	size--;
        if (size == 0) {
	  break;
	}
        c = getc(input);
      }

      /* Output byte or push pair on stack */
      if (c == left[c]) {
        putc(c, output);
      } else {
        stack[i++] = right[c];
        stack[i++] = left[c];
      }
    }
  }
}

void main (int argc, char *argv[])
{
  FILE *infile, *outfile;

  if (argc != 3)
    printf("Usage: expand infile outfile\n");
  else if ((infile=fopen(argv[1],"rb"))==NULL)
    printf("Error opening input %s\n",argv[1]);
  else if ((outfile=fopen(argv[2],"wb"))==NULL)
    printf("Error opening output %s\n",argv[2]);
  else {
    expand(infile,outfile);
    fclose(outfile);
    fclose(infile);
  }
}
