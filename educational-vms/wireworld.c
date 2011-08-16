// This is based on the wireworld computer by David Moore and Mark Owen 
// (described at quinpalus.com)
#include <assert.h>
#include <stdio.h>
#include <stdint.h>

// registers
uint16_t r[64];
#define PC (r[63])

uint16_t read(uint16_t which) {
  assert(which < 64);
  switch (which) {
  case 0: return 0; break;
  case 53: return r[54] & ~r[53]; break;
  case 54: return r[53] & ~r[54]; break;
  case 55: return 0; break;
  case 56: return r[56] ? r[55] : r[57]; break;
  case 57: return 0; break;
  case 58: return r[58] >> 1; break; // note: should be rotated, not shifted
  case 59: return r[59] << 1; break; // note, should be rotated, not shifted
  case 61: return r[60]+r[61]; break;
  case 62: return ~r[62]; break;
  default:
    return r[which];
    break;
  }
}

void write(uint16_t which, uint16_t what) {
  assert(which < 64);
  if (which == 0) {
    printf("%d\n", what);
  } else {
    r[which]= what;
  }
}

void run() {
  while (1) {
    uint16_t dest= (r[r[63]] >> 8) & 0x3F;
    uint16_t src= r[r[63]] & 0x3F;
    r[63]++;
    write(dest, read(src));
  }
}

void load(FILE* to_load) {
  int i;
  for (i= 0; i < 64; ++i) {
    if (fscanf(to_load, " %d", r+i) != 1) {
      break;
    }
  }
}

int main(int argc, char* argv[]) {
  assert(argc == 2);
  FILE* program= fopen(argv[1], "r");
  assert(program);
  load(program);
  run();
  return 0;
}
