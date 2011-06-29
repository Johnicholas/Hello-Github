
#define gb_next_rand() (*gb_fptr>=0?*gb_fptr--:gb_flip_cycle())
extern long* gb_fptr;
extern long gb_flip_cycle();

extern void gb_init_rand();

extern long gb_unif_rand();

