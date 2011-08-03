# the project consists of five object files
# two of which were not adding much
OBJ = m4.o m4ext.o m4macs.o

# to compile a .o file from a .c file use a c compiler in the usual way
.c.o: ; $(CC) -c $(CFLAGS) $(CPPFLAGS) $(WARN) $<

# The default target, "all" means "m4"
all: m4

# The executable "m4" requires all the objects
# Build it by linking them together in the usual way
m4: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) $(LIBS) -o m4

# install using the "install" utility
install: all
	test -d $(ROOT)$(BINDIR) || mkdir -p $(ROOT)$(BINDIR)
	$(INSTALL) -c -m 755 m4 $(ROOT)$(BINDIR)/m4
	$(STRIP) $(ROOT)$(BINDIR)/m4

clean:
	rm -f m4 $(OBJ) core log *~

mrproper: clean

m4.o: m4.c m4.h
m4ext.o: m4ext.c m4.h
m4macs.o: m4macs.c m4.h

