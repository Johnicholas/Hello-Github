LDLIBS=-L/usr/local/lib -ltolua++ -llua -llualib -ldl -lm

test: list_of_trees list_of_trees.lua
	./list_of_trees
.PHONY: test

list_of_trees: list_of_trees.cpp list_of_trees.h list_of_trees_bind.c
	g++ $(CFLAGS) list_of_trees.cpp -o list_of_trees $(LDLIBS)
TO_CLEAN += list_of_trees

list_of_trees_bind.c: list_of_trees.pkg
	tolua++ list_of_trees.pkg >list_of_trees_bind.c
TO_CLEAN += list_of_trees.bind.c

clean:
	rm -f $(TO_CLEAN)
.PHONY: clean

