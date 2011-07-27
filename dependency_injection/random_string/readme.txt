Johnicholas Hines Febuary 16 2007

This is a tool for generating strings from a grammar.
It can be used to generate the shortest string from a grammar, or all
of the strings from the grammar from shortest to longer, or it could
be used to generate random strings from a grammar.

It reads a context free grammar file, and constructs a bijective
function that transforms any nonnegative integer to a grammatical
string. Then it reads stdin, which it expects to be a sequence of
(possibly large) nonnnegative ascii decimal integers separated by
white space, and outputs the corresponding grammatical strings.

Syntax of the grammar file:
The grammar file consists of a nonempty sequence of productions,
separated or terminated by ".", which must be surrounded by white
space. Each production consists of a word, followed by "::=", followed
by one or more words. The words can consist of at least [a-zA-Z0-9_],
and need to be surrounded by white space.

There is no provision for comments yet.

Nonterminals are those words which appear on the left hand side of
some production. Terminals are those words which do not appear on the
left hand side of any production. The start symbol is the left hand
side of the first production.

Bugs:
It is currently impossible to generate strings which contain white
space. Similarly, "." is awkward.

Empty right hand sides are not yet supported.

If you ask for the Nth string from a finite language of size less than
N, it will go into an endless loop consuming more and more memory.

The function this tool produces is not guaranteed to be bijective if
the context free grammar is ambiguous. It might produce each string a
number of times equal to the number of parse trees of that string, or
it might not.

There are essentially no error messages. If it encounters something
unexpected, it dies.

Installation:
$ make

Usage:
$ ./random_string --usage
$ echo "0" | ./random_string -g testsuite/C.grammar