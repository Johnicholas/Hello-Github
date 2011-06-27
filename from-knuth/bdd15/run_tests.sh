#!/bin/bash

diff -u <(echo "Usage: ./bdd15 [commandfile]") <(./bdd15 foo bar 2>&1) 
diff -u example_zddl_out.txt <(./bdd15 <example_zddl.txt)
