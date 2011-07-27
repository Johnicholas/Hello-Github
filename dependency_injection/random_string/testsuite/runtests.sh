#!/bin/bash
# Johnicholas Hines Febuary 19 2007
# The naming convention is:
# .grammar for grammar files
# .in for input files (on stdin)
# .gold for correct output
# .out for most recent actual output
#
# This script ought to run itself on every .grammar/.in pair in testdata,
# but I don't know much bash scripting, and so I just keep editing the 
# list of tests in braces.

for i in {1,2,3,4,5,6,7,8,9,10,C,C2}
do
  echo "Running $i"
  if [ -f testdata/$i.gold ] 
  then
    ../random_string -g testdata/$i.grammar < testdata/$i.in > testdata/$i.out
    diff -Naur testdata/$i.out testdata/$i.gold
  else
    ../random_string -g testdata/$i.grammar < testdata/$i.in | tee testdata/$i.out
  fi
done
