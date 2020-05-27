test-results.txt: gold-output.txt output.txt
	diff -Naur gold-output.txt output.txt

output.txt: testsuite gold-input.txt
	cat gold-input.txt | ./testsuite > output.txt

testsuite: test-driver.cpp people.o
	g++ -o testsuite test-driver.cpp people.o

people.o: people.cpp
	g++ -c -o people.o people.cpp
