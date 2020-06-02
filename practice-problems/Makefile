.PHONY: all
all: run-test

.PHONY: clean
clean:
	rm -f test-driver *.o

.PHONY: run-test
run-test: test-driver
	./test-driver

test-driver: googletest-master/bazel-bin/libgtest_main.a people-indexes-test.o people.o googletest-master/bazel-bin/libgtest.a
	g++ -o test-driver googletest-master/bazel-bin/libgtest_main.a people-indexes-test.o people.o googletest-master/bazel-bin/libgtest.a -lpthread

people-indexes-test.o: googletest-master/googletest/include/gtest/gtest.h googletest-master/googlemock/include/gmock/gmock.h people-indexes-test.cpp
	g++ -c -o people-indexes-test.o -Igoogletest-master/googletest/include -Igoogletest-master/googlemock/include people-indexes-test.cpp

people.o: people.cpp
	g++ -c -o people.o people.cpp
