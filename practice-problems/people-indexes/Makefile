.PHONY: all
all: run-test

.PHONY: clean
clean:
	rm -f test-driver *.o

.PHONY: run-test
run-test: test-driver
	./test-driver

test-driver: googletest-master/bazel-bin/libgtest_main.a people-indexes-test.o people-indexes.o googletest-master/bazel-bin/libgtest.a golden-people-indexes.o
	g++ -o test-driver googletest-master/bazel-bin/libgtest_main.a people-indexes-test.o people-indexes.o googletest-master/bazel-bin/libgtest.a -lpthread golden-people-indexes.o

people-indexes-test.o: googletest-master/googletest/include/gtest/gtest.h googletest-master/googlemock/include/gmock/gmock.h people-indexes-test.cpp golden-people-indexes.h
	g++ -c -o people-indexes-test.o -Igoogletest-master/googletest/include -Igoogletest-master/googlemock/include people-indexes-test.cpp

people-indexes.o: people-indexes.cpp
	g++ -c -o people-indexes.o people-indexes.cpp

golden-people-indexes.o: golden-people-indexes.cpp golden-people-indexes.h
	g++ -c -o golden-people-indexes.o golden-people-indexes.cpp
