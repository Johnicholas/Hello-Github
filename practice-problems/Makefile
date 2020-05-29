.PHONY: run-test
run-test: test-driver
	./test-driver

test-driver: googletest-master/bazel-bin/libgtest_main.a people_indexes_test.o people.o googletest-master/bazel-bin/libgtest.a
	g++ -o test-driver googletest-master/bazel-bin/libgtest_main.a people_indexes_test.o people.o googletest-master/bazel-bin/libgtest.a -lpthread

people_indexes_test.o: googletest-master/googletest/include/gtest/gtest.h googletest-master/googlemock/include/gmock/gmock.h people_indexes_test.cpp
	g++ -c -o people_indexes_test.o -Igoogletest-master/googletest/include -Igoogletest-master/googlemock/include people_indexes_test.cpp

people.o: people.cpp
	g++ -c -o people.o people.cpp
