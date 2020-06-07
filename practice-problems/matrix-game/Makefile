.PHONY: all
all: test all-together-now

.PHONY: test
test: matrix-game golden-input.txt golden-output.txt all-together-now
	./matrix-game <golden-input.txt | diff -Naur - golden-output.txt
	./all-together-now <golden-input.txt | diff -Naur - golden-output.txt

all-together-now: all-together-now.cpp
	g++ -o all-together-now all-together-now.cpp

matrix-game: matrix-game-driver.cpp matrix-game.o matrix-game.h
	g++ -o matrix-game matrix-game-driver.cpp matrix-game.o

matrix-game.o: matrix-game.cpp matrix-game.h
	g++ -c -o matrix-game.o matrix-game.cpp
