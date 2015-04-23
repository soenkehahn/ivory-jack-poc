
default: play

play: a.out
	./a.out

a.out: foo.o main.o
	gcc *.o -ljack -O11

foo.o: foo.c
	gcc foo.c -o foo.o -c -O11

foo.c: Main.hs
	cabal exec -- runhaskell -Wall -Werror Main.hs

main.o: main.c
	gcc main.c -std=c99 -pedantic-errors -c -O11

clean:
	rm -rf a.out foo.* ivory.* ivory_asserts.h main.o
