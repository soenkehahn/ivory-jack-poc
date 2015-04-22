
all:
	cabal exec -- runhaskell Main.hs
	gcc foo.c -o foo.o -c
	gcc main.c -std=c99 -pedantic-errors -c
	gcc *.o -ljack
	./a.out
