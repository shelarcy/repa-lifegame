make:
	ghc lifegame.hs -rtsopts -threaded -Odph -fllvm -optlo-O3 -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000
test:
	./lifegame +RTS -N2 -K1024000000
clean:
	rm -rf lifegame lifegame.exe lifegame.o lifegame.hi
