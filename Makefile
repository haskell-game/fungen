HC      = ghc
HC_OPT  = -i.. -O2

all:
	$(HC) $(HC_OPT) --make FunGEn.hs
	$(HC) $(HC_OPT) --make game.hs

clean:
	rm -f *.o *.hi
