
GHC?=ghc

TARGETS=vobs fenfire

all: $(TARGETS)

vobs: Vobs.hs
	$(GHC) -o $@ -main-is Vobs.myMain --make $<

fenfire: Fenfire.hs Vobs.hs
	$(GHC) -o $@ --make $<

clean:
	rm -f *.hi *.o $(TARGETS)
