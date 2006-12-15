
GHC?=ghc

TARGETS=vobs fenfire

all: $(TARGETS)

vobs: VobTest.hs Vobs.hs
	$(GHC) -o $@ --make $<

fenfire: Fenfire.hs Vobs.hs
	$(GHC) -o $@ --make $<

clean:
	rm -f *.hi *.o $(TARGETS)
