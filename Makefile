
GHC?=ghc

TARGETS=vobtest fenfire

all: $(TARGETS)

vobtest: VobTest.hs *.hs
	$(GHC) -o $@ --make $<

fenfire: Fenfire.hs *.hs
	$(GHC) -o $@ --make $<

clean:
	rm -f *.hi *.o $(TARGETS)
