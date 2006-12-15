
GHC?=ghc
GHCFLAGS=-Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans

GHCCMD = $(GHC) $(GHCFLAGS)

TARGETS=vobtest fenfire

all: $(TARGETS)

vobtest: VobTest.hs *.hs
	$(GHCCMD) -o $@ --make $<

fenfire: Fenfire.hs *.hs
	$(GHCCMD) -o $@ --make $<

clean:
	rm -f *.hi *.o $(TARGETS)
