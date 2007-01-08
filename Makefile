
GHC?=ghc
GHCFLAGS=-fglasgow-exts -hide-package haskell98 -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

GHCCMD = $(GHC) $(GHCFLAGS)

SOURCES=*.hs *.chs Raptor.hs Raptor.o
TARGETS=vobtest fenfire

all: $(TARGETS)

vobtest: VobTest.hs $(SOURCES)
	$(GHCCMD) -o $@ -main-is $(shell basename $< .hs).main --make $<
	touch $@

run-vobtest: vobtest
	./$<

fenfire: Fenfire.hs $(SOURCES)
	$(GHCCMD) -lraptor -o $@ -main-is $(shell basename $< .hs).main --make $<
	touch $@

run-fenfire: ARGS=test.nt
run-fenfire: fenfire
	./$< $(ARGS)

# __attribute__ needs to be a no-op until c2hs learns to parse them in raptor.h
Raptor.hs: Raptor.chs
	c2hs --cppopts '-D"__attribute__(A)= "' $<

Raptor.o: Raptor.hs
	$(GHCCMD) -c -fvia-C -o $@ $<

clean:
	rm -f *.hi *.i Raptor.chi Raptor.h Raptor.hs Raptor_stub.* *.o $(TARGETS)
