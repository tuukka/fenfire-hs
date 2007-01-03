
GHC?=ghc
GHCFLAGS=-fglasgow-exts -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

GHCCMD = $(GHC) $(GHCFLAGS)

SOURCES=*.hs *.chs Raptor.hs
TARGETS=vobtest fenfire

all: $(TARGETS)

vobtest: VobTest.hs $(SOURCES)
	$(GHCCMD) -o $@ -main-is $(shell basename $< .hs).main --make $<

run-vobtest: vobtest
	./$<

fenfire: Fenfire.hs $(SOURCES)
	$(GHCCMD) -fvia-C -lraptor -o $@ -main-is $(shell basename $< .hs).main --make $<

run-fenfire: ARGS=test.n3
run-fenfire: fenfire
	./$< $(ARGS)

Raptor.hs: Raptor.chs
	c2hs $<

clean:
	rm -f *.hi Raptor.chi Raptor.h Raptor.hs Raptor_stub.* *.o $(TARGETS)
