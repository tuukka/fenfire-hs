
GHC?=ghc
GHCFLAGS=-fglasgow-exts -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans

GHCCMD = $(GHC) $(GHCFLAGS)

TARGETS=vobtest fenfire raptor

all: $(TARGETS)

vobtest: VobTest.hs *.hs
	$(GHCCMD) -o $@ -main-is $(shell basename $< .hs).main --make $<

run-vobtest: vobtest
	./$<

fenfire: Fenfire.hs *.hs
	$(GHCCMD) -o $@ -main-is $(shell basename $< .hs).main --make $<

run-fenfire: ARGS=test.n3
run-fenfire: fenfire
	./$< $(ARGS)

raptor: Raptor.hs *.hs
	$(GHCCMD) -fvia-C -lraptor -o $@ -main-is $(shell basename $< .hs).main --make $<

run-raptor: raptor
	./$< $(ARGS)

clean:
	rm -f *.hi *.o $(TARGETS)
