
GHC?=ghc
GHCFLAGS=-fglasgow-exts -hide-package haskell98 -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

GHCCMD = $(GHC) $(GHCFLAGS)

PREPROCESSED=$(patsubst %.fhs,%.hs,$(wildcard *.fhs)) \
             $(patsubst %.chs,%.hs,$(wildcard *.chs))
SOURCES=*.hs *.chs *.fhs $(PREPROCESSED) Raptor.o
TARGETS=vobtest functortest fenfire

all: $(TARGETS)

vobtest: VobTest.hs $(SOURCES)
	$(GHCCMD) -o $@ -main-is $(basename $<).main --make $<
	touch $@

run-vobtest: vobtest
	./$<

fenfire: Fenfire.hs $(SOURCES)
	$(GHCCMD) -lraptor -o $@ -main-is $(basename $<).main --make $<
	touch $@

run-fenfire: ARGS=test.nt
run-fenfire: fenfire
	./$< $(ARGS)

# __attribute__ needs to be a no-op until c2hs learns to parse them in raptor.h
%.hs: %.chs
	c2hs --cppopts '-D"__attribute__(A)= "' $<

Raptor.o: Raptor.hs
	$(GHCCMD) -c -fvia-C -o $@ $<

functortest: FunctorTest.hs $(SOURCES)
	$(GHCCMD) -o $@ -main-is $(basename $<).main --make $<
	touch $@

run-functortest: functortest
	./$<


%.hs: %.fhs
	trhsx $< $@

clean:
	rm -f $(PREPROCESSED) *.hi *.i *.chi Raptor.h Raptor_stub.* *.o $(TARGETS)
