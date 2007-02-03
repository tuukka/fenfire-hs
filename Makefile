
GHC?=ghc
GHCFLAGS=-fglasgow-exts -hide-package haskell98 -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

#GHCFLAGS+=-O -fexcess-precision -optc-ffast-math -optc-O3 
#crash: -optc-march=pentium4 -optc-mfpmath=sse

GHCCMD = $(GHC) $(GHCFLAGS)

PREPROCESSED=$(patsubst %.fhs,%.hs,$(wildcard *.fhs)) \
             $(patsubst %.chs,%.hs,$(wildcard *.chs))
SOURCES=*.hs *.chs *.fhs $(PREPROCESSED)
TARGETS=functortest vobtest fenfire

all: $(TARGETS)

profilable:
	rm -f $(TARGETS)
	$(MAKE) all
	rm -f $(TARGETS)
	$(MAKE) all "GHCFLAGS=-prof -auto-all -hisuf p_hi -osuf p_o $(GHCFLAGS)"
non-profilable:
	rm -f $(TARGETS)
	$(MAKE) all

functortest: FunctorTest.hs $(SOURCES)
vobtest: VobTest.hs $(SOURCES)
fenfire: opts=-lraptor
fenfire: Fenfire.hs $(SOURCES)
functortest vobtest fenfire:
	$(GHCCMD) $(opts) -o $@ -main-is $(basename $<).main --make $<
	touch $@

run-functortest: functortest
run-vobtest: vobtest
run-fenfire: ARGS=test.nt
run-fenfire: fenfire
run-%: %
	./$< $(ARGS)

clean:
	rm -f $(PREPROCESSED) *.p_hi *.hi *.i *.chi Raptor.h Raptor_stub.* *.p_o *.o $(TARGETS)

# __attribute__ needs to be a no-op until c2hs learns to parse them in raptor.h
%.hs: %.chs
	c2hs --cppopts '-D"__attribute__(A)= "' $<

%.hs: %.fhs
	trhsx $< $@
