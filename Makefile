GHC=ghc
SOURCES=syntax.hs parser.hs eval.hs main.hs
PROGNAME=main

all:	$(SOURCES)
	$(GHC) -o $(PROGNAME) $(SOURCES)

clean:
	rm -rf $(PROGNAME) *.hi *.o
