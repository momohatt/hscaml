GHC=ghc
SOURCES=syntax.hs lexer.hs parser.hs type.hs typing.hs eval.hs main.hs
PROGNAME=main

all:	$(SOURCES)
	alex lexer.x
	happy parser.y
	$(GHC) -o $(PROGNAME) $(SOURCES)

clean:
	rm -rf $(PROGNAME) *.hi *.o
