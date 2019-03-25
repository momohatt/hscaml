GHC=ghc
SOURCES=syntax.hs lexer.hs parser.hs type.hs typing.hs eval.hs main.hs
PROGNAME=main

all:	$(SOURCES)
	$(GHC) -o $(PROGNAME) $(SOURCES)

lexer.hs: lexer.x
	alex lexer.x

parser.hs: parser.y
	happy parser.y

clean:
	rm -rf $(PROGNAME) *.hi *.o lexer.hs parser.hs
