
PiForall: Lexer.hs
	@~/bin/my-ghc $<

Lexer.hs: Lexer.x
	alex $<

Parser.hs: Parser.y
	happy $<

clean:
	rm -f PiForall.hs PiForall.hi PiForall.o PiForall

.PHONY: clean

