build:
	fpc -O3 MyLanguage.pas

debug:
	fpc -g -gl MyLanguage.pas

clean:
	rm *.o *.ppu MyLanguage
