build:
	fpc -O3 MyLanguage.pas

debug:
	fpc -g -gl -Xg MyLanguage.pas

clean:
	rm *.o *.ppu MyLanguage
