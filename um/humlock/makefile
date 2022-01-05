
default : humlock.exe

MLTON_FLAGS = @MLton max-heap 200M --

humlock.exe : humlock
	rm -f humlock.exe
	cp humlock humlock.exe

humlock : makefile humlock.cm *.sml front/*.sml el/*.sml parser/*.sml util/*.sml cps/*.sml il/*.sml runtime/*.sml backend/*.sml ../../../../sml-lib/util/*.sml ../../../../sml-lib/algo/*.sml
	mlton $(MLTON_FLAGS) humlock.cm

# should remove some generated files in runtime/...
clean :
	rm -f `find . -name "*~"` *.exe humlock

wc :
	find . -name "*.sml" | grep -v CM | grep -v \\.cm | xargs wc -l

linelen :
	linelen `find . -name "*.sml" | grep -v CM`
