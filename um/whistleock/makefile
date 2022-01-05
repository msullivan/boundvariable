
default : whistleock

MLTON_FLAGS = @MLton max-heap 200M --

whistleock : makefile whistleock.cm *.smlutil/*.sml runtime/*.sml backend/*.sml ../../../../sml-lib/util/*.sml ../../../../sml-lib/algo/*.sml
	mlton $(MLTON_FLAGS) whistleock.cm

# should remove some generated files in runtime/...
clean :
	rm -f `find . -name "*~"` whistleock

wc :
	find . -name "*.sml" | grep -v CM | xargs wc -l

linelen :
	linelen `find . -name "*.sml" | grep -v CM`
