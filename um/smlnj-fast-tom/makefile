
.phony:

um : um.sml um.mlb makefile .phony
	echo 'CM.make "um.cm"; SMLofNJ.exportFn ("um", Main.main)' | sml

bench : um
	time ./um ../test-binaries/fact.161.um
