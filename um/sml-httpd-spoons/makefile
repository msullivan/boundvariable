um : um.sml um.mlb makefile
	mlton @MLton max-heap 200m -- -expert true -const 'MLton.detectOverflow false' -const 'MLton.safe false' um.mlb
	strip um

um-safe : um.sml um.mlb makefile
	mlton @MLton max-heap 200m -- -output um-safe um.mlb
	strip um-safe

bench : um
	time ./um ../test-binaries/fact.161.um
