

ucc.um : *.uh *.uml
	../humlock/humlock main.uml -o ucc.um

um.um : io.c std.c um.c ucc.um
	cat io.c std.c um.c | um ucc.um > um.um

parse : *.uh *.uml
	../humlock/humlock parse.uml -o parse.um
