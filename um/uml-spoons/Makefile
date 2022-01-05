
HUMLOCK = ../humlock/humlock

%.um: %.uml $(wildcard *.uh)${HUMLOCK}
	${HUMLOCK} -iloptstop 200 -sequence-unit $<;

mk-uum: mk-uum.c
	gcc -o mk-uum mk-uum.c
