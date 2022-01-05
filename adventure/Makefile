
all: main.um

adjectives.uh nouns.uh: nouns.in nouns.uh.in adjectives.in adjectives.uh.in generate.pl
	perl generate.pl

HUMLOCK = ../um/humlock/humlock

%.um: %.uml $(wildcard *.uh) nouns.uh adjectives.uh gc.rml ${HUMLOCK}
	time ${HUMLOCK} @MLton max-heap 400m -- -iloptstop 200 -writeasm -writecps -prof -sequence-unit $<;
