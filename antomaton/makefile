
all: antomaton.um

antomaton.um: *.uml puzzle*.ant
	../um/humlock/humlock @MLton max-heap 200m -- -sequence-unit antomaton-main.uml

bench : antomaton.um
	time ../um/c-tom/um antomaton.um < bench.ant > /dev/null

clean :
	rm -f *.cps *.sym *.bins *.uma
