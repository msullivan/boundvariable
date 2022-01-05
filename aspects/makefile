
INCLUDES =  -I ingame,../circuits

all: aspects-main.um

clean : 
	rm -f *.bins *.cps *.sym *.uma *.um mlmon.out *.prf

tidy : 
	rm -f *.bins *.cps *.sym *.uma *.prf mlmon.out

aspects-main.um: *.uml *.uh ingame/* ../um/humlock/humlock  ../um/humlock/stdlib/*
	../um/humlock/humlock ${INCLUDES} -root ../um/humlock/ aspects-main.uml

xml-testing-main.um: *.uml *.uh ingame/*
	../um/humlock/humlock ${INCLUDES} -I sol  -root ../um/humlock/ xml-testing-main.uml

aspects-main.prof : aspects-main.um 
	time ../um/c-prof/um aspects-main.um 2> aspects-main.prf
	../um/c-prof/runprof aspects-main.sym aspects-main.um aspects-main.prf aspects-main.bins | sort -n -r > testing/aspects-main-profile.txt

xml-testing-main.run : xml-testing-main.um
	time ../um/c-tom/um xml-testing-main.um

run : aspects-main.um
	time ../um/c-tom/um aspects-main.um
