
INCLUDES =  -I ingame

all: story-main.um

clean : 
	rm -f *.bins *.cps *.sym *.uma *.um mlmon.out

tidy : 
	rm -f *.bins *.cps *.sym *.uma mlmon.out

story-main.um: *.uml *.uh STORY_CONTEST* ../um/humlock/humlock
	../um/humlock/humlock ${INCLUDES} -root ../um/humlock/ story-main.uml

run : story-main.um
	time ../um/c-tom/um story-main.um