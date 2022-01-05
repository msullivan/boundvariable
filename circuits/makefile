HUM=../um/humlock/humlock

.SUFFIXES: .uml
.uml:
	$(HUM) $<

all: 2d-parse-example raytrace-test

raytrace-test: raytrace-test.uml

2d-examples: 2d-examples.uml

2d-parse-example: 2d-parse-example.uml

# no ../circuits since we're here
LANG_DIRS = ../antomaton ../balance ../adventure ../blackknots ../aspects ../story
INCLUDES = $(LANG_DIRS:%=-I %)
INCLUDES += -I ../aspects/ingame
DEPS = $(LANG_DIRS:%=%/*.uml)
DEPS += $(LANG_DIRS:%=%/*.uh)
DEPS += ../antomaton/*.ant ../balance/BALANCE* ../balance/*.mem ../balance/*.srcreg \
  ../balance/*.dstreg ../aspects/ingame/*
HUMLOCK = ../um/humlock/humlock
aspects-in-2d-main.um : aspects-in-2d-main.uml aspects-in-2d.uh \
  ../um/humlock/stdlib/*.uh ${HUMLOCK} ${DEPS}
	${HUMLOCK} @MLton max-heap 300m -- ${INCLUDES} -writecps -prof -writeasm -gcratio 10 -iloptstop 200 aspects-in-2d-main.uml

clean: 
	rm -f *.cps *.bins *.sym *.um *.uma

rmcrap:
	rm -f *.cps *.bins *.sym *.uma
