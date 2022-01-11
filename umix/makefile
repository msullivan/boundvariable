
default : umix.um

LANG_DIRS = ../antomaton ../balance ../circuits ../adventure ../blackknots ../aspects ../story ../um/uml-spoons ../um/ucc

INCLUDES = $(LANG_DIRS:%=-I %)
INCLUDES += -I ../aspects/ingame

DEPS = $(LANG_DIRS:%=%/*.uml)
DEPS += $(LANG_DIRS:%=%/*.uh)
DEPS += ../antomaton/*.ant ../balance/BALANCE* ../balance/*.mem ../balance/*.srcreg \
  ../balance/*.dstreg ../aspects/ingame/*

HUMLOCK = ../um/humlock/humlock

GCRATIO = 3

umix.um : umix.uml hack_bas.uh qbasic.uh mail.uh roman.uh \
  ../um/humlock/stdlib/*.uh ${HUMLOCK} ${DEPS}
	${HUMLOCK} ${INCLUDES} -writecps -prof -writeasm -gcratio ${GCRATIO} -iloptstop 200 umix.uml

run : umix.um
	../um/c-tom/um umix.um

bench : umix.um bench.in
	time ../um/c-tom/um umix.um < bench.in

umix.prof : umix.um bench.in
	time ../um/c-prof/um umix.um < bench.in 2>umix.prof

prof.txt : umix.prof umix.sym umix.bins
	../um/c-prof/runprof umix.sym umix.um umix.prof umix.bins | sort -n -r > prof.txt

clean :
	rm -f *.cps *.bins *.sym *.um *.umz *.uma

testy :
	${HUMLOCK} ${INCLUDES} -prof testy.uml -o testy.um

# always ready
codex.umz : .dummy
	${HUMLOCK} ${INCLUDES} -revision 9 -scloadprog -writecps -prof -writeasm -print -crypt -sd -junk -obsouter -gcdebug -gcratio ${GCRATIO}    umix.uml -o codex.umz

.dummy :

