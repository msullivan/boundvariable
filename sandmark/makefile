
default : sandmark.umz fastmark.umz

HUMLOCK = ../um/humlock/humlock

clean :
	rm -f *.cps *.bins *.sym *.um *.uma

# always ready
sandmark.umz : .dummy
	${HUMLOCK} ${INCLUDES} -scloadprog -scverbose -sd -sdprog -gcratio 10 sandmark.uml -o sandmark.umz

sandmark.um : .dummy
	${HUMLOCK} ${INCLUDES} -gcratio 10 sandmark.uml -o sandmark.um

fastmark.umz : .dummy
	${HUMLOCK} ${INCLUDES} -scloadprog -scverbose -writecps -prof -writeasm -sd -sdprog -gcratio 10 fastmark.uml -o fastmark.umz


.dummy :

