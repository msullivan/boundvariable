
# um.s : um.c makefile
# 	gcc -Wall -O3 -fomit-frame-pointer -march=i686 um.c -S

um: um.s makefile
	gcc -Wall -O3 -fomit-frame-pointer -march=i686 um.s -o um
	strip um

bench : um
	time ./um ../../umix/umix.um < ../../umix/bench.in
