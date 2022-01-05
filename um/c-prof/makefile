
all: um runprof

um: um.c makefile
	gcc -Wall -O3 -fomit-frame-pointer $< -o $@

runprof: runprof.c makefile
	gcc -Wall -O3 -fomit-frame-pointer $< -o $@

clean: 
	$(RM) um runprof

.PHONY: clean all
