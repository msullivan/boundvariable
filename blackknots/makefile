.SUFFIXES: .uml
.uml:
	$(HUM) $<

HUM=../um/humlock/humlock

all: bk-parse-test

bk-parse-test: bk-parse-test.uml

bk-test-gen: bk-test-gen.uml

clean: 
	rm -f *.cps *.bins *.sym *.um *.uma

rmcrap:
	rm -f *.cps *.bins *.sym *.uma
