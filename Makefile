
BASE = boundvariable

all: ${BASE}.tar.bz2 ${BASE}.zip

.PHONY: ${BASE}.tar.bz2 ${BASE}.zip

${BASE}.tar.bz2:
	rm -f /tmp/${BASE} $@;
	ln -s `pwd` /tmp/${BASE};
	(cd /tmp && tar jvchf $@ --exclude-from ${BASE}/.excludes --exclude ${BASE}.\* ${BASE});
	mv /tmp/$@ ./$@;
	rm /tmp/${BASE};

${BASE}.zip:
	rm -f /tmp/${BASE} $@;
	ln -s `pwd` /tmp/${BASE};
	(cd /tmp && zip -r $@ ${BASE} -x \*/.svn/\* -x \*~ -x \*/.cm/\* \
	  -x \*/.CM/\* -x \*/core -x \*.uma -x \*.sym -x \*.bins -x \*.cps \
	  -x \*/${BASE}.\*);
	mv /tmp/$@ ./$@;
	rm /tmp/${BASE};
