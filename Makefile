STACK=stack

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

COURSE=cs131w
ASGN=pa0
TARBALL=$(ASGN).tgz

test: clean
	$(STACK) test 

bin:
	$(STACK) build

clean: 
	$(STACK) clean

distclean: clean 
	rm -rf .stack-work 

tags:
	hasktags -x -c lib/

turnin: 
	rm -rf $(TARBALL)
	tar -zcvf ../$(TARBALL) --exclude .stack-work --exclude .git ../$(ASGN)
	mv ../$(TARBALL) . 
	turnin -c $(COURSE) ./$(TARBALL)

