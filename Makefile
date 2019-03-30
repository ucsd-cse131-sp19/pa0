STACK=stack

UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  FORMAT=aout
else
ifeq ($(UNAME), Darwin)
  FORMAT=macho
endif
endif

COURSE=cs131-a
ASGN=pa0
ZIP=$(ASGN).zip

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

zip:
	rm -rf $(ZIP)
	cd lib && zip ../$(ZIP) Hw0.hs
