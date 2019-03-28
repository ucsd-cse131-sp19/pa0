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
ASGN=00
COMPILER=warmup

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
	# rm -rf .stack-work
	rm -rf $(ASGN)-$(COMPILER).tgz
	tar -zcvf ../$(ASGN)-$(COMPILER).tgz --exclude .stack-work --exclude .git ../$(ASGN)-$(COMPILER) 	
	mv ../$(ASGN)-$(COMPILER).tgz . 
	turnin -c $(COURSE) ./$(ASGN)-$(COMPILER).tgz

