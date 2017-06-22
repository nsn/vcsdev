
PATH=..
SPRITES=$(shell find ./sprites -maxdepth 1  -name \*.png)
RUBY=$(shell which ruby)

include ${PATH}/Makefile.tpl

.PHONY:
compassdata.inc: sprites/compass.png 
	$(RUBY) ./gensprites.rb -p COMPASS -f 4 -w 1 -h 8 sprites/compass.png > compassdata.inc

run: run-vcscrawl

debug: debug-vcscrawl

