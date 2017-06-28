
PATH=..
SPRITES=$(shell find ./sprites -maxdepth 1  -name \*.png)
RUBY=$(shell which ruby)

include ${PATH}/Makefile.tpl

compassdata.inc: sprites/compass.png 
	$(RUBY) ./gensprites.rb -p COMPASS -f 4 -w 1 -h 8 -t 4 -b 4 -i sprites/compass.png > compassdata.inc

digitsdata.inc: sprites/digits.png 
	$(RUBY) ./gensprites.rb -p DIGITS -f 10 -w 1 -h 5 -t 0 -b 0 -i sprites/digits.png > digitsdata.inc

run: run-vcscrawl

debug: debug-vcscrawl

