
PATH=..
SPRITES=$(shell find ./sprites -maxdepth 1  -name \*.png)
RUBY=$(shell which ruby)
GENERATED=sprites.inc

include ${PATH}/Makefile.tpl

sprites.inc: assets/sprites.png
	$(RUBY) $(PATH)/tools/gensprites/gensprites.rb -p ZOMBIE,PLANT -f 2 -w 1 -h 28 -i assets/sprites.png > sprites.inc

run: run-lanes

debug: debug-lanes

