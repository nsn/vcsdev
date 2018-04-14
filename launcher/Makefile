
PATH=..
SPRITES=$(shell find ./sprites -maxdepth 1  -name \*.png)
RUBY=$(shell which ruby)
GENERATED=hero.inc

include ${PATH}/Makefile.tpl

hero.inc: sprites/hero.png
	$(RUBY) $(PATH)/tools/gensprites/gensprites.rb -p HERO_LEFT,HERO_RIGHT -f 3 -w 1 -h 27 -i sprites/hero.png > hero.inc

run: run-launcher

debug: debug-launcher

