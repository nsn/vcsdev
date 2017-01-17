
STELLA=Stella -grabmouse 0
DASM=dasm
FLAGS=-f3 -v5 -I./include/
SRC=$(shell find ./ -maxdepth 2  -name \*.asm)
OBJECTS=${SRC:.asm=.bin}

.PHONY:
all: $(OBJECTS)
	@echo "made $?"

%.bin: %.asm
	$(DASM) $< $(FLAGS) -o$@ -l$(subst .bin,.txt,$@) -s$(subst .bin,.sym,$@)

.PHONY:
run-%: %.bin
	$(STELLA) $<

.PHONY:
debug-%: %.bin
	$(STELLA) -debug $<

.PHONY:
clean:
	rm -f $(OBJECTS) $(OBJECTS:.bin=.txt) $(OBJECTS:.bin=.sym)

depfile:
	for f in $(find . -name \*.asm); do echo -n "$f: "; echo $(grep -i include $f | grep -v "vcs\.h\|macro\.h" | awk '{print $2}' | sed -e 's/"\(.*\)"/\1/g'); done > depfile


