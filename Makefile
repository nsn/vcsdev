
STELLA=Stella -grabmouse 0
DASM=dasm
FLAGS=-f3 -v5 -I./include/
SRC=$(shell find ./ -name \*.asm)
OBJECTS=${SRC:.asm=.bin}

.PHONY:
all: $(OBJECTS)
	@echo "made $?"

%.bin: %.asm
	$(DASM) $< $(FLAGS) -o$@ -l$(subst .bin,.txt,$@)

.PHONY:
run-%: %.bin
	$(STELLA) $<

.PHONY:
debug-%: %.bin
	$(STELLA) -debug $<

.PHONY:
clean:
	rm -f $(OBJECTS) $(OBJECTS:.bin=.txt)

