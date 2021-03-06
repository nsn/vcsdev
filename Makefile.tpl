
STELLA=Stella
DASM=dasm
FLAGS=-f3 -v5 -I$(PATH)/include/ -v4
SRC=$(shell find . -maxdepth 1  -name \*.asm)
OBJECTS=${SRC:.asm=.bin}

.PHONY:
all: $(OBJECTS) 
	@echo "made $?"

%.bin: %.asm
	$(DASM) $< $(FLAGS) -o$@ -l$(subst .bin,.txt,$@) -s$(subst .bin,.sym,$@)

.PHONY:
run-%: %.bin
	$(STELLA) -grabmouse 0 $<

.PHONY:
debug-%: %.bin
	$(STELLA) -debug $<

.PHONY:
clean:
	rm -f $(OBJECTS) $(OBJECTS:.bin=.txt) $(OBJECTS:.bin=.sym) depfile $(GENERATED)

depfile: $(SRC)
	$(shell sh $(PATH)/depgen.sh $(SRC) > depfile)
   
sinclude depfile

