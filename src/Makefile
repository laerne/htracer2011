EXEC=ingi2325-nbrack
CC=ghc --make
#CFLAGS=-O2 -fvia-C -threaded
CFLAGS=
RM=rm -f
MAIN=Main.hs
SRCS=$(wildcard *.hs)
OBJS=$(SRCS:%.hs=%.o)
HALF_OBJ=$(SRCS:%.hs=%.hi)


$(EXEC): $(SRCS)
	$(CC) $(CFLAGS) -o $@ $(MAIN)


#%.o : %.hs
#	$(CC) -c $<
#
#.PHONY: mproper clean

	
	
mproper: clean
	$(RM) $(EXEC)

clean:
	$(RM) $(OBJS) $(HALF_OBJ)
