INCLUDEDIR=../include
OBJ= ar_funs.o arith.o c_args.o cmd.o c_plot.o help.o lfd.o \
     makecmd.o post.o pout.o random.o readfile.o vari.o

catch:
	@ echo "####  Do not run make in this directory."

locfit-c: $(OBJ)
	
.c.o:
	$(CC) $(CFLAGS) -DCVERSION -I$(INCLUDEDIR) -c $<
clean:
	@ rm -f *.o
