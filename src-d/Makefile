INCLUDEDIR=../include
OBJ= main.o io.o dev-d.o

catch:
	@ echo "####  Do not run make in this directory."

locfit-d: $(OBJ)
	
.c.o:
	$(CC) $(CFLAGS) -DCVERSION -I$(INCLUDEDIR) -c $<
clean:
	@ rm -f *.o
