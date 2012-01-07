INCLUDEDIR=../include
include ./LOCAL_FILES

catch:
	@ echo "####  Do not run make in this directory."
locfit-c: $(OBJ);
	
.c.o:
	$(CC) $(CFLAGS) -DCVERSION -I$(INCLUDEDIR) -c $<
clean:
	@ rm -f *.o
