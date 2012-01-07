INCLUDEDIR=../include
CFLAGS=
OBJ=iface.o

catch:
	@echo "####  Do not run make in this directory."

locfit: $(OBJ)
	
.c.o:
	$(CC) $(CFLAGS) -DINTERFACE -I$(INCLUDEDIR) -c $<
clean:
	@ rm -f *.o
