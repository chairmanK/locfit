LDFLAGS=-lm
XLDFLAGS=-L/usr/X11R6/lib -lX11
CC=cc
CFLAGS=

locfit: locfit-x
	
locfit-nox: locfit-d
	
locfit-x: FORCE
	cd src-d; $(MAKE) -f Makefile-x locfit-x; cd ..
	cd src-c; $(MAKE) locfit-c; cd ..
	cd src;   $(MAKE) locfit-c; cd ..
	$(CC) -o locfit src/*.o src-c/*.o src-d/*.o $(LDFLAGS) $(XLDFLAGS)

locfit-d: FORCE
	cd src-d; $(MAKE) locfit-d; cd ..
	cd src-c; $(MAKE) locfit-c; cd ..
	cd src;   $(MAKE) locfit-c; cd ..
	$(CC) -o locfit src/*.o src-c/*.o src-d/*.o $(LDFLAGS)

nda: FORCE
	cd src-gr;$(MAKE) nda; cd ..
	cd src;   $(MAKE) -f Makefile.nda nda; cd ..
	$(CC) -o src-gr/nda src/*.o src-gr/*.o $(LDFLAGS)

liblf: FORCE
	cd src; $(MAKE) -f Makefile.if; cd ..
	ar cr liblf.a src/*.o

#locfit.so: $(OBJS) locfit.o S_enter.o Makefile
#	ld -o locfit.so -shared $(OBJS) locfit.o S_enter.o

iface: FORCE
	cd src-if;  $(MAKE) locfit; cd ..
	cd src;    $(MAKE) -f Makefile.if locfit; cd ..
	$(CC) -o $@ src/*.o src-if/*.o $(LDFLAGS)
iface2: FORCE
	cd src-if;  $(MAKE) locfit; cd ..
	ld -o iface2 -e main src-if/*.o $(LDFLAGS) -lc -L. -llf

imatlb: $(OBJS) locfit.o imatlb.o imatlbc.o Makefile
	$(CC) -o imatlb.o $(CFLAGS) $(OBJS) locfit.o imatlb.o imatlbc.o

FORCE:

very-clean: clean
	rm -f locfit iface locfit.so S.so *.tgz

clean:
	@ cd src;   $(MAKE) clean; cd ..;
	@ cd src-c; $(MAKE) clean; cd ..;
	@ cd src-d; $(MAKE) clean; cd ..;
	@ cd src-s; $(MAKE) clean; cd ..;
	@ cd src-if;$(MAKE) clean; cd ..;
	@ rm -f S/locfit.dat
