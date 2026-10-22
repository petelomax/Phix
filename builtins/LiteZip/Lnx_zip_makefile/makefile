CC=gcc
# Add -g to the next line to create debug version
CFLAGS=-Wall -O
LIBS=
OBJECTS=LiteZip.o

liblitezip.so: $(OBJECTS)
	$(CC) -shared -Wl,-soname,liblitezip.so -o $@ $(OBJECTS) $(LIBS)

LiteZip.o: LiteZip.c ../LiteZip.h
	$(CC) $(CFLAGS) -fPIC -c LiteZip.c

.PHONY: clean
clean:
	rm -f $(OBJECTS) liblitezip.so
