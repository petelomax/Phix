CC=gcc
CFLAGS=-Wall -O
LIBS=
OBJECTS=LiteUnzip.o
# Comment the next line to create version without debugging
LDFLAGS=-g

libliteunzip.so: $(OBJECTS)
	$(CC) $(LDFLAGS) -shared -Wl,-soname,libliteunzip.so -o $@ $(OBJECTS) $(LIBS)

LiteUnzip.o: LiteUnzip.c ../LiteUnzip.h
	$(CC) $(CFLAGS) -fPIC -c LiteUnzip.c

.PHONY: clean
clean:
	rm -f $(OBJECTS) libliteunzip.so
