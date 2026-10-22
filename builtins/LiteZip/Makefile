# === CONFIGURATION ===
CC      = x86_64-w64-mingw32-gcc
CFLAGS  = -Wall -Os -fomit-frame-pointer -fno-asynchronous-unwind-tables -fno-diagnostics-color -DNDEBUG
LDFLAGS = -shared -lkernel32 -luser32 -s -Wl,--enable-stdcall-fixup
DEFF    = deflate.c deflate.def
DEFU    = LiteUnzip.c LiteUnzip.def
DEFZ    = LiteZip.c LiteZip.def
OUTDIR  = bin

# === TARGETS ===
all: deflate32 deflate64 liteunzip32 liteunzip64 litezip32 litezip64

deflate32:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) -m32 $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/deflate.32.dll $(DEFF)

deflate64:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/deflate.64.dll $(DEFF)

liteunzip32:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) -m32 $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/LiteUnzip.32.dll $(DEFU)

liteunzip64:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/LiteUnzip.64.dll $(DEFU)

litezip32:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) -m32 $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/LiteZip.32.dll $(DEFZ)

litezip64:
	@if not exist "$(OUTDIR)" mkdir "$(OUTDIR)"
	$(CC) $(CFLAGS) $(LDFLAGS) -o $(OUTDIR)/LiteZip.64.dll $(DEFZ)

clean:
	rm -rf $(OUTDIR)

