
ATSCC = patscc
ATSOPT = patsopt

# '-flto' enables link-time optimization such as inlining lib functions
ATSCCFLAGS=-O2 -flto

all: main

main:
	$(ATSCC) $(ATSCCFLAGS) -o $@ main.dats

clean:
	rm main; rm -f *_?ats.o; rm -f *_?ats.c
