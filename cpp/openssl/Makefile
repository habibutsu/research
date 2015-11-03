
#
# http://joelinoff.com/blog/?p=885

LIBS = -lcrypto -lssl

all:
	g++ ${LIBS} -o main main.cc

clean:
	rm main

openssl:
	openssl aes-128-cfb -e -in in.txt -out out.txt -K 31323334353637383930313233343536 -iv 31323334353637383930313233343536