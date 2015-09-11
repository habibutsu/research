
parser.cc: parser.y
	bison -d -o parser.cc parser.y

scanner.cc: scanner.l
	flex -o scanner.cc scanner.l

clean:
	rm parser.cc scanner.cc