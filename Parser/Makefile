# Andrew Lorber
# Compilers - Parser Make File

parser: parser.tab.o astFunctions.o lexer.yy.o lexerFunctions.o
		gcc -o parser parser.tab.o astFunctions.o lexer.yy.o lexerFunctions.o

parser.tab.o: parser.y
		bison -vd parser.y
		gcc -c -o parser.tab.o parser.tab.c

parser.tab.h: parser.tab.o

astFunctions.o: astFunctions.h astFunctions.c
		gcc -c -o astFunctions.o astFunctions.c

lexer.yy.o: lexer.yy.c
		gcc -c -o lexer.yy.o lexer.yy.c

lexer.yy.c: lexer.l parser.tab.h
		flex -o lexer.yy.c lexer.l

lexerFunctions.o: lexerFunctions.h lexerFunctions.c
		gcc -c -o lexerFunctions.o lexerFunctions.c

clean:
		rm -f ../Lexer/lexerFunctions.o ../Lexer/lexer.yy.c ../Lexer/lexer.yy.o ../Lexer/lexer
		rm -f astFunctions.o parser.tab.h parser.tab.c parser.tab.o parser parser.output