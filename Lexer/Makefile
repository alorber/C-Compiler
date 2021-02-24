# Andrew Lorber
# Compilers - Lexical Analyzer Make File

lexer: lexer.yy.o lexerFunctions.o
		gcc -o lexer lexer.yy.o lexerFunctions.o

lexer.yy.o: lexer.yy.c
		gcc -c -o lexer.yy.o lexer.yy.c

lexer.yy.c: lexer.l
		flex -o lexer.yy.c lexer.l

lexerFunctions.o: lexerFunctions.h lexerFunctions.c
		gcc -c -o lexerFunctions.o lexerFunctions.c

clean:
		rm -f lexerFunctions.o lexer.yy.c lexer.yy.o lexer
