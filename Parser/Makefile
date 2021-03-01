# Andrew Lorber
# Compilers - Parser Make File

parser: parser.tab.o astFunctions.o lexer.yy.o lexerFunctions.o
		gcc -o parser parser.tab.o astFunctions.o ../Lexer/lexer.yy.o ../Lexer/lexerFunctions.o

parser.tab.o: parser.y
		# For debugging:
		#bison --debug -vd parser.y
		bison -vd parser.y
		gcc -c -o parser.tab.o parser.tab.c

parser.tab.h: parser.tab.o

astFunctions.o: astFunctions.h astFunctions.c
		gcc -c -o astFunctions.o astFunctions.c

lexer.yy.o: lexer.yy.c
		gcc -c -o ../Lexer/lexer.yy.o ../Lexer/lexer.yy.c

lexer.yy.c: ../Lexer/lexer.l parser.tab.h
		flex -o ../Lexer/lexer.yy.c ../Lexer/lexer.l
		# For debugging:
		#flex --debug -o ../Lexer/lexer.yy.c ../Lexer/lexer.l

lexerFunctions.o: ../Lexer/lexerFunctions.h ../Lexer/lexerFunctions.c
		gcc -c -o ../Lexer/lexerFunctions.o ../Lexer/lexerFunctions.c

clean:
		rm -f ../Lexer/lexerFunctions.o ../Lexer/lexer.yy.c ../Lexer/lexer.yy.o ../Lexer/lexer
		rm -f astFunctions.o parser.tab.h parser.tab.c parser.tab.o parser parser.output