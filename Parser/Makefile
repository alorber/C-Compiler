# Andrew Lorber
# Compilers - Parser Make File

parser: parser.tab.o quads.o symbol_table.o astFunctions.o lexer.yy.o lexerFunctions.o
		gcc --std=gnu99 -o parser parser.tab.o quads.o symbol_table.o astFunctions.o ../Lexer/lexer.yy.o ../Lexer/lexerFunctions.o

parser.tab.o: parser.y
		# For debugging:
		# bison --debug -vd parser.y
		bison -vd parser.y
		gcc --std=gnu99 -c -o parser.tab.o parser.tab.c

parser.tab.h: parser.tab.o

quads.o: astFunctions.o quads.h quads.c
		gcc --std=gnu99 -c -o quads.o quads.c

symbol_table.o: astFunctions.o symbol_table.h symbol_table.c 
		gcc --std=gnu99 -c -o symbol_table.o symbol_table.c

astFunctions.o: astFunctions.h astFunctions.c
		gcc -std=gnu99 -c -o astFunctions.o astFunctions.c

lexer.yy.o: lexer.yy.c
		gcc --std=gnu99 -c -o ../Lexer/lexer.yy.o ../Lexer/lexer.yy.c

lexer.yy.c: ../Lexer/lexer.l parser.tab.h
		flex -o ../Lexer/lexer.yy.c ../Lexer/lexer.l
		# For debugging:
		# flex --debug -o ../Lexer/lexer.yy.c ../Lexer/lexer.l

lexerFunctions.o: ../Lexer/lexerFunctions.h ../Lexer/lexerFunctions.c
		gcc --std=gnu99 -c -o ../Lexer/lexerFunctions.o ../Lexer/lexerFunctions.c

clean:
		rm -f ../Lexer/lexerFunctions.o ../Lexer/lexer.yy.c ../Lexer/lexer.yy.o ../Lexer/lexer
		rm -f quads.o symbol_table.o astFunctions.o parser.tab.h parser.tab.c parser.tab.o parser parser.output