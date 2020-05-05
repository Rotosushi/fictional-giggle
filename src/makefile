

pink: parser lexer
	clang -o pink pink.c parser.c lexer.c ast.c printer.c -g 

parser:
	bison -o parser.c parser.y

lexer:
	flex lexer.l

clean:
	rm *.o parser.c parser.h lexer.c lexer.h pink
