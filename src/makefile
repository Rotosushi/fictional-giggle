

pink: lexer
	clang -o pink pink.c parser.c lexer.c ast.c symboltable.c precedencetable.c error.c typechecker.c  evaluator.c -g

lexer:
	re2c -W --storable-state lexer.re -o lexer.c

clean:
	rm *.o parser.c parser.h lexer.c lexer.h pink

debug: pink
	gdb -tui pink
