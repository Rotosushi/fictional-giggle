

pink: lexer
	clang -o pink pink.c parser.c lexer.c ast.c symboltable.c precedencetable.c stringset.c error.c typechecker.c evaluator.c pink_kernel.c -g

lexer:
	re2c -W --input custom lexer.re -o lexer.c

clean:
	rm *.o parser.c parser.h lexer.c lexer.h pink

debug: pink
	gdb -tui pink

grind: pink
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./pink
