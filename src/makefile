

Pink: Lexer
	clear
	clang++ -fstandalone-debug -g --std=c++17 --stdlib=libstdc++ Ast.cc SymbolTable.cc OperatorTable.cc Kernel.cc Lexer.cc Parser.cc Pink.cc -o Pink

Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cc

clean:
	rm *.o Pink core Lexer.cc

debug: Pink
	gdb -tui Pink

grind: Pink
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./Pink
