

Pink: Lexer
	clear
	clang++ -I/usr/include/llvm-10 -I/usr/include/llvm-c-10 `llvm-config-10 --cxxflags --ldflags --libs` -fexceptions -fstandalone-debug -g --std=c++17 --stdlib=libstdc++ Ast.cc SymbolTable.cc OperatorTable.cc Kernel.cc Lexer.cc Parser.cc Pink.cc -o Pink

Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cc

clean:
	rm *.o Pink core Lexer.cc

debug: Pink
	gdb -tui Pink

grind: Pink
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./Pink
