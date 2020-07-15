

Pink: Lexer
	clang++ -g --std=c++17 Ast.cc SymbolTable.cc OperatorTable.cc Kernel.cc Lexer.cc Parser.cc Pink.cc -o Pink

Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cc
