
Pink: Lexer
	clear
	clang++ -fstandalone-debug -g -std=c++17 \
	--stdlib=libstdc++ Ast.cpp Lexer.cpp Parser.cpp Type.cpp \
	PinkError.cpp ParserError.cpp TypeError.cpp EvalError.cpp  \
	ParserJudgement.cpp TypeJudgement.cpp EvalJudgement.cpp \
	Binop.cpp Unop.cpp SymbolTable.cpp \
	BinopPrecedenceTable.cpp BinopEliminators.cpp \
	UnopEliminators.cpp Environment.cpp PinkKernel.cpp \
	Location.cpp Variable.cpp \
	Bind.cpp Application.cpp Conditional.cpp Iteration.cpp \
	Entity.cpp Pink.cpp -o Pink


Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cpp

clean:
	rm *.o Pink core Lexer.cpp

debug: Pink
	gdb -tui Pink

grind:
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./Pink
