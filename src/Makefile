
Pink: Lexer
	clear
	clang++ -fstandalone-debug -fexceptions -g -std=c++17 \
	--stdlib=libstdc++ Ast.cpp Lexer.cpp Parser.cpp Type.cpp \
	PinkError.cpp ParserError.cpp TypeError.cpp EvalError.cpp  \
	ParserJudgement.cpp TypeJudgement.cpp EvalJudgement.cpp \
	Gensym.cpp StringInterner.cpp REPL.cpp SymbolTable.cpp   \
	Object.cpp TypeLiteral.cpp Nil.cpp Integer.cpp Boolean.cpp \
	Lambda.cpp PolyLambda.cpp Binop.cpp Unop.cpp PinkException.cpp \
	BinopPrecedenceTable.cpp BinopEliminators.cpp \
	UnopEliminators.cpp Environment.cpp PinkKernel.cpp \
	Location.cpp Variable.cpp Sequence.cpp Assignment.cpp \
	Bind.cpp Application.cpp Conditional.cpp Iteration.cpp \
	Reference.cpp Entity.cpp Empty.cpp Pink.cpp -o Pink



Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cpp

clean:
	rm *.o Pink core Lexer.cpp

debug: Pink
	gdb -tui Pink

grind: Pink
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./Pink
