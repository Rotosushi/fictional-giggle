
Pink: Lexer
	clear
	clang++ -fstandalone-debug -g -std=c++17 \
	--stdlib=libstdc++ Lexer.cpp Parser.cpp Error.hpp \
	TypeError.hpp EvalError.hpp Type.hpp TypeJudgement.hpp \
	EvalJudgement.hpp SymbolTable.cpp Location.hpp Variable.cpp \
	Bind.cpp Application.cpp Conditional.cpp Iteration.cpp \
	Entity.cpp 


Lexer:
	re2c -W --input custom Lexer.re -o Lexer.cpp

clean:
	rm *.o Pink core Lexer.cpp

debug: Pink
	gdb -tui Pink

grind:
	valgrind -v --leak-check=full --show-leak-kinds=all --track-origins=yes ./Pink
