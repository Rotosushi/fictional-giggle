
build: pink.o

pink.o: pink.cpp lexer.o parser.o code_generation.o 
	clang++ -g -o pink pink.cpp lexer.o parser.o code_generation.o

lexer.o: lexer.h lexer.cpp
	clang++ -g -c lexer.cpp
	
parser.o: parser.h parser.cpp 
	clang++ -g -c parser.cpp 
	
code_generation.o: code_generation.h code_generation.cpp
	clang++ -g -c code_generation.cpp
