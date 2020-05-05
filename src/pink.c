/*
 * Pink: Interpreter
 *
 * 	Simplified: Read -> Evaluate -> Print -> Loop
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

#include "parser.h"
#include "lexer.h"


int main(int argc, char** argv)
{
	int parser_status, token;
	yyscan_t scanner;
	yypstate* parser;
	YYSTYPE* lval = (YYSTYPE*)malloc(sizeof(YYSTYPE));
	YYLTYPE* lloc = (YYLTYPE*)malloc(sizeof(YYLTYPE));

	yydebug = 1;

	/* in order to support reentrancy
	     the parser and lexer internal state
	     must become parameters, since
	     c has no way of specifying construction
	     of types, we use the provided helper functions
	     which act as constructors.
	     this localizes the state which controls the lexer
	     and parser into these few local variables.
	     which makes maintinence and extension straightforward.
	     this also vastly simplifies adding multithreaded
	     lexing and parsing.
	*/


	printf("Welcome to Pink v0.0.1!\n \
				  press ctrl+d to end a line\n \
					press ctrl+c to end your session\n");

	while (1) {
		yylex_init (&scanner);
		parser = yypstate_new();

		printf(":> ");

		// the minimal parser control loop;
		// eat tokens until we reach some valid state at EOF
		do {
			token = yylex(lval, lloc, scanner);
			parser_status = yypush_parse(parser, token, lval, lloc, scanner);
		} while (parser_status == YYPUSH_MORE);

		PrintAst(lval->ast);

		AstDelete(lval->ast);

		yypstate_delete(parser);
		yylex_destroy  (scanner);
	}

	return 0;
}
