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
#include <stdio.h>
#include <errno.h>

#include "parser.h"
#include "lexer.h"

#include "ast.h"
#include "printer.h"

int main(int argc, char** argv)
{
	char * input = NULL;
	int parser_status, token, chars_read;
	yy_size_t input_buf_size = 512;
	YY_BUFFER_STATE scanner_buffer_handle;
	yyscan_t scanner;
	yypstate* parser;
	YYSTYPE* lval = (YYSTYPE*)malloc(sizeof(YYSTYPE));
	YYLTYPE* lloc = (YYLTYPE*)malloc(sizeof(YYLTYPE));
	Ast* result = NULL;

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
	yylex_init (&scanner);
	parser = yypstate_new();

	printf("Welcome to Pink v0.0.1!\npress ctrl+d to end a line\npress ctrl+c to end your session\n");

	while (1) {
		input = (char*)calloc(input_buf_size, sizeof(char));

		printf(":> ");
		chars_read = getline(&input, &input_buf_size, stdin);

		if (chars_read > 0) {
			scanner_buffer_handle = yy_scan_buffer(input, (chars_read + 2), scanner);
			if (scanner_buffer_handle == NULL) {
				fprintf(stderr, "yy_scan_buffer failed!");
				exit(1);
			}
			yy_switch_to_buffer(scanner_buffer_handle, scanner);

			while(1) {
					token = yylex(lval, lloc, scanner);
					parser_status = yypush_parse(parser, token, lval, lloc, &result, scanner);

					if (parser_status == 0) {
						break;
					}
					else if (parser_status == 1) {
						fprintf(stderr, "\nParser failed due to invalid input\n");
						exit(1);
					}
					else if (parser_status == 2) {
						fprintf(stderr, "\nParser failed due to exhausted memory\n");
						exit(1);
					}
					else if (parser_status == YYPUSH_MORE) {
						continue;
					}
					else {
						fprintf(stderr, "\nunknown parser status, aborting\n");
						exit(1);
					}
			}

			/*
			do {
				token = yylex(lval, lloc, scanner);
				parser_status = yypush_parse(parser, token, lval, lloc, scanner, result);
			} while (parser_status == YYPUSH_MORE);
			*/

			if (result != NULL) {
				char* ast_string = AstToString(result);
				printf ("==>> %s \n", ast_string);
				AstDelete(result);
			}
			if (scanner_buffer_handle != NULL)
				yy_delete_buffer(scanner_buffer_handle, scanner);

			if (input != NULL)
				free (input);
		} else if (chars_read == 0){
			printf("exiting!");
			break;
		} else {
			/* fgets failed */
			perror ("fgets failed");
			exit(1);
		}
	} /* !while(1) */


	if (parser != NULL)
		yypstate_delete(parser);

	if (scanner != NULL)
		yylex_destroy  (scanner);


	return 0;
}
