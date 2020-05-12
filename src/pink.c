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
#include "typechecker.h"
#include "symboltable.h"
#include "error.h"

char* get_input(int max_len, FILE* in_stream);
Ast* parse_buffer(char* buf, int len, yypstate* parser, yyscan_t scanner);


/*
the overall structure of this program is
mainly tree walking.

the data structure that the entire program is
built around is the Ast.

if the code needs to walk the tree and preform actions,
the general strategy for tree walking is recursive,
which means that memory is going to be used linearly
with the size of the input.

this probably isn't the best strategy for compilation
in the long term, especially as we consider having to
scale compilation to very large inputs.
but that's a bridge that should be burned later.
i mean, it's also suboptimal that we have the
tree walking tied into the structure of the
algorithms that walk the tree, instead of factoring
out the walking logic. but that is waiting until
after we can preform execution on terms in the
limited calculus currently specified.
*/

int main(int argc, char** argv)
{
	char * input = NULL;
	int chars_read;
	yy_size_t input_buf_size = 512;
	yyscan_t scanner;
	yypstate* parser;
	symboltable env;

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
		/* we add two here to garuntee that the resulting
		   buffer is always formatted in the way that flex
			 wants for the call to yyscan_buffer, namely
			 to have the input buffer be double-null-terminated.
			 even if the resulting call to getline fills the input
			 buffer completely.
	  */
		input = (char*)calloc(input_buf_size + 2, sizeof(char));

		printf(":> ");
		chars_read = getline(&input, &input_buf_size, stdin);

		if (chars_read > 0) {
			result = parse_buffer(input, input_buf_size, parser, scanner);

			if (result != NULL) {
				Ast* type = type_of(result, &env);
				Ast* val  = value_of(result, &env);
				if (type != NULL) {
					char* ast_string  = AstToString(result);
					char* type_string = AstToString(type);
					printf (":ast  %s\n", ast_string);
					printf (":type %s\n", type_string);
				}
				else {
					printf ("term not typable!\n");
				}
				AstDelete(result);
			}

			if (input != NULL)
				free (input);
		}
		else if (chars_read == EOF) {
			printf("exiting!");
			break;
		}
		else {
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

/*
	input: a double-null-terminated string containing the
				 text to be parsed, the strings length, and pointers
				 to the state objects that are already allocated for
				 the lexer and parser.

  output: the abstrax syntax tree describing the input string.
				  as parsed by the grammar described in parser.y
 */
Ast* parse_buffer(char* buf, int len, yypstate* parser, yyscan_t scanner)
{
	int parser_status, token;
	Ast* result   = NULL;
	YYSTYPE* lval = (YYSTYPE*)malloc(sizeof(YYSTYPE));
	YYLTYPE* lloc = (YYLTYPE*)malloc(sizeof(YYLTYPE));
	YY_BUFFER_STATE scanner_buffer_handle = yy_scan_buffer(buf, len, scanner);
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
				break;
			}
			else if (parser_status == 2) {
				fprintf(stderr, "\nParser failed due to exhausted memory\n");
				break;
			}
			else if (parser_status == YYPUSH_MORE) {
				continue;
			}
			else {
				error_abort("\nunknown parser status, aborting\n");
			}
	}

	if (scanner_buffer_handle != NULL)
		yy_delete_buffer(scanner_buffer_handle, scanner);

	free(lval);
	free(lloc);

	return result;
}


char* get_input(int max_len, FILE* in_stream)
{
	int chars_read;
	/* we add two here to garuntee that the resulting
		 buffer is always formatted in the way that flex
		 wants for the call to yyscan_buffer, namely
		 to have the input buffer be double-null-terminated.
		 even if the resulting call to getline fills the input
		 buffer completely.
	*/
	char* input = (char*)calloc(max_len + 2, sizeof(char));

	printf(":> ");
	chars_read = getline(&input, &max_len, stdin);
	
	if (chars_read < 0) {
		free(input);
		return NULL;
	} else {
		return input;
	}
}
