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
 *	Programmer: Cade Weinberg
 *
 */
#include <stdio.h>
#include <errno.h>

#include "parser.h"
#include "lexer.h"
#include "ast.h"
#include "typechecker.h"
#include "symboltable.h"
#include "evaluator.h"
#include "error.h"

/*
	if we are operating in an interactive capacity,
	I think get_input is a viable candidate for
	yywrap...
 */
char* get_input(int max_len, int* chars_read, FILE* in_stream);

Ast* parse_buffer(char* buf, int len, Parser* parser, yyscan_t scanner);


/*
the overall structure of this program is
mainly tree walking.

the data structure that the entire program is
built around is the Ast.

if the code needs to walk the tree and preform actions,
the general strategy employed for tree walking is recursive,
which means that memory is going to be used linearly
with the size of the input, specifically relative to the depth
of the tree. memory has to be used linearly
to represent the input as a tree, then each task that
needs to walk the tree, if it walks it recursively, will
always use memory equal to or less than some linear scale
of the depth of the tree.

this probably isn't the best strategy for compilation
in the long term, especially as we consider having to
scale compilation to very large inputs.
but that's a bridge that should be burned later.
i mean, it's also suboptimal that we have the
tree walking tied into the structure of the
algorithms that walk the tree, instead of factoring
out the walking logic. but that is waiting until
after we can preform execution on terms in the
limited calculus currently specified. i.e. getting
something that gives feedback correctly is first priority
then make it nice, then upgrade it.

in the split walk/action pattern the
walk algorithm takes as a param a fn ptr to
the action to call, this action fn takes a void*
which we use to pass the node being acted on currently,
and then the action can do it's work.
which action being called is dependant on the type of
the node being looked at, and the node is passed
by refrence because that is the cheapest for the
memory footprint of the function, and passing tree
or list structures by-value has bitten me too many times.
within the body of the action code includes a
typecast from the void* to the Ast* type,
and then it can do work with the Ast node.
actions include:
	-> tree construction itself
	-> tree deletion
	-> tree copying
	-> printing the tree
	-> typechecking

a refactor to improve the preformance of the program,
by reducing the memory usage of algorithms working
on the Ast to O(1) instead of O(n), which I think
can be done by walking in an iterative style.


*/

int main(int argc, char** argv)
{
	char * input = NULL;
	yy_size_t input_buf_size = 512;
	yyscan_t scanner;
	Parser* parser;
	Symboltable* env;
	Ast* result;
	int chars_read = 0;

	// enable/disable parser trace printing
	//yydebug = 1;

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
	parser = createParser();
	env = createSymboltable();

	printf("Welcome to Pink v0.0.1!\npress ctrl+d to end a line\npress ctrl+c to end your session\n");

	while (1) {
		/*
			nice error messages are possible!
			we use the fact that we are getting
			input a line at a time, and we refactor
			the Ast to store the location data of each
			token/token-range. this should meet the
			data gathering requirements, when an
			error is spotted, we need to be able to
			call an error reporting routine,
			the exact location of the error is only known
			the moment we discover the error.
			the input text isn't generally usefull for
			the evaluator or typechecker to know
			until we need to call the error reporting
			routine. but we need to be able to put together
			the location of the erroneous token and
		  the full input string, then we can
			underscore the text of the erroneous token.
		*/
		input = get_input(input_buf_size, &chars_read, stdin);

		if (input != NULL) {
			result = parse_buffer(input, input_buf_size, parser, scanner);

			if (result != NULL) {
				Ast* type = type_of(result, env);
				if (type != NULL) {
					Ast* copy = CopyAst(result);
					copy = evaluate(copy, env);

					if (!copy) {
						printf("term not evaluatable!\n");
					} else {
						//char* ast_string = AstToString(result);
						char* type_string = AstToString(type);
						char* eval_string = AstToString(copy);
						printf (":type %s\n", type_string);
						printf ("==>>  %s\n", eval_string);
					}
				}
				else {
					printf ("term not typable!\n");
				}
				DeleteAst(result);
			}

			if (input != NULL)
				free (input);
		}
		else {
			perror("getline ");
			exit(1);
		}
	} /* !while(1) */


	if (parser != NULL)
		destroyParser(parser);

	if (scanner != NULL)
		yylex_destroy(scanner);

	if (env != NULL)
		destroySymboltable(env);

	return 0;
}

/*
	input: a double-null-terminated string containing the
				 text to be parsed, the strings length, and pointers
				 to the state objects that are already allocated for
				 the lexer and parser.

  output: the abstax syntax tree describing the input string.
				  as parsed by the grammar described in parser.y
 */
Ast* parse_buffer(char* buf, int len, Parser* parser, yyscan_t scanner)
{
	Ast* result = NULL;
	StrLoc* lloc = (StrLoc*)malloc(sizeof(StrLoc));

	YY_BUFFER_STATE scanner_buffer_handle = yy_scan_buffer(buf, len, scanner);
	if (scanner_buffer_handle == NULL) {
		fprintf(stderr, "yy_scan_buffer failed!");
		exit(1);
	}
	yy_switch_to_buffer(scanner_buffer_handle, scanner);

	result = parse(parser, scanner, lloc);

	if (scanner_buffer_handle != NULL)
		yy_delete_buffer(scanner_buffer_handle, scanner);

	free(lloc);

	return result;
}


char* get_input(int max_len, int* chars_read, FILE* in_stream)
{
	int num_chars;
	/* we add two here to garuntee that the resulting
		 buffer is always formatted in the way that flex
		 wants for the call to yyscan_buffer, namely
		 to have the input buffer be double-null-terminated.
		 even if the resulting call to getline fills the input
		 buffer completely. if the buffer given to
		 yyscan_buffer is not formatted in this way, then
		 the function fails.
	*/
	char* input = (char*)calloc(max_len + 2, sizeof(char));

	printf(":> ");
	num_chars = getline(&input, (size_t*)&max_len, stdin);

	if (num_chars < 0) {
		free(input);
		*chars_read = 0;
		return NULL;
	} else {
		*chars_read = num_chars;
		return input;
	}
}
