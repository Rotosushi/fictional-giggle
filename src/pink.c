/*
 * Pink: Interpreter
 *
 * 	Simplified: Read -> Evaluate -> Print -> Loop
 *
 *	version 0.0.2
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
the overall structure of this program is
tree walking.

the data structure that the entire program is
built around is the Ast.

if the code needs to walk the tree and preform actions,
the general strategy employed for tree walking is recursive,
using switches over tagged unions to disambiguate actions
upon the nodes.
which means that memory is going to be used mostly linearly
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

hey, hey, hey, we bumped up the version number,
how about we reimplement in c++ and take advantage
of all the niceties of newer language constructs.

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

bool traced = false;

int main(int argc, char** argv)
{
	Scanner* scanner;
	Parser* parser;
	Symboltable* env;
	Ast* result;
	char* input = NULL;
	int   charsRead = 0;
	size_t   maxChars  = SCANNER_BUF_SZ;
	StrLoc lloc;

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
	scanner = createScanner(stdin);
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
			since we use buffered scanning we have the
			string associated with the input tokens directly
			available within the scanner structure.
		*/
			printf("::> ");
			charsRead = getline(&input, &maxChars, stdin);

			if (charsRead == -1) {
				if (feof(stdin) != 0) {
					break;
				}
			}

			yysetbuffer(scanner, input, charsRead);

			result = parse(parser, scanner);
			Ast* parse = CopyAst(result);
			if (result != NULL) {
				Ast* type = type_of(result, env);
				if (type != NULL) {

					result = evaluate(result, env);

					if (!result) {
						printf("term not evaluatable!\n");
					} else {
						char* type_string = NULL;
						if (is_polymorphic(type)) {
							Ast* result_type = type_of(result, env);
							type_string = AstToString(result_type);
							DeleteAst(result_type);
						} else {
							type_string = AstToString(type);
						}
						char* ast_string = AstToString(parse);
						char* eval_string = AstToString(result);
						printf (":parse %s\n", ast_string);
						printf (":type %s\n", type_string);
						printf ("==>>  %s\n", eval_string);
					}
					DeleteAst(parse);
					DeleteAst(type);
					DeleteAst(result);
				}
				else {
					printf ("term not typable!\n");
				}
			} else {
				printf ("input not parsable\n");
			}
	} /* !while(1) */

	if (parser != NULL)
		destroyParser(parser);

	if (scanner != NULL)
		destroyScanner(scanner);

	if (env != NULL)
		destroySymboltable(env);

	return 0;
}
