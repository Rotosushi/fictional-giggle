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

 given how many ideas this programming language simply
 lifts wholly from other languages and from theory,
 i feel slightly unethical about saying I
 did anything other than arrange other human beings ideas.
 so the licensing of this program is going to be open source.
 because to claim otherwise is cheating humans after I die
 of openly viewing my ideas behing arbitrary paywalls. when
 in reality, this is simply one particular arrangement of
 some fundamental ideas. not unlike any other language.

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
so, i feel like all the new features aren't worth
the extra headache of the pervasive massive amount
of syntax for everything, which makes using the
limited form of polymorphism that c++ supports,
more like arranging the procudure i want to write
in just the right way in order to make the polymorphism
usefull. which takes away the simplicity of the idea,
which is having a function with the same name acting
as different instances depending upon the compiletime or
runtime type of the entity being passed.

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


breifly:
so, the first stage is a reentrant re2c scanner.

the second stage is a backtracking parser which is
also reentrant.

both of these modules are called via the parse command/procedure
which simply takes a string and returns an Ast representing the
string.

we then choose a tree style evaluation strategy from a simplicity of
implementation perspective. (the other option I see is to generate
LLVM bytecode here, and in my mind, that is how pink generates standalone
executables.)

so, given our evaluation strategy, both typechecking and evaluation
are simply sets of mutually recursive procedures, which supply
a typechecking/evaluation strategy per node kind in the Ast,
and returning either the resulting type tree or result Ast.
these procedures are tied together via the dispatch procedure
which dispatches over which particular Ast node was passed.
because we cannot know that peice of information until runtime,
the dispatcher needs to dispatch over some dynamic memory cell.
each procedure returns either a partial result or the final result,
with entities being final results, and sinew/connective-language-entities
being partial results. entities correspond to beta-normal forms from
the theoretical perspective, and to memory cells from the implementation
perspective. this is the case precisely because we choose the
eager/strict evaluation strategy.

*/

// this variable is exploiting an often griped about
// feature of C, the non-local state change.
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
			routine. we need to be able to put together
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
						printf (":type  %s\n", type_string);
						printf ("==>>   %s\n", eval_string);
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
