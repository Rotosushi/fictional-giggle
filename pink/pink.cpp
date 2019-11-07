// pink.cpp : This file contains the 'main' function. Program execution begins and ends there.

#include <iostream>
using std::cout;
using std::endl;
#include <string>
using std::string;
/*#include "pink_lexer.h"
#include "pink_parser.h"*/
#include "utilities.h"
//#include "testing.h"
#include "parser.h"

_parser par;

void print_manual() {
	cout << "Usage:\n\t '--help' prints this message\n"
		 << "\t --input <filename> uses <filename> as the input" << std::endl;
}

int main(int argc, char** argv)
{
    cout << "Hello, Pink!\n";
	char* opt = nullptr;
	string infile;
	char options[] = "input help";
	while ((opt = getopt(argc, argv, options)) != nullptr) {
		if (opt == "input") {
			infile.clear();
			infile.append(optarg);
		}
		else if (opt == "help") {
			print_manual();
			exit(0);
		}
		free(opt);
	}

	//_test_results parser_results;

	/*
	10/31/2019: the test suite is horribly broken
		after modifications to the underlying data-structures,
		namely the AST

		this feels like an instance of both cross-cutting concerns
		and knock-on viscosity.

		a test suite is a basic thing that literally every single
		project* should have. (*larger than a school project, project)

		we could further say that that it is of such importance,
		that your tools should automate some of the work for you.

		if we could automate the creation of overloaded '=='
		and '!=' (plain refrence '*' would compare address, owned refrence(pointer) '!*'
		would deep compare) operations on plain old data (in my ideal world
		the language can also autogenerate the overloaded '=' operation,
		plain refrence '*' would shallow copy, owned refrence(pointer) '!*'
		would deep copy (maybe its an optimization that we can just
		pass the pointer in some instances?))
		that would go a long way to solving this problem.
		which would improve programmers experience in the language
		by reducing knock-on-viscosity in the test-suite, a major
		cross cutting concern.

	try {
		test_parser(parser_results);
	}
	catch (_parser_error err) {
		cout << err.what() << endl;
		exit(1);
	}

	for (auto test : parser_results) {
		cout << "Test: "   << test.name
			 << "\nPassed: " << (test.passed == true ? "true" : "false") << endl;
	}*/

	/*
	_module* module;
	if (infile.size() > 0)
		module = par.parse_module(infile);
	else
		module = par.parse_module();
	*/
	// TODO:
	/* 
	/ semantic analysis:
	  / scope resolution

	  / array bounds checking

	  / typecheck program:
		typecheck declarations of primitives
		typecheck expressions of primitives
		typecheck functions of primitives
		typecheck if statements
		typecheck do/while loops
		types: int, float, string, bool

	  / semantic errors:
		type mismatch
		undeclared variable
		reserved identifier misuse
		multiple declaration 
		out of scope access
		actual parameter mismatch
	*/

	// future -> optimize code

	// output assembly

	// assemble to binary
	
	return 0; // funny thought; this return 0 will probably be untouched all the way into the final version...
}

