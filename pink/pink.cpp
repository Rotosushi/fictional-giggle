// pink.cpp : This file contains the 'main' function. Program execution begins and ends there.

#include <iostream>
using std::cout;
#include <string>
using std::string;
#include "pink_lexer.h"
#include "pink_parser.h"
#include "pink_utilities.h"

old_parser par;

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

	_module* module;
	if (infile.size() > 0)
		module = par.parse_module(infile);
	else
		module = par.parse_module();

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

	// later -> optimize code

	// assemble code

	// output assembly

	return 0;
}

