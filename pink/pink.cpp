

#include <string>
using std::string;
#include <exception>
using std::exception;

#include <cstdio>

#include "ast.h"
#include "parser.h"
#include "code_generation.h"

int main(int argc, char** argv)
{
	try {
	// behavior
	Parser p;
	CodeGen c;
	
	// state
	Module m;
	AsmFile output;
	
	m = p.parse_module("print \"Hello, World!\";");
	output = c.generate_asm_file(m);
	
	fputs(output.data.c_str(), stdout);
	fputs(output.bss.c_str(), stdout);
	fputs(output.text.c_str(), stdout);
	} catch (const exception& e) {
		fputs(e.what(), stderr);
	}
	return 0; 
}


//#include <iostream>
//using std::cout;
//using std::endl;
//#include "pink_lexer.h"
//#include "pink_parser.h"*/
//#include "utilities.h"
////#include "testing.h"
//#include "parser.h"
//int a = 8, b = 16, c = 32;
//int sum = CalcSum(a, b, c);
//const char* hello_world = "hello, world!";
//const int BUFLEN = 32;
//char buffer[BUFLEN] = {'\0'};

////printf("a %d\nb %d\n c %d\n sum %d\n", a, b, c, sum);

////a = 42, b = 7;
////int product, quotient, remainder, error;

////error = IntMul(a, b, &product);

////error = IntDiv(b, a, &quotient, &remainder);

////printf("a %d\nb %d\nproduct %d\n quotient %d\nremainder %d\n", a, b, product, quotient, remainder);

////WriteChar('p');

//////fgets(buffer, BUFLEN, stdin);
////
////error = IntMul(a, b, &product);

////WriteStr(buffer);

//// read a string into a buffer
//ReadStr(buffer, BUFLEN);

//WriteStr(buffer);
/*cout << "Hello, Pink!\n";
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
}*/

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
	the language can also autogenerate the overloaded ':=' operation,
	plain refrence '*' would shallow copy, owned refrence(pointer) '!*'
	would deep copy

	that would go a long way to solving this problem.
	because, many of the breaks are in comparing the wrong member names
	or members who have moved or changed type. a lot of these changes
	could be handled under the hood if we generate the comparison
	ops for user types.
	which would improve programmers experience in the language
	by reducing knock-on-viscosity in the test-suite, a major
	cross cutting concern. it might also reduce maintinence costs
	elswhere in the program while refactoring.

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
