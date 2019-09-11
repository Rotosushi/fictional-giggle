// pink.cpp : This file contains the 'main' function. Program execution begins and ends there.

#include <iostream>
using std::cout;
#include <string>
using std::string;
#include "pink_lexer.h"
#include "pink_parser.h"
#include "pink_utilities.h"

void test_parser() {
	parse_module();
}

void print_manual() {
	cout << "Usage:\n\t '--help' prints this message\n"
		 << "\t --input <filename> uses <filename> as the input" << std::endl;
}

int main(int argc, char** argv)
{
    cout << "Hello, Pink!\n";
	char* opt = nullptr;
	char options[] = "input help";
	while ((opt = getopt(argc, argv, options)) != nullptr) {
		if (opt == "input") {

		}
		else if (opt == "help") {
			print_manual();
			exit(0);
		}
	}

	test_parser();

	return 0;
}

