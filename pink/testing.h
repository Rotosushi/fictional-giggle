#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;

typedef struct _test_data {
	string name;
	bool passed;
	string failure_message;
} _test_data;

typedef struct _test_results {
	vector<_test_data> tests;
} _test_results;

void test_lexer(_test_results& td);
void test_parser(_test_results& td);