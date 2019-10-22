#pragma once
#include <string>
using std::string;
#include <vector>
using std::vector;

#include "testing.h"
#include "error.h"
#include "ast.h"
#include "type.h"
#include "lexer.h"
#include "parser.h"

typedef struct _test_data {
	string name;
	bool passed;
} _test_data;

typedef struct _test_results {
	vector<_test_data> tests;

	auto begin() { return tests.begin(); }
	auto end() { return tests.end(); }
} _test_results;

void test_lexer(_test_results& td);
void test_parser(_test_results& td);

bool assert_equivalent_type_definition(_ast* expected, _ast* parsed);
bool assert_equivalent_postop(_ast* expected, _ast* parsed);
bool assert_equivalent_expression(_ast* expected, _ast* parsed);
bool assert_equivalent_initializer(_ast* expected, _ast* parsed);
bool assert_equivalent_statement(_ast* expected, _ast* parsed);
bool assert_equivalence(_int& expected, _int& parsed);
bool assert_equivalence(_float& expected, _float& parsed);
bool assert_equivalence(_text& expected, _text& parsed);
bool assert_equivalence(_bool& expected, _bool& parsed);
bool assert_equivalence(_binop& expected, _binop& parsed);
bool assert_equivalence(_unop& expected, _unop& parsed);
bool assert_equivalence(_return& expected, _return& parsed);
bool assert_equivalence(_fcall& expected, _fcall& parsed);
bool assert_equivalence(_named_member_access& expected, _named_member_access& parsed);
bool assert_equivalence(_positional_member_access& expected, _positional_member_access& parsed);
bool assert_equivalence(_type_specifier& expected, _type_specifier& parsed);
bool assert_equivalence(_var& expected, _var& parsed);
bool assert_equivalence(_vardecl& expected, _vardecl& parsed);
bool assert_equivalence(_arg& expected, _arg& parsed);
bool assert_equivalence(_lambda& expected, _lambda& parsed);
bool assert_equivalence(_fndecl& expected, _fndecl& parsed);
bool assert_equivalence(_member& expected, _member& parsed);
bool assert_equivalence(_record& expected, _record& parsed);
bool assert_equivalence(_tuple& expected, _tuple& parsed);
bool assert_equivalence(_array& expected, _array& parsed);
bool assert_equivalence(_if& expected, _if& parsed);
bool assert_equivalence(_while& expected, _while& parsed);
bool assert_equivalence(_dowhile& expected, _dowhile& parsed);
bool assert_equivalence(_scope& expected, _scope& parsed);
bool assert_equivalence(_module& expected, _module& parsed);