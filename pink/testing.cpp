#include <string>
using std::string;
#include <vector>
using std::vector;

#include "testing.h"
#include "ast.h"
#include "type.h"
#include "lexer.h"
#include "parser.h"

_parser parser;

bool assert_equivalent_types(_ast* expected, _ast* parsed) 
{}

bool assert_equivalent_postop(_ast* expected, _ast* parsed)
{

}

bool assert_equivalent_expression(_ast* expected, _ast* parsed)
{

}

bool assert_equivalence(_binop& expected, _binop& parsed)
{

}

bool assert_equivalence(_unop& expected, _unop& parsed)
{

}

bool asser_equivalence(_return& expected, _return& parsed)
{}

bool assert_equivalence(_fcall& expected, _fcall& parsed)
{}

bool assert_equivalence(_named_member& expected, _named_member& parsed)
{

}

bool assert_equivalence(_type_specifier& expected, _type_specifier& parsed)
{

}

bool assert_equivalence(_var& expected, _var& parsed)
{}

bool assert_equivalence(_vardecl& expected, _vardecl& parsed)
{

}

bool assert_equivalence(_arg& expected, _arg& parsed)
{}

bool assert_equivalence(_fndecl& expected, _fndecl& parsed)
{}

bool assert_equivalence(_member& expected, _member& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec))
}

bool assert_equivalence(_struct& expected, _struct& parsed)
{
	if (expected.id != parsed.id) return false;
	if (expected.members.size() != parsed.members.size()) return false;
	for (int i = 0; i < expected.members.size(); ++i) {
		if (!assert_equivalence(expected.members[i], parsed.members[i])) return false;
	}
}

bool assert_equivalence(_tuple& expected, _tuple& parsed)
{
	if (expected.members.size() != parsed.members.size()) return false;
	for (int i = 0; i < expected.members.size(); ++i) {
		if (!assert_equivalence(expected.members[i], parsed.members[i])) return false;
	}
}

bool assert_equivalence(_array& expected, _array& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec, parsed.tspec)) return false;
	if (!assert_equivalent_expression(expected.length_expr, parsed.length_expr)) return false;
}

bool assert_equivalence(_if& expected, _if& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_expression(expected.then, parsed.then)) return false;
	if (!assert_equivalent_expression(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_while& expected, _while& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_expression(expected.body, parsed.body)) return false;
	if (!assert_equivalent_expression(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_dowhile& expected, _dowhile& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_expression(expected.body, parsed.body)) return false;
	if (!assert_equivalent_expression(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_scope& expected, _scope& parsed)
{
	if (expected.variables != parsed.variables) return false;
	if (expected.statements.size() != parsed.variables.size()) return false;
	
	for (int i = 0; i < expected.statements.size(); ++i) {
		if (!assert_equivalent_expression(expected.statements[i], parsed.statements[i])) return false;
	}
}

bool assert_equivalence(_module& expected, _module& parsed) {
	if (expected.id != parsed.id) return false;
	if (expected.main_fn != parsed.main_fn) return false;

	if (expected.import_list.size() != parsed.import_list.size())
		return false;
	else {
		for (int i = 0; i < expected.import_list.size(); ++i) {
			if (expected.import_list[i] != parsed.import_list[i]) return false;
		}
	}

	if (expected.export_list.size() != parsed.export_list.size())
		return false;
	else {
		for (int i = 0; i < expected.export_list.size(); ++i) {
			if (expected.export_list[i] != parsed.export_list[i]) return false;
		}
	}

	if (expected.types.size() != parsed.types.size())
		return false;
	else {
		for (int i = 0; i < expected.types.size(); ++i) {
			if (!assert_equivalent_types(expected.types[i], parsed.types[i])) return false;
		}
	}

	if (!assert_equivalence(expected.body, parsed.body)) return false;
	return true;
}


_test_data* test_module_declaration_empty()
{
	bool success = true;
	_module expected;
	_module* parsed = parser.parse_module("module :: { }");

	success = assert_equivalence(expected, *parsed);
	
}

void test_lexer(_test_results& td)
{
	
}

void test_parser(_test_results& td)
{
}
