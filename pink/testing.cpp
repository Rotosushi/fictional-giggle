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

_parser parser;

/*
	in my ideal language, '=' and '==' operators can be generated
	automatically for product and sum types. operator overloading could be used
	to allow users to override the default and specify other operators.
	like in c++ saying <type-name> operator<op-symbol>(<type-name> lhs, <type-name> rhs)
	will be used to add the type to the overload set. 
*/

bool assert_equivalent_type_definition(_ast* expected, _ast* parsed) 
{
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_FN: {
		auto e = (_fn*)expected;
		auto p = (_fn*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_RECORD: {
		auto e = (_record*)expected;
		auto p = (_record*)parsed;
		return assert_equivalence(*e, *p);
	}
	default: throw _parser_error(__FILE__, __LINE__, "internal error: bad type definition type", ast_type_to_string(expected->ast_type));
	}
}

bool assert_equivalent_postop(_ast* expected, _ast* parsed)
{
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_FCALL: {
		_fcall* e = (_fcall*)expected;
		_fcall* p = (_fcall*)parsed;

		if (e->argument_list.size() != p->argument_list.size())
			return false;
		else for (unsigned int i = 0; i < e->argument_list.size(); i++)
			if (!assert_equivalence(e->argument_list[i], p->argument_list[i]))
				return false;

		if (e->return_list.size() != p->return_list.size())
			return false;
		else for (unsigned int i = 0; i < e->return_list.size(); i++)
			if (!assert_equivalence(e->return_list[i], p->return_list[i]))
				return false;
		return true;
	}
	case AST_NAMED_MEMBER: {
		_named_member_access* e = (_named_member_access*)expected;
		_named_member_access* p = (_named_member_access*)parsed;

		if (e->member_id != p->member_id) 
			return false;
		else return true;
	}
	case AST_POSITIONAL_MEMBER: {
		_positional_member_access* e = (_positional_member_access*)expected;
		_positional_member_access* p = (_positional_member_access*)parsed;

		if (!assert_equivalent_expression(e->offset_expression, p->offset_expression))
			return false;
		else return true;
	}
	default: throw _parser_error(__FILE__, __LINE__, "internal error: bad postop type", ast_type_to_string(expected->ast_type));
	}
}

bool assert_equivalent_tspec_expression(_ast* expected, _ast* parsed) {
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_LAMBDA: {
		auto e = (_lambda*)expected;
		auto p = (_lambda*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_RECORD: {
		auto e = (_record*)expected;
		auto p = (_record*)parsed;
		return assert_equivalence(*e, *p);
	}
	default: throw _parser_error(__FILE__, __LINE__, "internal error: bad _ast_type in tspec expr: ", ast_type_to_string(expected->ast_type));
	}
}

bool assert_equivalent_expression(_ast* expected, _ast* parsed)
{
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_BINOP: {
		auto e = (_binop*)expected;
		auto p = (_binop*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_UNOP: {
		auto e = (_unop*)expected;
		auto p = (_unop*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_RETURN: {
		auto e = (_return*)expected;
		auto p = (_return*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_VAR: {
		auto e = (_var*)expected;
		auto p = (_var*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_FCALL: {
		auto e = (_fcall*)expected;
		auto p = (_fcall*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_NAMED_MEMBER: {
		auto e = (_named_member_access*)expected;
		auto p = (_named_member_access*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_POSITIONAL_MEMBER: {
		auto e = (_positional_member_access*)expected;
		auto p = (_positional_member_access*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_TUPLE: {
		auto e = (_tuple*)expected;
		auto p = (_tuple*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_INT: {
		auto e = (_int*)expected;
		auto p = (_int*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_FLOAT: {
		auto e = (_float*)expected;
		auto p = (_float*)parsed;
		return assert_equivalence(*e, *p);
	} 
	case AST_TEXT: {
		auto e = (_text*)expected;
		auto p = (_text*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_BOOL: {
		auto e = (_bool*)expected;
		auto p = (_bool*)parsed;
		return assert_equivalence(*e, *p);
	}
	default: throw _parser_error(__FILE__, __LINE__, "internal error: bad _ast_type in expression", ast_type_to_string(expected->ast_type));
	}
}

bool assert_equivalent_initializer(_ast* expected, _ast* parsed)
{
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_LAMBDA: {
		auto e = (_lambda*)expected;
		auto p = (_lambda*)parsed;
		return assert_equivalence(*e, *p);
	}
	default: {
		return assert_equivalent_expression(expected, parsed);
	}
	}
}

bool assert_equivalent_statement(_ast* expected, _ast* parsed)
{
	if (expected == nullptr && parsed == nullptr)
		return true;
	else if (expected == nullptr || parsed == nullptr)
		return false;

	if (expected->ast_type != parsed->ast_type) return false;
	switch (expected->ast_type) {
	case AST_IF: {
		auto e = (_if*)expected;
		auto p = (_if*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_WHILE: {
		auto e = (_while*)expected;
		auto p = (_while*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_DOWHILE: {
		auto e = (_dowhile*)expected;
		auto p = (_dowhile*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_SCOPE: {
		auto e = (_scope*)expected;
		auto p = (_scope*)parsed;
		return assert_equivalence(*e, *p);
	}
	case AST_RETURN: {
		auto e = (_return*)expected;
		auto p = (_return*)parsed;
		return assert_equivalence(*e, *p);
	}
	default: {
		return assert_equivalent_expression(expected, parsed);
	}
	}
}

bool assert_equivalence(_int& expected, _int& parsed)
{
	if (expected.value != parsed.value) return false;
	return true;
}

bool assert_equivalence(_float& expected, _float& parsed)
{
	if (expected.value != parsed.value) return false;
	return true;
}

bool assert_equivalence(_text& expected, _text& parsed)
{
	// below statement doesn't pass??? the behavior should be equivalent right???
	//if (expected.value != parsed.value) return false;
	bool success = strcmp(expected.value.c_str(), parsed.value.c_str()) == 0 ? true : false;
	return success;
}

bool assert_equivalence(_bool& expected, _bool& parsed)
{
	if (expected.value != parsed.value) return false;
	return true;
}

bool assert_equivalence(_binop& expected, _binop& parsed)
{
	if (expected.op != parsed.op) return false;
	if (!assert_equivalent_expression(expected.lhs, parsed.lhs)) return false;
	if (!assert_equivalent_expression(expected.rhs, parsed.rhs)) return false;
	return true;
}

bool assert_equivalence(_unop& expected, _unop& parsed)
{
	if (expected.op != parsed.op) return false;
	if (!assert_equivalent_expression(expected.rhs, parsed.rhs)) return false;
	return true;
}

bool assert_equivalence(_return& expected, _return& parsed)
{
	if (!assert_equivalent_expression(expected.rhs, parsed.rhs)) return false;
	return true;
}

bool assert_equivalence(_fcall& expected, _fcall& parsed)
{
	if (expected.argument_list.size() != parsed.argument_list.size())
		return false;
	else for (unsigned int i = 0; i < expected.argument_list.size(); ++i)
		if (!assert_equivalence(expected.argument_list[i], parsed.argument_list[i])) return false;
	return true;
}

bool assert_equivalence(_named_member_access& expected, _named_member_access& parsed)
{
	if (expected.member_id != parsed.member_id) return false;
	return true;
}

bool assert_equivalence(_positional_member_access& expected, _positional_member_access& parsed)
{
	if (!assert_equivalent_expression(expected.offset_expression, parsed.offset_expression))
		return false;
	return true;
}

bool assert_equivalence(_type_specifier& expected, _type_specifier& parsed)
{
	if (expected.type != parsed.type) return false;
	if (expected.name != parsed.name) return false;
	if (!assert_equivalent_tspec_expression(expected.expr, parsed.expr)) return false;
	return true;
}

bool assert_equivalence(_var& expected, _var& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec, parsed.tspec)) return false;
	if (expected.postops.size() != parsed.postops.size())
		return false;
	else for (unsigned int i = 0; i < expected.postops.size(); ++i)
		if (!assert_equivalent_postop(expected.postops[i], parsed.postops[i])) return false;
	return true;
}

bool assert_equivalence(_vardecl& expected, _vardecl& parsed)
{
	if (!assert_equivalence(expected.lhs, parsed.lhs)) return false;
	if (expected.op != parsed.op) return false;
	if (!assert_equivalent_initializer(expected.rhs, parsed.rhs)) return false;
	return true;
}

bool assert_equivalence(_arg& expected, _arg& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec, parsed.tspec)) return false;
	return true;
}

bool assert_equivalence(_lambda& expected, _lambda& parsed)
{
	if (expected.argument_list.size() != parsed.argument_list.size())
		return false;
	else for (unsigned int i = 0; i < expected.argument_list.size(); i++)
		if (!assert_equivalence(expected.argument_list[i], parsed.argument_list[i])) 
			return false;

	if (expected.return_list.size() != parsed.return_list.size())
		return false;
	else for (unsigned int i = 0; i < expected.return_list.size(); i++)
		if (!assert_equivalence(expected.return_list[i], parsed.return_list[i]))
			return false;

	if (!assert_equivalence(expected.body, parsed.body)) 
		return false;

	return true;
}

bool assert_equivalence(_fndecl& expected, _fndecl& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.fn, parsed.fn)) return false;
	return true;
}

bool assert_equivalence(_member& expected, _member& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec, parsed.tspec)) return false;
	if (!assert_equivalent_expression(expected.initializer, parsed.initializer)) return false;
	return true;
}

bool assert_equivalence(_record& expected, _record& parsed)
{
	if (expected.id != parsed.id) return false;
	if (expected.members.size() != parsed.members.size()) return false;
	for (unsigned int i = 0; i < expected.members.size(); ++i) {
		if (!assert_equivalence(expected.members[i], parsed.members[i])) return false;
	}
	return true;
}

bool assert_equivalence(_tuple& expected, _tuple& parsed)
{
	if (expected.members.size() != parsed.members.size()) return false;
	for (unsigned int i = 0; i < expected.members.size(); ++i) {
		if (!assert_equivalence(expected.members[i], parsed.members[i])) return false;
	}
	return true;
}

bool assert_equivalence(_array& expected, _array& parsed)
{
	if (expected.id != parsed.id) return false;
	if (!assert_equivalence(expected.tspec, parsed.tspec)) return false;
	if (!assert_equivalent_expression(expected.length_expr, parsed.length_expr)) return false;
	return true;
}

bool assert_equivalence(_if& expected, _if& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_statement(expected.then, parsed.then)) return false;
	if (!assert_equivalent_statement(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_while& expected, _while& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_statement(expected.body, parsed.body)) return false;
	if (!assert_equivalent_statement(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_dowhile& expected, _dowhile& parsed)
{
	if (!assert_equivalent_expression(expected.cond, parsed.cond)) return false;
	if (!assert_equivalent_statement(expected.body, parsed.body)) return false;
	if (!assert_equivalent_statement(expected.els, parsed.els)) return false;
	return true;
}

bool assert_equivalence(_scope& expected, _scope& parsed)
{
	if (expected.variables.size() != parsed.variables.size())
		return false;
	else for (auto pair : expected.variables) {
		auto expected_decl = pair.second;
		auto parsed_decl = parsed.variables[pair.first];
		if (!assert_equivalence(expected_decl, parsed_decl))
			return false;
	}

	if (expected.statements.size() != parsed.statements.size()) 
		return false;
	else for (unsigned int i = 0; i < expected.statements.size(); ++i)
		if (!assert_equivalent_expression(expected.statements[i], parsed.statements[i])) 
			return false;

	return true;
}

bool assert_equivalence(_module& expected, _module& parsed)
{
	if (expected.id != parsed.id) return false;
	if (expected.main_fn != parsed.main_fn) return false;

	if (expected.import_list.size() != parsed.import_list.size())
		return false;
	else for (unsigned int i = 0; i < expected.import_list.size(); ++i)
			if (expected.import_list[i] != parsed.import_list[i]) return false;
		

	if (expected.export_list.size() != parsed.export_list.size())
		return false;
	else for (unsigned int i = 0; i < expected.export_list.size(); ++i)
			if (expected.export_list[i] != parsed.export_list[i]) return false;
		

	if (expected.types.size() != parsed.types.size())
		return false;
	else for (unsigned int i = 0; i < expected.types.size(); ++i)
			if (!assert_equivalent_type_definition(expected.types[i], parsed.types[i])) return false;
		
	if (!assert_equivalence(expected.body, parsed.body)) return false;
	return true;
}

_test_data test_module_declaration_empty()
{
	_test_data test_data = {"empty module declaration", false};
	_module expected;

	_module* parsed = parser.parse_module("module :: { }");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_module_declaration_full()
{
	_test_data test_data = { "full module declaration", false };
	_module expected;
	expected.import_list.push_back("hello");
	expected.import_list.push_back("world");

	expected.export_list.push_back("fly");
	expected.export_list.push_back("away");

	expected.main_fn = "fly";

	_module* parsed = parser.parse_module("module :: { import hello, world; export fly, away; begin fly; }");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_variable_declaration_without_initialization()
{
	_test_data test_data = { "variable declaration without initialization", false };
	_module expected;
	_var a, b, c, d;
	a.id = "a";
	a.tspec.type = _INT;
	
	b.id = "b";
	b.tspec.type = _FLOAT;

	c.id = "c";
	c.tspec.type = _TEXT;

	d.id = "d";
	d.tspec.type = _BOOL;

	_vardecl ad, bd, cd, dd;
	ad.lhs = a;
	ad.op = T_COLON;

	bd.lhs = b;
	bd.op = T_COLON;

	cd.lhs = c;
	cd.op = T_COLON;

	dd.lhs = d;
	dd.op = T_COLON;

	expected.body.variables["a"] = ad;
	expected.body.variables["b"] = bd;
	expected.body.variables["c"] = cd;
	expected.body.variables["d"] = dd;

	_module* parsed = parser.parse_module("a : int; b : float; c : text; d : bool;");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_variable_declaration_with_initialization()
{
	_test_data test_data = { "variable declaration with initialization", false };
	_module expected;
	_var a, b, c, d;
	a.id = "a";
	a.tspec.type = _INT;

	b.id = "b";
	b.tspec.type = _FLOAT;

	c.id = "c";
	c.tspec.type = _TEXT;

	d.id = "d";
	d.tspec.type = _BOOL;

	_vardecl ad, bd, cd, dd;
	ad.lhs = a;
	ad.op  = T_COLON_EQ;
	ad.rhs = new _int(420);

	bd.lhs = b;
	bd.op  = T_COLON_EQ;
	bd.rhs = new _float((float)6.9);

	cd.lhs = c;
	cd.op  = T_COLON_EQ;
	cd.rhs = new _text("Hello, World!");

	dd.lhs = d;
	dd.op  = T_COLON_EQ;
	dd.rhs = new _bool(true);

	expected.body.variables["a"] = ad;
	expected.body.variables["b"] = bd;
	expected.body.variables["c"] = cd;
	expected.body.variables["d"] = dd;

	_module* parsed = parser.parse_module("a : int = 420; b : float = 6.9; c : text = 'Hello, World!'; d : bool = true;");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_variable_declaration_from_initializer()
{
	_test_data test_data = { "variable declaration from initializer", false };
	_module expected;
	_var a, b, c, d;
	a.id = "a";

	b.id = "b";

	c.id = "c";

	d.id = "d";

	_vardecl ad, bd, cd, dd;
	ad.lhs = a;
	ad.op = T_COLON_EQ;
	ad.rhs = new _int(420);

	bd.lhs = b;
	bd.op = T_COLON_EQ;
	bd.rhs = new _float((float)6.9);

	cd.lhs = c;
	cd.op = T_COLON_EQ;
	cd.rhs = new _text("Hello, World!");

	dd.lhs = d;
	dd.op = T_COLON_EQ;
	dd.rhs = new _bool(true);

	expected.body.variables["a"] = ad;
	expected.body.variables["b"] = bd;
	expected.body.variables["c"] = cd;
	expected.body.variables["d"] = dd;

	_module* parsed = parser.parse_module("a := 420; b := 6.9; c := 'Hello, World!'; d := true;");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_constant_declaration()
{
	_test_data test_data = { "constant declaration", false };
	_module expected;
	_var a, b, c, d;
	a.id = "a";

	b.id = "b";

	c.id = "c";

	d.id = "d";

	_vardecl ad, bd, cd, dd;
	ad.lhs = a;
	ad.op = T_COLON_COLON;
	ad.rhs = new _int(420);

	bd.lhs = b;
	bd.op = T_COLON_COLON;
	bd.rhs = new _float((float)6.9);

	cd.lhs = c;
	cd.op = T_COLON_COLON;
	cd.rhs = new _text("Hello, World!");

	dd.lhs = d;
	dd.op = T_COLON_COLON;
	dd.rhs = new _bool(true);

	expected.body.variables["a"] = ad;
	expected.body.variables["b"] = bd;
	expected.body.variables["c"] = cd;
	expected.body.variables["d"] = dd;

	_module* parsed = parser.parse_module("a :: 420; b :: 6.9; c :: 'Hello, World!'; d :: true;");

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_lambda_declaration_without_initialization()
{
	_test_data test_data = { "lambda declaration without initialization", false };
	_module expected;
	_vardecl a, b, c;
	_lambda l1, l2, l3;
	_arg l1a1, l2a1, l2a2, l3a1, l3a2;
	_arg l1r1, l2r1, l3r1, l3r2;

	l1a1.id = "a";
	l1a1.tspec.type = _INT;
	l1r1.tspec.type = _INT;
	l1.argument_list.push_back(l1a1);
	l1.return_list.push_back(l1r1);
	a.lhs.id = "l1";
	a.lhs.tspec.type = _LAMBDA;
	a.lhs.tspec.expr = &l1;
	a.op = T_COLON;

	l2a1.id = "a";
	l2a1.tspec.type = _FLOAT;
	l2a2.id = "b";
	l2a2.tspec.type = _TEXT;
	l2r1.tspec.type = _BOOL;
	l2.argument_list.push_back(l2a1);
	l2.argument_list.push_back(l2a2);
	l2.return_list.push_back(l2r1);
	b.lhs.id = "l2";
	b.lhs.tspec.type = _LAMBDA;
	b.lhs.tspec.expr = &l2;
	b.op = T_COLON;

	l3a1.id = "a";
	l3a1.tspec.type = _BOOL;
	l3a2.id = "b";
	l3a2.tspec.type = _INT;
	l3r1.tspec.type = _FLOAT;
	l3r2.tspec.type = _INT;
	l3.argument_list.push_back(l3a1);
	l3.argument_list.push_back(l3a2);
	l3.return_list.push_back(l3r1);
	l3.return_list.push_back(l3r2);
	c.lhs.id = "l3";
	c.lhs.tspec.type = _LAMBDA;
	c.lhs.tspec.expr = &l3;
	c.op = T_COLON;

	expected.body.variables[a.lhs.id] = a;
	expected.body.variables[b.lhs.id] = b;
	expected.body.variables[c.lhs.id] = c;

	_module* parsed = parser.parse_module("l1 : (a : int) -> (int); l2 : (a : float, b : text) -> (bool); l3 : (a : bool, b : int) -> (float, int);");
	
	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_lambda_declaration_with_initialization()
{
	_test_data test_data = { "lambda declaration with initialization", false };
	_module expected;
	_vardecl a, b, c;
	_lambda l1, l2, l3;
	_lambda l1h, l2h, l3h;
	_arg l1a1, l2a1, l2a2, l3a1, l3a2;
	_arg l1r1, l2r1, l3r1, l3r2;
	_return l1r, l2r, l3r;
	_positional_member_access l2pma;
	_binop l1rexpr, l2rexpr, l3m1expr, l3m2expr;
	_member l3m1, l3m2;
	_tuple l3rexpr;

	// setup lambda 1's argument & return lists.
	l1a1.id = "a";
	l1a1.tspec.type = _INT;
	l1r1.tspec.type = _INT;
	l1.argument_list.push_back(l1a1);
	l1.return_list.push_back(l1r1);
	l1h.argument_list.push_back(l1a1);
	l1h.return_list.push_back(l1r1);
	// setup lambda 1's body
	l1rexpr.lhs = new _var;
	((_var*)l1rexpr.lhs)->id = "a";
	l1rexpr.rhs = new _var;
	((_var*)l1rexpr.rhs)->id = "a";
	l1rexpr.op = T_ADD;
	l1r.rhs = &l1rexpr;
	l1.body.statements.push_back(&l1r);
	// attach lambda 1 to the vardecl
	a.lhs.id = "l1";
	a.lhs.tspec.type = _LAMBDA;
	a.lhs.tspec.expr = &l1h;
	a.op = T_COLON_EQ;
	a.rhs = &l1;

	// setup lambda 2's type
	l2a1.id = "a";
	l2a1.tspec.type = _FLOAT;
	l2a2.id = "b";
	l2a2.tspec.type = _TEXT;
	l2r1.tspec.type = _BOOL;
	l2.argument_list.push_back(l2a1);
	l2.argument_list.push_back(l2a2);
	l2.return_list.push_back(l2r1);
	l2h.argument_list.push_back(l2a1);
	l2h.argument_list.push_back(l2a2);
	l2h.return_list.push_back(l2r1);
	// setup lambda 2's body
	// { return b[a] == a; }
	l2rexpr.lhs = new _var;
	((_var*)l2rexpr.lhs)->id = "b";
	l2pma.offset_expression = new _var;
	((_var*)l2pma.offset_expression)->id = "a";
	((_var*)l2rexpr.lhs)->postops.push_back(&l2pma);
	l2rexpr.rhs = new _var;
	((_var*)l2rexpr.rhs)->id = "a";
	l2rexpr.op = T_EQUALS;
	l2r.rhs = &l2rexpr;
	l2.body.statements.push_back(&l2r);
	// attach lambda 2 to the vardecl
	b.lhs.id = "l2";
	b.lhs.tspec.type = _LAMBDA;
	b.lhs.tspec.expr = &l2h;
	b.op = T_COLON_EQ;
	b.rhs = &l2;

	// setup lambda 3's type
	l3a1.id = "a";
	l3a1.tspec.type = _FLOAT;
	l3a2.id = "b";
	l3a2.tspec.type = _INT;
	l3r1.tspec.type = _FLOAT;
	l3r2.tspec.type = _INT;
	l3.argument_list.push_back(l3a1);
	l3.argument_list.push_back(l3a2);
	l3.return_list.push_back(l3r1);
	l3.return_list.push_back(l3r2);
	l3h.argument_list.push_back(l3a1);
	l3h.argument_list.push_back(l3a2);
	l3h.return_list.push_back(l3r1);
	l3h.return_list.push_back(l3r2);
	// setup lambda 3's body
	// return { a + b, a - b };
	l3m1expr.lhs = new _var;
	((_var*)l3m1expr.lhs)->id = "a";
	l3m1expr.rhs = new _var;
	((_var*)l3m1expr.rhs)->id = "b";
	l3m1expr.op = T_ADD;
	l3m2expr.lhs = new _var;
	((_var*)l3m2expr.lhs)->id = "a";
	l3m2expr.rhs = new _var;
	((_var*)l3m2expr.rhs)->id = "b";
	l3m2expr.op = T_SUB;
	l3m1.initializer = &l3m1expr;
	l3m1.tspec.type = _DEDUCE;
	l3m2.initializer = &l3m2expr;
	l3m2.tspec.type = _DEDUCE;
	l3rexpr.members.push_back(l3m1);
	l3rexpr.members.push_back(l3m2);
	l3r.rhs = &l3rexpr;
	l3.body.statements.push_back(&l3r);
	// attach lambda 3 to the vardecl
	c.lhs.id = "l3";
	c.lhs.tspec.type = _LAMBDA;
	c.lhs.tspec.expr = &l3h;
	c.op = T_COLON_EQ;
	c.rhs = &l3;

	expected.body.variables[a.lhs.id] = a;
	expected.body.variables[b.lhs.id] = b;
	expected.body.variables[c.lhs.id] = c;

	_module* parsed = parser.parse_module( \
			"l1 : (a : int) -> (int) = (a : int) -> (int) { return a + a; }; \
			 l2 : (a : float, b : text) -> (bool) = (a : float, b : text) -> (bool) { return b[a] == a; } ; \
			 l3 : (a : float, b : int) -> (float, int) = (a : float, b : int) -> (float, int) { return { a + b; a - b }; };"
			);

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_lambda_declaration_from_initializer()
{
	_test_data test_data = { "lambda declaration from initializer", false };
	_module expected;
	_vardecl a, b, c;
	_lambda l1, l2, l3;
	_arg l1a1, l2a1, l2a2, l3a1, l3a2;
	_arg l1r1, l2r1, l3r1, l3r2;
	_return l1r, l2r, l3r;
	_positional_member_access l2pma;
	_binop l1rexpr, l2rexpr, l3m1expr, l3m2expr;
	_member l3m1, l3m2;
	_tuple l3rexpr;

	// setup lambda 1's argument & return lists.
	l1a1.id = "a";
	l1a1.tspec.type = _INT;
	l1r1.tspec.type = _INT;
	l1.argument_list.push_back(l1a1);
	l1.return_list.push_back(l1r1);
	// setup lambda 1's body
	l1rexpr.lhs = new _var;
	((_var*)l1rexpr.lhs)->id = "a";
	l1rexpr.rhs = new _var;
	((_var*)l1rexpr.rhs)->id = "a";
	l1rexpr.op = T_ADD;
	l1r.rhs = &l1rexpr;
	l1.body.statements.push_back(&l1r);
	// attach lambda 1 to the vardecl
	a.lhs.id = "l1";
	a.lhs.tspec.type = _LAMBDA;
	a.op = T_COLON_EQ;
	a.rhs = &l1;

	// setup lambda 2's type
	l2a1.id = "a";
	l2a1.tspec.type = _FLOAT;
	l2a2.id = "b";
	l2a2.tspec.type = _TEXT;
	l2r1.tspec.type = _BOOL;
	l2.argument_list.push_back(l2a1);
	l2.argument_list.push_back(l2a2);
	l2.return_list.push_back(l2r1);
	// setup lambda 2's body
	// { return b[a] == a; }
	l2rexpr.lhs = new _var;
	((_var*)l2rexpr.lhs)->id = "b";
	l2pma.offset_expression = new _var;
	((_var*)l2pma.offset_expression)->id = "a";
	((_var*)l2rexpr.lhs)->postops.push_back(&l2pma);
	l2rexpr.rhs = new _var;
	((_var*)l2rexpr.rhs)->id = "a";
	l2rexpr.op = T_EQUALS;
	l2r.rhs = &l2rexpr;
	l2.body.statements.push_back(&l2r);
	// attach lambda 2 to the vardecl
	b.lhs.id = "l2";
	b.lhs.tspec.type = _LAMBDA;
	b.op = T_COLON_EQ;
	b.rhs = &l2;

	// setup lambda 3's type
	l3a1.id = "a";
	l3a1.tspec.type = _FLOAT;
	l3a2.id = "b";
	l3a2.tspec.type = _INT;
	l3r1.tspec.type = _FLOAT;
	l3r2.tspec.type = _INT;
	l3.argument_list.push_back(l3a1);
	l3.argument_list.push_back(l3a2);
	l3.return_list.push_back(l3r1);
	l3.return_list.push_back(l3r2);
	// setup lambda 3's body
	// return { a + b, a - b };
	l3m1expr.lhs = new _var;
	((_var*)l3m1expr.lhs)->id = "a";
	l3m1expr.rhs = new _var;
	((_var*)l3m1expr.rhs)->id = "b";
	l3m1expr.op = T_ADD;
	l3m2expr.lhs = new _var;
	((_var*)l3m2expr.lhs)->id = "a";
	l3m2expr.rhs = new _var;
	((_var*)l3m2expr.rhs)->id = "b";
	l3m2expr.op = T_SUB;
	l3m1.initializer = &l3m1expr;
	l3m1.tspec.type = _DEDUCE;
	l3m2.initializer = &l3m2expr;
	l3m2.tspec.type = _DEDUCE;
	l3rexpr.members.push_back(l3m1);
	l3rexpr.members.push_back(l3m2);
	l3r.rhs = &l3rexpr;
	l3.body.statements.push_back(&l3r);
	// attach lambda 3 to the vardecl
	c.lhs.id = "l3";
	c.lhs.tspec.type = _LAMBDA;
	c.op = T_COLON_EQ;
	c.rhs = &l3;

	expected.body.variables[a.lhs.id] = a;
	expected.body.variables[b.lhs.id] = b;
	expected.body.variables[c.lhs.id] = c;

	_module* parsed = parser.parse_module( \
			"l1 := (a : int) -> (int) { return a + a; }; \
			 l2 := (a : float, b : text) -> (bool) { return b[a] == a; } ; \
			 l3 := (a : float, b : int) -> (float, int) { return { a + b; a - b }; };"
	);
	
	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

_test_data test_constant_lambda()
{
	_test_data test_data = { "constant lambda", false };
	_module expected;
	_vardecl a, b, c;
	_lambda l1, l2, l3;
	_arg l1a1, l2a1, l2a2, l3a1, l3a2;
	_arg l1r1, l2r1, l3r1, l3r2;
	_return l1r, l2r, l3r;
	_positional_member_access l2pma;
	_binop l1rexpr, l2rexpr, l3m1expr, l3m2expr;
	_member l3m1, l3m2;
	_tuple l3rexpr;

	// setup lambda 1's argument & return lists.
	l1a1.id = "a";
	l1a1.tspec.type = _INT;
	l1r1.tspec.type = _INT;
	l1.argument_list.push_back(l1a1);
	l1.return_list.push_back(l1r1);
	// setup lambda 1's body
	l1rexpr.lhs = new _var;
	((_var*)l1rexpr.lhs)->id = "a";
	l1rexpr.rhs = new _var;
	((_var*)l1rexpr.rhs)->id = "a";
	l1rexpr.op = T_ADD;
	l1r.rhs = &l1rexpr;
	l1.body.statements.push_back(&l1r);
	// attach lambda 1 to the vardecl
	a.lhs.id = "l1";
	a.lhs.tspec.type = _LAMBDA;
	a.op = T_COLON_COLON;
	a.rhs = &l1;

	// setup lambda 2's type
	l2a1.id = "a";
	l2a1.tspec.type = _FLOAT;
	l2a2.id = "b";
	l2a2.tspec.type = _TEXT;
	l2r1.tspec.type = _BOOL;
	l2.argument_list.push_back(l2a1);
	l2.argument_list.push_back(l2a2);
	l2.return_list.push_back(l2r1);
	// setup lambda 2's body
	// { return b[a] == a; }
	l2rexpr.lhs = new _var;
	((_var*)l2rexpr.lhs)->id = "b";
	l2pma.offset_expression = new _var;
	((_var*)l2pma.offset_expression)->id = "a";
	((_var*)l2rexpr.lhs)->postops.push_back(&l2pma);
	l2rexpr.rhs = new _var;
	((_var*)l2rexpr.rhs)->id = "a";
	l2rexpr.op = T_EQUALS;
	l2r.rhs = &l2rexpr;
	l2.body.statements.push_back(&l2r);
	// attach lambda 2 to the vardecl
	b.lhs.id = "l2";
	b.lhs.tspec.type = _LAMBDA;
	b.op = T_COLON_COLON;
	b.rhs = &l2;

	// setup lambda 3's type
	l3a1.id = "a";
	l3a1.tspec.type = _FLOAT;
	l3a2.id = "b";
	l3a2.tspec.type = _INT;
	l3r1.tspec.type = _FLOAT;
	l3r2.tspec.type = _INT;
	l3.argument_list.push_back(l3a1);
	l3.argument_list.push_back(l3a2);
	l3.return_list.push_back(l3r1);
	l3.return_list.push_back(l3r2);
	// setup lambda 3's body
	// return { a + b, a - b };
	l3m1expr.lhs = new _var;
	((_var*)l3m1expr.lhs)->id = "a";
	l3m1expr.rhs = new _var;
	((_var*)l3m1expr.rhs)->id = "b";
	l3m1expr.op = T_ADD;
	l3m2expr.lhs = new _var;
	((_var*)l3m2expr.lhs)->id = "a";
	l3m2expr.rhs = new _var;
	((_var*)l3m2expr.rhs)->id = "b";
	l3m2expr.op = T_SUB;
	l3m1.initializer = &l3m1expr;
	l3m1.tspec.type = _DEDUCE;
	l3m2.initializer = &l3m2expr;
	l3m2.tspec.type = _DEDUCE;
	l3rexpr.members.push_back(l3m1);
	l3rexpr.members.push_back(l3m2);
	l3r.rhs = &l3rexpr;
	l3.body.statements.push_back(&l3r);
	// attach lambda 3 to the vardecl
	c.lhs.id = "l3";
	c.lhs.tspec.type = _LAMBDA;
	c.op = T_COLON_COLON;
	c.rhs = &l3;

	expected.body.variables[a.lhs.id] = a;
	expected.body.variables[b.lhs.id] = b;
	expected.body.variables[c.lhs.id] = c;

	_module* parsed = parser.parse_module(\
		"l1 :: (a : int) -> (int) { return a + a; }; \
			 l2 :: (a : float, b : text) -> (bool) { return b[a] == a; } ; \
			 l3 :: (a : float, b : int) -> (float, int) { return { a + b; a - b }; };"
	);

	test_data.passed = assert_equivalence(expected, *parsed);
	return test_data;
}

void test_lexer(_test_results& td)
{
}

void test_parser(_test_results& td)
{
	td.tests.push_back(test_module_declaration_empty());
	td.tests.push_back(test_module_declaration_full());
	td.tests.push_back(test_variable_declaration_without_initialization());
	td.tests.push_back(test_variable_declaration_with_initialization());
	td.tests.push_back(test_variable_declaration_from_initializer());
	td.tests.push_back(test_constant_declaration());
	td.tests.push_back(test_lambda_declaration_without_initialization());
	td.tests.push_back(test_lambda_declaration_with_initialization());
	td.tests.push_back(test_lambda_declaration_from_initializer());
	td.tests.push_back(test_constant_lambda());
}
