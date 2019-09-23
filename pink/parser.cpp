#include <iostream>
using std::cout;
using std::endl;
#include "parser.h"

_module* _parser::parse_module()
{
	auto top = new _module;

	nexttok(); // prime our input.

	while (curtok != T_EOF) {
		auto decl = parse_module_level_declaration();
		if (decl != nullptr) {
			switch (decl->ast_type) {
			case AST_VARDECL:
				auto dec = ((_vardecl*)decl);
				try {
					auto result = top->body.resolve(dec->lhs.id);
					cout << "error: variable { " << dec->lhs.id << " } already defined." << endl;
					exit(0);
				}
				catch (...) {
					top->body.define(*dec);
				}
				break;
			case AST_FNDECL:
				auto dec = ((_fndecl*)decl);
				auto result = top->resolve_type(dec->lhs.id);
				if (result == nullptr) {
					top->define_type(dec);
				}
				else {
					cout << "error: function { " << dec->lhs.id << " } already defined." << endl;
					exit(0);
				}
				break;
			}
		}
	}
}

void _parser::init_precedence_table()
{
}

void _parser::nexttok()
{
	curtok = lexer.gettok();
}

string _parser::gettext()
{
	return lexer.gettext();
}

bool _parser::is_unop(_token t)
{
	return false;
}

bool _parser::is_binop(_token t)
{
	return false;
}

bool _parser::is_literal(_token t)
{
	return false;
}

bool _parser::is_postop(_token t)
{
	return false;
}

_ast* _parser::parse_module_level_declaration()
{
	switch (curtok) {
	case T_ID:
		return parse_declaration();
		break;
	case T_FN:
		return parse_function_definition();
		break;
	default: return nullptr;

	}
}

_ast* _parser::parse_declaration()
{
	/*
<declaration>  := <identifier> ':' <type-specifier> ';'
				| <identifier> ':' <type-specifier> '=' <initializer> ';'
				| <identifier> '::' <initializer> ';'
				| <identifier> ':=' <initializer> ';'

	*/
	auto decl = new _vardecl;
	
	if (curtok != T_ID) throw;
	decl->lhs.id = gettext();
	nexttok();

	switch (curtok) {
	case T_COLON:
		nexttok();
		decl->rhs = parse_type_specifier();
		switch (decl->rhs->ast_type) {
		case AST_VAR:

		case AST_INT:

		case AST_FLOAT:

		case AST_TEXT:

		case AST_BOOL:

		}
		break;
	case T_COLON_EQ:

		break;
	case T_COLON_COLON:

		break;
	default: throw;
	}
}

_ast* _parser::parse_function_definition()
{
	return nullptr;
}

void _parser::parse_type_specifier(_vardecl& decl)
{
	_ast* ts = nullptr;
	switch (curtok) {
	case T_ID:
		decl.lhs.type = _DEF;
		break;
	case T_INT:
		decl.lhs.type = _INT;
		decl.rhs = new _int;
		break;
	case T_FLOAT:
		decl.lhs.type = _FLOAT;
		decl.rhs = new _float;
		break;
	case T_TEXT:
		decl.lhs.type = _TEXT;
		decl.rhs = new _text;
		break;
	case T_BOOL:
		decl.lhs.type = _BOOL;
		decl.rhs = new _bool;
		break;
	case T_LPAREN:
		decl.lhs.type = _LAMBDA;
		decl.rhs = new _lambda;
		parse_lambda_header(*decl.rhs);
	default: 
		cout << "error: type specified unknown { " << curtok << " }" << endl;
		throw;
	}
}
