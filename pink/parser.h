#pragma once

#include <stack>
using std::stack;
#include <string>
using std::string;
#include <map>
using std::map;

#include "token.h"
#include "type.h"
#include "ast.h"
#include "lexer.h"


class _parser {
	_module* parse_module();

	_parser();
private:
	_lexer lexer;
	map<_token, int> ptable;
	_token curtok;

	/*
	stack<int> marks;
	vector<_token> tokbuf;
	int tokidx;
	stack<int> parens;
	*/
	/* support functions+ */
	/* general */
	void init_precedence_table();

	void nexttok();

	string gettext();

	/*
	void print_token_buffer();
	void print_token_buffer(int i);
	void parser_error(const char* str);
	*/

	/* speculation support */
	/*
	int  mark();
	void release();
	*/
	/* parsing primitives */
	/*
	_token curtok();
	bool  speculate(_token t);
	void  consume();
	void  sync(int i);
	bool  speculating();
	*/

	bool is_unop(_token t);
	bool is_binop(_token t);
	bool is_literal(_token t);
	bool is_postop(_token t);

	/* parsing functions */
	void parse_module_level_declaration(_module& mdl);

	void parse_declaration(_vardecl& decl);

	void parse_type_specifier(_vardecl& decl);
	void parse_type_specifier(_arg& arg);

	void parse_initializer(_vardecl& decl);

	void parse_function_definition(_fndecl& fn);

	void parse_lambda(_lambda& fn);
	void parse_lambda_header(_lambda& fn);
	void parse_lambda_body(_lambda& fn);
	void parse_argument_list(vector<_arg>& args);
	void parse_return_list(vector<_arg>& args);

	void parse_expression(_ast* expr);
	_ast* _parse_expression(_ast* lhs, int min_prec);
	_ast* _parse_postop();
	_ast* _parse_primary_expr();

	_lambda* _parse_function_call();

	void parse_conditional(_if& conditional);
	void parse_iteration(_while& loop);

	void parse_statement(_ast* expr);

	void parse_block(_scope& body);
};