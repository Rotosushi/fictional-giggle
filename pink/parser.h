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
	_ast* parse_module_level_declaration();
	_ast* parse_declaration();
	_ast* parse_function_definition();

	void parse_type_specifier(_vardecl& decl);
	void parse_lambda_header(_lambda& fn);
	void parse_lambda_body()
};