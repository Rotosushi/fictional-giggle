#pragma once

#include <stack>
using std::stack;
#include <string>
using std::string;
#include <map>
using std::map;
#include <iostream>
using std::ifstream;

#include "token.h"
#include "type.h"
#include "ast.h"
#include "lexer.h"


class _parser {
public:
	_module* parse_module();
	_module* parse_module(string input);
	_module* parse_module(ifstream& input);

	_parser() {
		init_precedence_table();
		reset_internal_state();
	}
private:
	_lexer lexer;
	map<_token, int> ptable;
	
	stack<int> parens;

	vector<_token> tokbuf;
	vector<string> texbuf;
	int tokidx;

	stack<int> marks;

	/* support functions */
	/* general */
	void init_precedence_table();
	void reset_internal_state();

	/* token handling */
	_token curtok();
	void nexttok();
	string curtext();

	/* speculation support */
	int mark();
	void release();

	bool speculate(_token t);
	bool speculating();
	void sync(int i);
	
	/* set membership functions */

	bool is_unop(_token t);
	bool is_binop(_token t);
	bool is_literal(_token t);
	bool is_postop(_token t);

	bool is_module_keyword(_token t);

	/* parsing functions */
	/* Module parsing */
	void parse_module_declaration(_module& mdl);

	void parse_context_statement(_module& mdl);

	/* Variable parsing */
	void parse_variable_declaration(_vardecl& decl);
	void parse_initializer(_vardecl& decl);

	void parse_type(_vardecl& decl);
	void parse_type(_arg& arg);

	/* function parsing */
	void parse_function_declaration(_fn& fn);
	void parse_function_type(_fn& fn);
	void parse_function_body(_fn& fn);
	void parse_argument_list(vector<_arg>& args);
	void parse_arg(_arg& arg);
	void parse_return_list(vector<_arg>& rargs);

	/* expression parsing */
	void parse_expression(_expr& expr);
	_ast* _parse_expression(_ast* lhs, int min_prec);
	_ast* _parse_postop();
	_ast* _parse_primary_expr();

	_fcall* _parse_function_call();
	void parse_carg(_arg& carg);

	/* statement parsing */
	void parse_conditional(_if& conditional);
	void parse_iteration(_while& loop);

	_ast* parse_statement();

	void parse_scope(_scope& body);

	bool speculate_declaration();
	bool speculate_type();
	bool speculate_initializer();

	bool speculate_expression();
	bool speculate_id_expr();
	bool speculate_literal_expr();
	bool speculate_binop_expr();
	bool speculate_unop_expr();
	bool speculate_lparen_expr();
	bool speculate_rparen_expr();
	bool speculate_postop_expr();
	bool speculate_fcall();
	bool speculate_carg();
};